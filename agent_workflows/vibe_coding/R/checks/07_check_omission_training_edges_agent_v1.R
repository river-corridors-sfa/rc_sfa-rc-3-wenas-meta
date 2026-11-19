# Verify sparse subset folds and fresh full-set reproduction without running bootstraps.
library(tidyverse)
library(here)
library(glmnet)
root <- here("agent_workflows","vibe_coding")
script <- file.path(root,"R/sensitivities/02_run_predictor_omission_agent_v1.R")
for(expression in parse(script)) {
  if(is.call(expression) && identical(expression[[1]],as.name("<-")) &&
     is.symbol(expression[[2]]) && as.character(expression[[2]]) %in% c("prepare","weights_for","fit_nested")) {
    eval(expression)
  }
}
weight_method <- "equal_study"
transformations <- c(x="none",constant="none")
fixture <- tibble(Study_ID=rep(c("a","b","c"),each=4),
  candidate_pair_id=Study_ID,lnRR_mean=seq_len(12)/10,x=seq_len(12),constant=1)
one <- fit_nested(fixture,c("x","constant"))
prepared <- prepare(fixture,fixture,c("x","constant"))
stopifnot(identical(prepared$predictors,"x"),all(is.finite(predict(one,
  newx=cbind(as.matrix(prepared$test["x"]),zero_padding=0),s="lambda.1se"))))
zero <- fit_nested(mutate(fixture,x=1),c("x","constant"))
stopifnot(isTRUE(zero$intercept_only),abs(zero$intercept-mean(fixture$lnRR_mean))<1e-12)
sparse <- fixture %>% mutate(x=if_else(Study_ID=="a",x,NA_integer_))
mixed <- fit_nested(sparse,c("x","constant"))
stopifnot(is.finite(mixed$lambda.1se))

dictionary <- read_csv(file.path(root,"config/predictor_dictionary.csv"),show_col_types=FALSE)
transformations <- setNames(dictionary$transformation,dictionary$predictor)
data <- read_csv(file.path(root,"data/derived/lasso_model_table.csv"),show_col_types=FALSE) %>%
  mutate(row_id=row_number())
config <- read_csv(file.path(root,"config/issue6_revised_primary_predictors.csv"),show_col_types=FALSE)
reference <- read_csv(file.path(root,"output/paper/tables/heldout_predictions.csv"),show_col_types=FALSE) %>%
  filter(model=="lasso")
for(a in c("DOC","NO3")) {
  rows <- filter(data,response_var==a)
  candidates <- config %>% filter(response_var==a) %>% arrange(position) %>% pull(predictor)
  for(study in unique(rows$Study_ID)) {
    train <- filter(rows,Study_ID!=study)
    test <- filter(rows,Study_ID==study)
    fit <- fit_nested(train,candidates)
    prep <- prepare(train,test,candidates)
    prediction <- as.numeric(predict(fit,newx=as.matrix(prep$test[prep$predictors]),s="lambda.1se"))
    expected <- reference$predicted[match(test$row_id,reference$row_id)]
    stopifnot(max(abs(prediction-expected))<1e-10)
  }
}
message("PASS: one/zero usable-predictor folds; sparse inner folds; fresh full-set fits reproduce every primary LOSO prediction.")
