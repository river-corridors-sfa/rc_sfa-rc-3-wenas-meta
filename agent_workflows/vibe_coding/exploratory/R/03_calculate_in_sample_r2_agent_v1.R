# Full-data apparent fit using existing tuning and fitting functions.
# Load only function definitions, avoiding the original scripts' output writes/refits.
library(tidyverse)
library(glmnet)
root <- file.path("agent_workflows", "vibe_coding")
data <- read_csv(file.path(root,"data/derived/lasso_model_table.csv"),show_col_types=FALSE)
dictionary <- read_csv(file.path(root,"config/predictor_dictionary.csv"),show_col_types=FALSE)
config <- read_csv(file.path(root,"config/issue6_revised_primary_predictors.csv"),show_col_types=FALSE)
transformations <- setNames(dictionary$transformation,dictionary$predictor)
weight_method <- "equal_study"
lambdas <- 10^seq(2,-4,length.out=80)
for (script in c("01_fit_issue6_revised_primary_agent_v1.R", "03_fit_predictor_group_cv_agent_v1.R")) {
  expressions <- parse(file.path(root,"R/analysis",script))
  for (expr in expressions) {
    if (is.call(expr) && identical(expr[[1]],as.name("<-")) &&
        is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name("function"))) eval(expr)
  }
}
results <- list()
predictions <- list()
coefficients <- list()
for (a in c("DOC","NO3")) {
  rows <- filter(data,response_var==a)
  candidates <- config %>% filter(response_var==a) %>% arrange(position) %>% pull(predictor)
  prep <- prepare(rows,rows,candidates)
  fit <- fit_nested(rows,candidates)
  all_penalized_prediction <- as.numeric(predict(fit,newx=as.matrix(prep$test[prep$predictors]),s="lambda.1se"))
  studies <- sort(unique(rows$Study_ID))
  fold_map <- setNames(rep(seq_len(min(5,length(studies))),length.out=length(studies)),studies)
  errors <- matrix(NA_real_,length(studies),length(lambdas))
  for (f in unique(fold_map)) {
    train <- rows[fold_map[rows$Study_ID]!=f,,drop=FALSE]
    validation <- rows[fold_map[rows$Study_ID]==f,,drop=FALSE]
    path <- fit_group_path(train,validation,candidates,lambdas)
    for (study in unique(validation$Study_ID)) {
      ix <- which(validation$Study_ID==study)
      ew <- weights_for(validation[ix,,drop=FALSE])
      errors[match(study,studies),] <- colSums((path$prediction[ix,,drop=FALSE]-validation$lnRR_mean[ix])^2*ew)/sum(ew)
    }
  }
  mean_error <- colMeans(errors)
  se <- apply(errors,2,sd)/sqrt(length(studies))
  best <- which.min(mean_error)
  selected <- which(mean_error<=mean_error[best]+se[best])[1]
  path <- fit_group_path(rows,rows,candidates,lambdas)
  for (model in c("all_penalized_full","time_unpenalized_full")) {
    prediction <- if(model=="all_penalized_full") all_penalized_prediction else path$prediction[,selected]
    lambda <- if(model=="all_penalized_full") fit$lambda.1se else lambdas[selected]
    beta <- if(model=="all_penalized_full") as.matrix(coef(fit,s="lambda.1se"))[,1] else path$coefficient[,selected]
    y <- rows$lnRR_mean
    w <- weights_for(rows)
    results[[length(results)+1]] <- tibble(response_var=a,model=model,n=length(y),publications=length(studies),
      lambda_1se=lambda,in_sample_R2=1-sum((y-prediction)^2)/sum((y-mean(y))^2),
      weighted_in_sample_R2=1-sum(w*(y-prediction)^2)/sum(w*(y-weighted.mean(y,w))^2))
    predictions[[length(predictions)+1]] <- rows %>% select(response_var,Study_ID,candidate_pair_id,year) %>%
      mutate(model=model,observed=y,predicted=prediction,fitting_weight=w)
    coefficients[[length(coefficients)+1]] <- tibble(response_var=a,model=model,predictor=names(beta),coefficient=as.numeric(beta))
  }
}
out <- file.path(root,"exploratory/output/in_sample_r2")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
write_csv(bind_rows(results),file.path(out,"performance.csv"))
write_csv(bind_rows(predictions),file.path(out,"predictions.csv"))
write_csv(bind_rows(coefficients),file.path(out,"coefficients.csv"))
capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
print(bind_rows(results),width=Inf)
