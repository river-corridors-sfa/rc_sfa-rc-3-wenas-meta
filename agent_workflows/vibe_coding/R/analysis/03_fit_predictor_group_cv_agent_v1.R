# Figure 3: time and predictor-group improvement over a common intercept model.
# Preprocessing and balance weights adapted from the primary/omission scripts.
# Refit only nested publication-level CV; does not modify bootstrap results.
library(tidyverse)
library(here)
library(glmnet)
root <- here("agent_workflows","vibe_coding")
out <- file.path(root,"output/paper/predictor_group_cv")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
data <- read_csv(file.path(root,"data/derived/lasso_model_table.csv"),show_col_types=FALSE) %>%
  mutate(row_id=row_number())
dictionary <- read_csv(file.path(root,"config/predictor_dictionary.csv"),show_col_types=FALSE)
config <- read_csv(file.path(root,"config/issue6_revised_primary_predictors.csv"),show_col_types=FALSE)
transformations <- setNames(dictionary$transformation,dictionary$predictor)
weight_method <- "equal_study"
lambdas <- 10^seq(2,-4,length.out=80)
stopifnot(all(is.finite(data$lnRR_mean)),all(data$effect_size_definition=="log_ratio_annual_arithmetic_means"),
  !anyDuplicated(data[c("candidate_pair_id","response_var","year")]))

prepare <- function(train, test, candidates) {
  for (p in candidates) {
    if (!is.numeric(train[[p]])) {
      train[[p]] <- parse_number(as.character(train[[p]]))
      test[[p]] <- parse_number(as.character(test[[p]]))
    }
    if (identical(transformations[[p]], "log1p")) {
      train[[p]] <- log1p(pmax(train[[p]], 0))
      test[[p]] <- log1p(pmax(test[[p]], 0))
    }
  }
  usable <- candidates[vapply(train[candidates], function(x)
    sum(is.finite(x)) >= 3 && is.finite(sd(x, na.rm = TRUE)) && sd(x, na.rm = TRUE) > 0,
    logical(1))]
  for (p in usable) {
    fill <- median(train[[p]], na.rm = TRUE)
    train[[p]][!is.finite(train[[p]])] <- fill
    test[[p]][!is.finite(test[[p]])] <- fill
    center <- mean(train[[p]])
    scale <- sd(train[[p]])
    train[[p]] <- (train[[p]] - center) / scale
    test[[p]] <- (test[[p]] - center) / scale
  }
  list(train = train, test = test, predictors = usable)
}

weights_for <- function(rows) {
  if (weight_method == "equal_row") return(rep(1,nrow(rows)))
  if (weight_method == "equal_study") {
    # Bootstrap multiplicity is preserved: each draw receives equal total mass.
    cluster <- if ("bootstrap_cluster" %in% names(rows)) rows$bootstrap_cluster else rows$Study_ID
    pair <- interaction(cluster,rows$candidate_pair_id,drop=TRUE)
    n_years <- as.numeric(table(pair)[as.character(pair)])
    n_pairs <- tapply(rows$candidate_pair_id,cluster,function(x) length(unique(x)))
    w <- 1 / (n_years * as.numeric(n_pairs[as.character(cluster)]))
    return(w/mean(w))
  }
  precision <- ifelse(is.finite(rows$lnRR_var) & rows$lnRR_var > 0,
                      1 / rows$lnRR_var, NA_real_)
  if (all(is.na(precision))) precision[] <- 1
  precision[is.na(precision)] <- median(precision, na.rm = TRUE)
  precision <- pmin(precision, as.numeric(quantile(precision, .95)))
  w <- precision * rows$reference_family_weight
  w / mean(w)
}

fit_group_path <- function(train,test,candidates,penalties) {
  prep <- prepare(train,test,candidates)
  weights <- weights_for(prep$train)
  group_predictors <- setdiff(prep$predictors,"post_fire_year")
  if(!length(group_predictors)) {
    if("post_fire_year" %in% prep$predictors) {
      fit <- lm(lnRR_mean~post_fire_year,data=prep$train,weights=weights)
      prediction <- as.numeric(predict(fit,newdata=prep$test))
      beta <- coef(fit)
    } else {
      prediction <- rep(weighted.mean(prep$train$lnRR_mean,weights),nrow(test))
      beta <- c("(Intercept)"=prediction[1])
    }
    return(list(prediction=matrix(prediction,nrow=nrow(test),ncol=length(penalties)),
      coefficient=matrix(beta,nrow=length(beta),ncol=length(penalties),
        dimnames=list(names(beta),NULL))))
  }
  x_train <- as.matrix(prep$train[prep$predictors])
  x_test <- as.matrix(prep$test[prep$predictors])
  if(ncol(x_train)==1) {
    x_train <- cbind(x_train,zero_padding=0)
    x_test <- cbind(x_test,zero_padding=0)
  }
  factors <- ifelse(colnames(x_train)=="post_fire_year",0,1)
  fit <- glmnet(x_train,prep$train$lnRR_mean,weights=weights,alpha=1,
    lambda=penalties,penalty.factor=factors,standardize=FALSE,control=list(maxit=1000000))
  list(prediction=as.matrix(predict(fit,newx=x_test,s=penalties)),
       coefficient=as.matrix(coef(fit,s=penalties)))
}

predictions <- list(); coefficients <- list(); tuning <- list(); folds <- list(); groups <- list()
for(a in c("DOC","NO3")) {
  rows <- filter(data,response_var==a)
  candidates <- config %>% filter(response_var==a) %>% arrange(position) %>% pull(predictor)
  models <- list(time_only="post_fire_year",intercept_only=character(),
    time_watershed=c("post_fire_year",setdiff(candidates,c("post_fire_year","burn_percent_fire_year","burn_sev_high"))),
    time_wildfire=c("post_fire_year","burn_percent_fire_year","burn_sev_high"),
    time_full=candidates)
  for(model in names(models)) groups[[length(groups)+1]] <- tibble(response_var=a,model=model,
    predictor=models[[model]],penalty_factor=if_else(predictor=="post_fire_year",0,1))
  for(held_out in sort(unique(rows$Study_ID))) {
    train <- filter(rows,Study_ID!=held_out)
    test <- filter(rows,Study_ID==held_out)
    stopifnot(!any(train$Study_ID %in% test$Study_ID))
    training_studies <- sort(unique(train$Study_ID))
    fold_map <- setNames(rep(seq_len(min(5,length(training_studies))),length.out=length(training_studies)),training_studies)
    folds[[length(folds)+1]] <- tibble(response_var=a,held_out_study=held_out,
      Study_ID=training_studies,inner_fold=unname(fold_map))
    for(model in names(models)) {
      selected <- 1L; best <- 1L
      if(!model %in% c("time_only","intercept_only")) {
        errors <- matrix(NA_real_,length(training_studies),length(lambdas))
        for(f in unique(fold_map)) {
          inner_train <- train[fold_map[train$Study_ID]!=f,,drop=FALSE]
          validation <- train[fold_map[train$Study_ID]==f,,drop=FALSE]
          stopifnot(!any(inner_train$Study_ID %in% validation$Study_ID))
          path <- fit_group_path(inner_train,validation,models[[model]],lambdas)
          for(study in unique(validation$Study_ID)) {
            ix <- which(validation$Study_ID==study)
            evaluation_weights <- weights_for(validation[ix,,drop=FALSE])
            errors[match(study,training_studies),] <- colSums(
              (path$prediction[ix,,drop=FALSE]-validation$lnRR_mean[ix])^2*evaluation_weights)/sum(evaluation_weights)
          }
        }
        mean_error <- colMeans(errors)
        se <- apply(errors,2,sd)/sqrt(length(training_studies))
        best <- which.min(mean_error)
        selected <- which(mean_error<=mean_error[best]+se[best])[1]
      }
      path <- fit_group_path(train,test,models[[model]],lambdas)
      predicted <- path$prediction[,selected]
      stopifnot(all(is.finite(predicted)))
      predictions[[length(predictions)+1]] <- test %>%
        select(row_id,Study_ID,candidate_pair_id,response_var,year) %>%
        mutate(held_out_study=held_out,model=model,observed=test$lnRR_mean,predicted=predicted)
      beta <- path$coefficient[,selected]
      coefficients[[length(coefficients)+1]] <- tibble(response_var=a,held_out_study=held_out,
        model=model,predictor=rownames(path$coefficient),coefficient=as.numeric(beta)) %>%
        filter(predictor!="zero_padding")
      tuning[[length(tuning)+1]] <- tibble(response_var=a,held_out_study=held_out,model=model,
        lambda_1se=if(model %in% c("time_only","intercept_only")) NA_real_ else lambdas[selected],
        lambda_min=if(model %in% c("time_only","intercept_only")) NA_real_ else lambdas[best],
        best_at_lower_boundary=if(model %in% c("time_only","intercept_only")) FALSE else best==length(lambdas))
    }
  }
}
predictions <- bind_rows(predictions)
stopifnot(nrow(predictions)==5*nrow(data),
  !anyDuplicated(predictions[c("response_var","row_id","model")]))
reference <- read_csv(file.path(root,"output/paper/tables/heldout_predictions.csv"),show_col_types=FALSE) %>%
  filter(model %in% c("time_only","intercept_only")) %>% select(response_var,row_id,model,reference=predicted)
baseline_check <- predictions %>% filter(model %in% c("time_only","intercept_only")) %>%
  left_join(reference,by=c("response_var","row_id","model"))
stopifnot(!anyNA(baseline_check$reference),max(abs(baseline_check$predicted-baseline_check$reference))<1e-10)
study_errors <- predictions %>% group_by(response_var,model,held_out_study) %>%
  summarise(n_records=n(),MSE=mean((observed-predicted)^2),MAE=mean(abs(observed-predicted)),.groups="drop")
performance <- study_errors %>% group_by(response_var,model) %>%
  summarise(publications=n(),records=sum(n_records),RMSE=sqrt(mean(MSE)),MAE=mean(MAE),.groups="drop")
improvement <- performance %>%
  left_join(performance %>% filter(model=="intercept_only") %>%
    select(response_var,baseline_RMSE=RMSE,baseline_MAE=MAE),by="response_var") %>%
  mutate(RMSE_improvement=baseline_RMSE-RMSE,
    relative_RMSE_improvement=RMSE_improvement/baseline_RMSE,
    MAE_improvement=baseline_MAE-MAE,
    definition="positive=lower_error_than_intercept_only")
write_csv(predictions,file.path(out,"heldout_predictions.csv"))
write_csv(bind_rows(coefficients),file.path(out,"outer_coefficients.csv"))
write_csv(bind_rows(tuning),file.path(out,"tuning.csv"))
write_csv(bind_rows(folds),file.path(out,"inner_fold_assignments.csv"))
write_csv(bind_rows(groups),file.path(out,"predictor_groups.csv"))
write_csv(study_errors,file.path(out,"performance_by_study.csv"))
write_csv(performance,file.path(out,"performance.csv"))
write_csv(improvement,file.path(out,"group_improvement.csv"))
input_paths <- c("data/derived/lasso_model_table.csv","config/issue6_revised_primary_predictors.csv",
  "config/predictor_dictionary.csv","R/analysis/03_fit_predictor_group_cv_agent_v1.R")
write_csv(tibble(path=input_paths,md5=unname(tools::md5sum(file.path(root,input_paths)))),file.path(out,"input_manifest.csv"))
write_csv(tibble(alpha=1,time_penalty_factor=0,group_penalty_factor=1,penalty_rule="one_SE",
  penalty_min=.0001,penalty_max=100,penalty_count=80,weighting="equal_study_pair_year",
  validation="nested_publication_LOSO",baseline="weighted_intercept_only"),file.path(out,"run_settings.csv"))
capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
print(improvement,width=Inf)
message("Predictor-group CV complete; time-only predictions reproduce the saved primary benchmark.")
