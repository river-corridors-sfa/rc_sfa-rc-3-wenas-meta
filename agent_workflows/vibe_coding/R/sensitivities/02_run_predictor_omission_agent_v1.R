# Focused predictor-omission sensitivity for the manuscript LASSO models.
# Preprocessing, weights and tuning adapted from
# R/analysis/01_fit_issue6_revised_primary_agent_v1.R (October 27, 2026).
# Run from repository root; N_BOOTSTRAP defaults to 1000 attempts/analyte.
library(tidyverse)
library(here)
library(glmnet)
set.seed(20260929)
root <- here("agent_workflows", "vibe_coding")
out <- file.path(root, "output/paper/predictor_omission")
dir.create(out, recursive=TRUE, showWarnings=FALSE)
data <- read_csv(file.path(root,"data/derived/lasso_model_table.csv"),show_col_types=FALSE) %>%
  mutate(row_id=row_number())
stopifnot(all(is.finite(data$lnRR_mean)), !anyDuplicated(data[c("candidate_pair_id","response_var","year")]),
          all(data$effect_size_definition=="log_ratio_annual_arithmetic_means"))
config <- read_csv(file.path(root,"config/issue6_revised_primary_predictors.csv"),show_col_types=FALSE)
dictionary <- read_csv(file.path(root,"config/predictor_dictionary.csv"),show_col_types=FALSE)
transformations <- setNames(dictionary$transformation,dictionary$predictor)
weight_method <- "equal_study"
sets <- list()
for(a in c("DOC","NO3")) sets[[a]] <- list(revised_primary=config %>%
  filter(response_var==a) %>% arrange(position) %>% pull(predictor))
omissions <- list(full=character(), omit_burned_percent="burn_percent_fire_year",
  omit_high_severity="burn_sev_high", omit_both_burn=c("burn_percent_fire_year","burn_sev_high"),
  omit_runoff="runoffws", omit_baseflow="bfiws", omit_both_hydrology=c("runoffws","bfiws"),
  omit_watershed_area="Area_watershed_km", omit_forest_cover="forest_cover",
  omit_soil="soil_property", omit_time="post_fire_year",
  omit_all_watershed=c("Area_watershed_km","runoffws","bfiws","forest_cover","soil_property"))
labels <- c(full="Full set",omit_burned_percent="Omit burned area (%)",
  omit_high_severity="Omit high-severity burn",omit_both_burn="Omit wildfire group",
  omit_runoff="Omit runoff",omit_baseflow="Omit baseflow index",omit_both_hydrology="Omit both hydrology metrics",
  omit_watershed_area="Omit watershed area",omit_forest_cover="Omit forest cover",
  omit_soil="Omit soil property",omit_time="Omit time since fire",omit_all_watershed="Omit watershed group")
n_bootstrap <- as.integer(Sys.getenv("N_BOOTSTRAP","1000"))
stopifnot(is.finite(n_bootstrap),n_bootstrap>=10)
# Optional reuse supports extending the set comparison without repeating unchanged
# fits. Inputs, candidate sets and every ordered training/test draw must match.
reuse <- Sys.getenv("OMISSION_RESUME","0")=="1"
cached_predictions <- cached_coefficients <- cached_tuning <- cached_members <- NULL
cached_keys <- character()
if(reuse) {
  previous_settings <- read_csv(file.path(out,"run_settings.csv"),show_col_types=FALSE)
  stopifnot(previous_settings$attempts==n_bootstrap,previous_settings$seed==20260929)
  previous_manifest <- read_csv(file.path(out,"input_manifest.csv"),show_col_types=FALSE) %>%
    filter(!str_starts(path,"R/"))
  stopifnot(all(previous_manifest$md5==unname(tools::md5sum(file.path(root,previous_manifest$path)))))
  cached_predictions <- read_csv(file.path(out,"heldout_predictions.csv.gz"),show_col_types=FALSE)
  cached_coefficients <- read_csv(file.path(out,"coefficients.csv.gz"),show_col_types=FALSE)
  cached_tuning <- read_csv(file.path(out,"tuning.csv"),show_col_types=FALSE)
  cached_members <- read_csv(file.path(out,"split_membership.csv"),show_col_types=FALSE)
  cached_keys <- paste(cached_tuning$response_var,cached_tuning$validation,
                       cached_tuning$iteration,cached_tuning$specification)
  stopifnot(!anyDuplicated(cached_keys))
}

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

# Fold-specific preprocessing and weighting; fixed penalty grid avoids using validation
# outcomes to define a tuning range. Every copy of an original study shares a fold.
fit_nested <- function(raw, candidates) {
  studies <- sort(unique(raw$Study_ID))
  if(length(studies)<3) stop("Fewer than three unique studies available for inner tuning")
  fold_map <- setNames(rep(seq_len(min(5,length(studies))),length.out=length(studies)),studies)
  lambdas <- 10^seq(2,-4,length.out=80)
  errors <- matrix(NA_real_,length(studies),length(lambdas))
  for(f in unique(fold_map)) {
    tr <- raw[fold_map[raw$Study_ID]!=f,,drop=FALSE]
    va <- raw[fold_map[raw$Study_ID]==f,,drop=FALSE]
    stopifnot(!any(tr$Study_ID %in% va$Study_ID))
    prep <- prepare(tr,va,candidates)
    if(!length(prep$predictors)) {
      pred <- matrix(weighted.mean(prep$train$lnRR_mean,weights_for(prep$train)),
                     nrow=nrow(prep$test),ncol=length(lambdas))
    } else {
      x_train <- as.matrix(prep$train[prep$predictors])
      x_test <- as.matrix(prep$test[prep$predictors])
      # glmnet requires two columns. A constant zero column lets a one-predictor
      # fold fit normally; it is never selected or counted as a candidate.
      if(ncol(x_train)==1) {
        x_train <- cbind(x_train,zero_padding=0)
        x_test <- cbind(x_test,zero_padding=0)
      }
      fit <- glmnet(x_train,prep$train$lnRR_mean,
        weights=weights_for(prep$train),alpha=1,lambda=lambdas,standardize=FALSE,control=list(maxit=1000000))
      pred <- predict(fit,newx=x_test,s=lambdas)
    }
    # Equal study scoring for the primary transferability target. Within a study,
    # equal pair weighting prevents long follow-up pairs dominating tuning.
    for(st in unique(va$Study_ID)) {
      ix <- which(va$Study_ID==st)
      ew <- if(weight_method=="equal_study") weights_for(va[ix,,drop=FALSE]) else rep(1,length(ix))
      errors[match(st,studies),] <- colSums((pred[ix,,drop=FALSE]-va$lnRR_mean[ix])^2*ew)/sum(ew)
    }
  }
  mult <- if("bootstrap_cluster" %in% names(raw))
    vapply(studies,function(st) length(unique(raw$bootstrap_cluster[raw$Study_ID==st])),integer(1)) else rep(1,length(studies))
  avg <- colSums(errors*mult)/sum(mult)
  # Use unique original studies for SE, not duplicated bootstrap copies.
  se <- apply(errors,2,sd)/sqrt(length(studies))
  best <- which.min(avg)
  selected <- which(avg <= avg[best]+se[best])[1]
  prep <- prepare(raw,raw,candidates)
  if(!length(prep$predictors)) return(list(intercept_only=TRUE,
    intercept=weighted.mean(prep$train$lnRR_mean,weights_for(prep$train)),
    lambda.1se=lambdas[selected],lambda.min=lambdas[best]))
  x_train <- as.matrix(prep$train[prep$predictors])
  if(ncol(x_train)==1) x_train <- cbind(x_train,zero_padding=0)
  fit <- glmnet(x_train,prep$train$lnRR_mean,
    weights=weights_for(prep$train),alpha=1,lambda=lambdas,standardize=FALSE,control=list(maxit=1000000))
  structure(list(glmnet.fit=fit,lambda.1se=lambdas[selected],lambda.min=lambdas[best]),class="cv.glmnet")
}

predictions <- list(); coefficients <- list(); tuning <- list(); failures <- list()
split_membership <- list(); inner_folds <- list(); bootstrap_status <- list()
specifications <- crossing(response_var=c("DOC","NO3"),specification=names(omissions)) %>%
  rowwise() %>% mutate(label=labels[[specification]],
    omitted=paste(if_else(omissions[[specification]]=="soil_property",
      if_else(response_var=="DOC","omws","clayws"),omissions[[specification]]),collapse=";"),
    candidates=paste(setdiff(sets[[response_var]]$revised_primary,
      if_else(omissions[[specification]]=="soil_property",if_else(response_var=="DOC","omws","clayws"),
        omissions[[specification]])),collapse=";")) %>% ungroup()
if(reuse) {
  previous_specs <- read_csv(file.path(out,"specifications.csv"),show_col_types=FALSE)
  matched_specs <- previous_specs %>% select(response_var,specification,candidates) %>%
    left_join(specifications %>% select(response_var,specification,candidates),
      by=c("response_var","specification"),suffix=c("_old","_new"))
  stopifnot(!anyNA(matched_specs$candidates_new),all(matched_specs$candidates_old==matched_specs$candidates_new))
  predictions[[1]] <- cached_predictions
  coefficients[[1]] <- cached_coefficients
  tuning[[1]] <- cached_tuning
}
write_csv(specifications,file.path(out,"specifications.csv"))

for(a in c("DOC","NO3")) {
  rows <- filter(data,response_var==a)
  studies <- sort(unique(rows$Study_ID))
  stopifnot(length(sets[[a]]$revised_primary)==8)
  # The full-set LOSO fits must reproduce the saved primary predictions.
  # Bootstrap training draws retain multiplicity; test publications are never sampled.
  for(split_index in seq_len(length(studies)+n_bootstrap)) {
    validation <- if(split_index<=length(studies)) "leave_one_study_out" else "bootstrap_oob"
    iteration <- if(validation=="leave_one_study_out") split_index else split_index-length(studies)
    draws <- if(validation=="leave_one_study_out") studies[-split_index] else
      sample(studies,length(studies),replace=TRUE)
    held_out <- setdiff(studies,draws)
    train <- map2_dfr(draws,seq_along(draws),function(st,i)
      filter(rows,Study_ID==st) %>% mutate(bootstrap_cluster=i))
    test <- filter(rows,Study_ID %in% held_out)
    stopifnot(!any(train$Study_ID %in% test$Study_ID))
    if(reuse) {
      previous_split <- cached_members %>% filter(response_var==a,validation==!!validation,iteration==!!iteration)
      stopifnot(identical(previous_split$Study_ID[previous_split$role=="training"],draws),
                identical(previous_split$Study_ID[previous_split$role=="held_out"],held_out))
    }
    split_membership[[length(split_membership)+1]] <- bind_rows(
      tibble(Study_ID=draws,draw_id=seq_along(draws),role="training"),
      tibble(Study_ID=held_out,draw_id=NA_integer_,role="held_out")) %>%
      mutate(response_var=a,validation=validation,iteration=iteration)
    unique_training <- sort(unique(draws))
    if(length(unique_training)<3) {
      failures[[length(failures)+1]] <- tibble(response_var=a,validation=validation,
        iteration=iteration,specification="all",error="Fewer than three unique studies available for inner tuning")
      bootstrap_status[[length(bootstrap_status)+1]] <- tibble(response_var=a,iteration=iteration,
        unique_training=length(unique_training),oob_studies=length(held_out),successful=FALSE)
      next
    }
    fold_map <- setNames(rep(seq_len(min(5,length(unique_training))),length.out=length(unique_training)),unique_training)
    inner_folds[[length(inner_folds)+1]] <- tibble(response_var=a,validation=validation,
      iteration=iteration,Study_ID=unique_training,inner_fold=unname(fold_map))
    succeeded <- TRUE
    for(spec in names(omissions)) {
      if(paste(a,validation,iteration,spec) %in% cached_keys) next
      candidates <- specifications %>% filter(response_var==a,specification==spec) %>% pull(candidates) %>%
        str_split(";",simplify=FALSE) %>% .[[1]]
      fit <- tryCatch(fit_nested(train,candidates),error=function(e)e)
      if(inherits(fit,"error")) {
        succeeded <- FALSE
        failures[[length(failures)+1]] <- tibble(response_var=a,validation=validation,
          iteration=iteration,specification=spec,error=conditionMessage(fit))
        next
      }
      prep <- prepare(train,test,candidates)
      if(isTRUE(fit$intercept_only)) {
        beta_table <- tibble(predictor=character(),coefficient=double())
      } else {
        beta <- as.matrix(coef(fit,s="lambda.1se"))
        beta_table <- tibble(predictor=rownames(beta),coefficient=as.numeric(beta)) %>%
          filter(predictor!="(Intercept)",predictor!="zero_padding")
      }
      coefficients[[length(coefficients)+1]] <- tibble(predictor=sets[[a]]$revised_primary) %>%
        left_join(beta_table,by="predictor") %>% mutate(
          available=predictor %in% candidates,
          coefficient=if_else(available,replace_na(coefficient,0),NA_real_),
          response_var=a,validation=validation,iteration=iteration,specification=spec)
      tuning[[length(tuning)+1]] <- tibble(response_var=a,validation=validation,
        iteration=iteration,specification=spec,lambda_1se=fit$lambda.1se,lambda_min=fit$lambda.min,
        n_train_studies=length(unique_training),n_test_studies=length(held_out))
      if(nrow(test)) {
        if(isTRUE(fit$intercept_only)) {
          predicted <- rep(fit$intercept,nrow(test))
        } else {
          x_test <- as.matrix(prep$test[prep$predictors])
          if(ncol(x_test)==1) x_test <- cbind(x_test,zero_padding=0)
          predicted <- as.numeric(predict(fit,newx=x_test,s="lambda.1se"))
        }
        stopifnot(length(predicted)==nrow(test),all(is.finite(predicted)))
        predictions[[length(predictions)+1]] <- test %>%
          select(row_id,Study_ID,candidate_pair_id,response_var,year) %>%
          mutate(validation=validation,iteration=iteration,specification=spec,
            observed=test$lnRR_mean,predicted=predicted)
      }
    }
    if(validation=="bootstrap_oob") bootstrap_status[[length(bootstrap_status)+1]] <- tibble(
      response_var=a,iteration=iteration,unique_training=length(unique_training),
      oob_studies=length(held_out),successful=succeeded)
    if(validation=="bootstrap_oob" && iteration%%50==0) {
      message(a,": ",iteration,"/",n_bootstrap," paired bootstrap attempts complete")
      # Preserve progress without replacing the finalized result tables.
      write_csv(bind_rows(bootstrap_status),file.path(out,"bootstrap_progress.csv"))
    }
  }
}
predictions <- bind_rows(predictions)
coefficients <- bind_rows(coefficients)
tuning <- bind_rows(tuning)
failures <- bind_rows(failures)
status <- bind_rows(bootstrap_status)
stopifnot(!anyDuplicated(predictions[c("response_var","validation","iteration","specification","row_id")]))

# Evaluate the same held-out records for every specification; each test study
# contributes one MSE regardless of its record count, matching primary evaluation.
study_errors <- predictions %>% group_by(response_var,validation,iteration,specification,Study_ID) %>%
  summarise(n=n(),MSE=mean((observed-predicted)^2),MAE=mean(abs(observed-predicted)),.groups="drop")
oob_errors <- study_errors %>% filter(validation=="bootstrap_oob") %>%
  semi_join(filter(status,successful),by=c("response_var","iteration")) %>%
  group_by(response_var,iteration,specification) %>%
  summarise(n_test_studies=n(),RMSE=sqrt(mean(MSE)),MAE=mean(MAE),.groups="drop")
loso_errors <- study_errors %>% filter(validation=="leave_one_study_out") %>%
  group_by(response_var,specification) %>%
  summarise(n_test_studies=n(),RMSE=sqrt(mean(MSE)),MAE=mean(MAE),.groups="drop")
paired_oob <- oob_errors %>% filter(specification!="full") %>%
  left_join(oob_errors %>% filter(specification=="full") %>%
    select(response_var,iteration,full_RMSE=RMSE,full_MAE=MAE,full_test_studies=n_test_studies),
    by=c("response_var","iteration")) %>%
  mutate(delta_RMSE=RMSE-full_RMSE,delta_MAE=MAE-full_MAE)
stopifnot(all(paired_oob$n_test_studies==paired_oob$full_test_studies),
          all(is.finite(paired_oob$delta_RMSE)))
loso_deltas <- loso_errors %>% filter(specification!="full") %>%
  left_join(loso_errors %>% filter(specification=="full") %>%
    select(response_var,full_RMSE=RMSE,full_MAE=MAE),by="response_var") %>%
  mutate(delta_RMSE=RMSE-full_RMSE,delta_MAE=MAE-full_MAE)
paired_summary <- paired_oob %>% group_by(response_var,specification) %>%
  summarise(paired_oob_refits=n(),median_delta_RMSE=median(delta_RMSE),
    lower_delta_RMSE=quantile(delta_RMSE,.025),upper_delta_RMSE=quantile(delta_RMSE,.975),
    proportion_worse=mean(delta_RMSE>1e-10),proportion_tied=mean(abs(delta_RMSE)<=1e-10),
    proportion_better=mean(delta_RMSE < -1e-10),.groups="drop") %>%
  left_join(loso_deltas,by=c("response_var","specification"))
bootstrap_coefs <- coefficients %>% filter(validation=="bootstrap_oob") %>%
  semi_join(filter(status,successful),by=c("response_var","iteration"))
stability <- bootstrap_coefs %>% group_by(response_var,specification,predictor,available) %>%
  summarise(successful_refits=n(),selection_frequency=if(first(available)) mean(coefficient!=0) else NA_real_,
    median_coefficient=if(first(available)) median(coefficient) else NA_real_,
    positive_frequency=if(first(available)) mean(coefficient>0) else NA_real_,
    negative_frequency=if(first(available)) mean(coefficient<0) else NA_real_,.groups="drop")
bootstrap_summary <- status %>% group_by(response_var) %>% summarise(attempted=n(),
  failed=sum(!successful),
  successful_without_oob=sum(successful & oob_studies==0),
  paired_oob_refits=sum(successful & oob_studies>0),successful=sum(successful),.groups="drop") %>%
  select(response_var,attempted,successful,failed,successful_without_oob,paired_oob_refits)

# Group contrasts target the manuscript hypotheses. Time is retained in both
# group exclusions. Relative errors aid descriptive cross-analyte comparison;
# the analytes still differ in study coverage, response variation and soil field.
group_loso <- loso_deltas %>% filter(specification %in% c("omit_all_watershed","omit_both_burn")) %>%
  select(response_var,specification,full_RMSE,delta_RMSE) %>%
  pivot_wider(names_from=specification,values_from=delta_RMSE) %>%
  mutate(watershed_relative_RMSE_change=omit_all_watershed/full_RMSE,
    wildfire_relative_RMSE_change=omit_both_burn/full_RMSE,
    watershed_minus_wildfire=omit_all_watershed-omit_both_burn)
group_oob <- paired_oob %>% filter(specification %in% c("omit_all_watershed","omit_both_burn")) %>%
  select(response_var,iteration,specification,full_RMSE,delta_RMSE) %>%
  pivot_wider(names_from=specification,values_from=delta_RMSE) %>%
  mutate(watershed_minus_wildfire=omit_all_watershed-omit_both_burn,
    relative_watershed_minus_wildfire=watershed_minus_wildfire/full_RMSE)
group_summary <- group_oob %>% group_by(response_var) %>%
  summarise(paired_oob_refits=n(),median_group_contrast=median(watershed_minus_wildfire),
    lower_group_contrast=quantile(watershed_minus_wildfire,.025),
    upper_group_contrast=quantile(watershed_minus_wildfire,.975),
    proportion_watershed_larger=mean(watershed_minus_wildfire>1e-10),
    proportion_groups_tied=mean(abs(watershed_minus_wildfire)<=1e-10),
    proportion_wildfire_larger=mean(watershed_minus_wildfire < -1e-10),.groups="drop") %>%
  left_join(group_loso,by="response_var")
group_stability <- bootstrap_coefs %>% mutate(predictor_group=case_when(
  predictor %in% c("burn_percent_fire_year","burn_sev_high") ~ "wildfire",
  predictor=="post_fire_year" ~ "time",TRUE ~ "watershed")) %>%
  group_by(response_var,iteration,specification,predictor_group) %>%
  summarise(available_count=sum(available),
    any_selected=if(any(available)) any(replace_na(coefficient,0)!=0) else NA,.groups="drop") %>%
  group_by(response_var,specification,predictor_group,available_count) %>%
  summarise(successful_refits=n(),group_selection_frequency=if(first(available_count)>0)
    mean(any_selected) else NA_real_,.groups="drop")

# Guard against drift from the established primary model before publishing outputs.
reference <- read_csv(file.path(root,"output/paper/tables/heldout_predictions.csv"),show_col_types=FALSE) %>%
  filter(model=="lasso") %>% select(response_var,row_id,reference_prediction=predicted)
comparison <- predictions %>% filter(validation=="leave_one_study_out",specification=="full") %>%
  left_join(reference,by=c("response_var","row_id"))
stopifnot(nrow(comparison)==nrow(data),!anyNA(comparison$reference_prediction),
          max(abs(comparison$predicted-comparison$reference_prediction))<1e-10)
if(n_bootstrap==1000) {
  reference_stability <- read_csv(file.path(root,"output/paper/tables/selection_stability.csv"),show_col_types=FALSE)
  comparison_stability <- stability %>% filter(specification=="full") %>%
    left_join(reference_stability,by=c("response_var","predictor"),suffix=c("","_reference"))
  stopifnot(all(comparison_stability$successful_refits==comparison_stability$completed_iterations),
    max(abs(comparison_stability$selection_frequency-comparison_stability$selection_frequency_reference))<1e-12)
}
write_csv(predictions,file.path(out,"heldout_predictions.csv.gz"))
write_csv(coefficients,file.path(out,"coefficients.csv.gz"))
write_csv(tuning,file.path(out,"tuning.csv"))
write_csv(failures,file.path(out,"failures.csv"))
write_csv(bind_rows(split_membership),file.path(out,"split_membership.csv"))
write_csv(bind_rows(inner_folds),file.path(out,"inner_fold_assignments.csv"))
write_csv(status,file.path(out,"bootstrap_status.csv"))
write_csv(bootstrap_summary,file.path(out,"bootstrap_summary.csv"))
write_csv(study_errors,file.path(out,"performance_by_study.csv"))
write_csv(loso_errors,file.path(out,"performance_loso.csv"))
write_csv(paired_oob,file.path(out,"paired_oob_differences.csv"))
write_csv(paired_summary,file.path(out,"paired_omission_summary.csv"))
write_csv(stability,file.path(out,"selection_stability.csv"))
write_csv(group_summary,file.path(out,"group_contribution_summary.csv"))
write_csv(group_oob,file.path(out,"paired_group_contrasts.csv"))
write_csv(group_stability,file.path(out,"group_selection_stability.csv"))
input_paths <- c("data/derived/lasso_model_table.csv","config/issue6_revised_primary_predictors.csv",
                 "config/predictor_dictionary.csv","R/sensitivities/02_run_predictor_omission_agent_v1.R")
write_csv(tibble(path=input_paths,md5=unname(tools::md5sum(file.path(root,input_paths)))),file.path(out,"input_manifest.csv"))
write_csv(tibble(seed=20260929,attempts=n_bootstrap,alpha=1,penalty_rule="one_SE",
  penalty_min=.0001,penalty_max=100,penalty_count=80,weighting="equal_study_pair_year",
  validation="publication_LOSO_and_paired_bootstrap_OOB"),file.path(out,"run_settings.csv"))
capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
print(bootstrap_summary)
print(paired_summary,width=Inf)
print(group_summary,width=Inf)
message("Predictor-omission analysis complete; full-set LOSO predictions reproduce primary results.")
