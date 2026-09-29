# User-defined issue #6 primary sets for paired annual lnRR.
# Builds from 09_issue6_curated_lasso_agent_v1.R without changing prior results.
# Run from repository root: N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/11_fit_issue6_revised_primary_agent_v1.R

library(tidyverse)
library(here)
library(glmnet)

set.seed(20260929)
root <- here("agent_workflows", "vibe_coding")
out <- file.path(root, "output", "issue_6_revised_primary")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
data <- read_csv(file.path(root, "data/derived/lasso_model_table.csv"), show_col_types = FALSE) %>%
  mutate(row_id = row_number()) %>% filter(is.finite(lnRR_mean))
dictionary <- read_csv(file.path(root, "config/predictor_dictionary.csv"), show_col_types = FALSE)
old_set <- dictionary$predictor[as.character(dictionary$include_primary) == "TRUE"]
primary_config <- read_csv(file.path(root, "config/issue6_revised_primary_predictors.csv"),
                           show_col_types = FALSE)
if (anyDuplicated(primary_config[c("response_var", "predictor")])) {
  stop("Duplicate predictor in revised primary configuration.")
}
if (!setequal(primary_config$response_var, c("DOC", "NO3"))) {
  stop("Revised primary configuration must include DOC and NO3.")
}
sets <- list()
for (a in c("DOC", "NO3")) {
  selected <- primary_config %>% filter(response_var == a) %>% arrange(position) %>% pull(predictor)
  if (length(selected) != 8) stop("Expected eight primary predictors for ", a)
  sets[[a]] <- list(current_shared = old_set, revised_primary = selected)
}
required <- unique(unlist(sets))
missing <- setdiff(required, names(data))
if (length(missing)) stop("Missing model-table predictors: ", paste(missing, collapse = ", "))
transformations <- setNames(dictionary$transformation, dictionary$predictor)
revised_dictionary <- dictionary %>%
  select(predictor, predictor_label, predictor_group, transformation,
         proportion_missing, n_unique) %>%
  mutate(DOC_primary = predictor %in% sets$DOC$revised_primary,
         NO3_primary = predictor %in% sets$NO3$revised_primary,
         decision_note = if_else(DOC_primary | NO3_primary,
           "User-defined primary set, 2026-09-29. Both burn measures and both hydrology measures are included by request.",
           "Not in the user-defined primary sets."))
write_csv(revised_dictionary, file.path(root, "config/issue6_revised_primary_dictionary.csv"))

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
  precision <- ifelse(is.finite(rows$lnRR_var) & rows$lnRR_var > 0,
                      1 / rows$lnRR_var, NA_real_)
  if (all(is.na(precision))) precision[] <- 1
  precision[is.na(precision)] <- median(precision, na.rm = TRUE)
  precision <- pmin(precision, as.numeric(quantile(precision, .95)))
  w <- precision * rows$reference_family_weight
  w / mean(w)
}

predictions <- list()
coefficients <- list()
failures <- list()
for (a in names(sets)) {
  rows <- filter(data, response_var == a)
  studies <- sort(unique(rows$Study_ID))
  if (length(studies) < 4) stop("Too few studies for ", a)
  for (set_name in names(sets[[a]])) {
    candidates <- sets[[a]][[set_name]]
    for (held_out in studies) {
      prepared <- prepare(filter(rows, Study_ID != held_out),
                          filter(rows, Study_ID == held_out), candidates)
      train <- prepared$train
      test <- prepared$test
      usable <- prepared$predictors
      if (!length(usable)) stop("No usable predictors: ", a, " / ", set_name)
      train_studies <- sort(unique(train$Study_ID))
      fold_map <- setNames(rep(seq_len(min(5, length(train_studies))),
                               length.out = length(train_studies)), train_studies)
      fold_id <- unname(fold_map[train$Study_ID])
      w <- weights_for(train)
      fit <- tryCatch(cv.glmnet(as.matrix(train[usable]), train$lnRR_mean,
                                weights = w, alpha = 1, foldid = fold_id,
                                standardize = FALSE, type.measure = "mse",
                                control = list(maxit = 1000000)),
                      error = function(e) e)
      if (inherits(fit, "error")) {
        failures[[length(failures) + 1]] <- tibble(response_var = a, predictor_set = set_name,
          held_out_study = held_out, error = conditionMessage(fit))
        next
      }
      x_test <- as.matrix(test[usable])
      baseline <- rep(weighted.mean(train$lnRR_mean, w), nrow(test))
      time_fit <- lm(lnRR_mean ~ post_fire_year, data = train, weights = w)
      fire_term <- if (a == "DOC") "burn_sev_high" else "burn_percent_fire_year"
      fire_terms <- intersect(c("post_fire_year", fire_term), usable)
      fire_fit <- lm(reformulate(fire_terms, response = "lnRR_mean"), data = train, weights = w)
      model_predictions <- list(intercept_only = baseline,
        time_only = as.numeric(predict(time_fit, newdata = test)),
        time_plus_fire = as.numeric(predict(fire_fit, newdata = test)),
        lasso = as.numeric(predict(fit, newx = x_test, s = "lambda.1se")))
      for (model_name in names(model_predictions)) {
        predictions[[length(predictions) + 1]] <- tibble(response_var = a,
          predictor_set = set_name, held_out_study = held_out, row_id = test$row_id,
          model = model_name, observed = test$lnRR_mean,
          predicted = model_predictions[[model_name]])
      }
      beta <- as.matrix(coef(fit, s = "lambda.1se"))
      coefficients[[length(coefficients) + 1]] <- tibble(response_var = a,
        predictor_set = set_name, held_out_study = held_out,
        predictor = rownames(beta), coefficient = as.numeric(beta),
        lambda = fit$lambda.1se)
    }
  }
}
predictions <- bind_rows(predictions)
coefficients <- bind_rows(coefficients)
failures <- bind_rows(failures)
if (!nrow(predictions)) stop("No outer fits completed")
performance <- predictions %>% group_by(response_var, predictor_set, model) %>%
  summarise(n = n(), n_studies = n_distinct(held_out_study),
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE = mean(abs(observed - predicted)),
    R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2),
    .groups = "drop")
write_csv(predictions, file.path(out, "heldout_predictions.csv"))
write_csv(coefficients, file.path(out, "outer_coefficients.csv"))
write_csv(performance, file.path(out, "performance.csv"))
write_csv(failures, file.path(out, "outer_failures.csv"))

n_bootstrap <- as.integer(Sys.getenv("N_BOOTSTRAP", "1000"))
if (!is.finite(n_bootstrap) || n_bootstrap < 10) stop("N_BOOTSTRAP must be >= 10")
boot_coefs <- list()
boot_failures <- list()
for (a in names(sets)) {
  rows <- filter(data, response_var == a)
  studies <- sort(unique(rows$Study_ID))
  candidates <- sets[[a]]$revised_primary
  for (iteration in seq_len(n_bootstrap)) {
    draws <- sample(studies, length(studies), replace = TRUE)
    sample_rows <- map2_dfr(draws, seq_along(draws), function(study, i)
      filter(rows, Study_ID == study) %>% mutate(bootstrap_cluster = i))
    prepared <- prepare(sample_rows, sample_rows, candidates)
    boot <- prepared$train
    usable <- prepared$predictors
    if (!length(usable)) next
    clusters <- unique(boot$bootstrap_cluster)
    fold_map <- setNames(rep(seq_len(min(5, length(clusters))), length.out = length(clusters)),
                         clusters)
    fit <- tryCatch(cv.glmnet(as.matrix(boot[usable]), boot$lnRR_mean,
                              weights = weights_for(boot), alpha = 1,
                              foldid = unname(fold_map[as.character(boot$bootstrap_cluster)]),
                              standardize = FALSE, type.measure = "mse",
                              control = list(maxit = 1000000)),
                    error = function(e) e)
    if (inherits(fit, "error")) {
      boot_failures[[length(boot_failures) + 1]] <- tibble(response_var = a,
        iteration = iteration, error = conditionMessage(fit))
      next
    }
    beta <- as.matrix(coef(fit, s = "lambda.1se"))
    boot_coefs[[length(boot_coefs) + 1]] <- tibble(response_var = a,
      iteration = iteration, predictor = rownames(beta), coefficient = as.numeric(beta)) %>%
      filter(predictor != "(Intercept)")
  }
  message(a, " bootstrap done: ", n_bootstrap)
}
boot_coefs <- bind_rows(boot_coefs)
boot_failures <- bind_rows(boot_failures)
complete_grid <- boot_coefs %>% distinct(response_var, iteration) %>%
  group_by(response_var) %>% group_modify(~ crossing(iteration = .x$iteration,
    predictor = sets[[.y$response_var]]$revised_primary)) %>% ungroup() %>%
  left_join(boot_coefs, by = c("response_var", "iteration", "predictor")) %>%
  mutate(coefficient = replace_na(coefficient, 0))
stability <- complete_grid %>% group_by(response_var, predictor) %>%
  summarise(completed_iterations = n(), selection_frequency = mean(coefficient != 0),
    median_coefficient = median(coefficient), positive_frequency = mean(coefficient > 0),
    negative_frequency = mean(coefficient < 0), .groups = "drop")
write_csv(boot_coefs, file.path(out, "bootstrap_coefficients.csv"))
write_csv(boot_failures, file.path(out, "bootstrap_failures.csv"))
write_csv(stability, file.path(out, "selection_stability.csv"))
message("Issue #6 revised-primary rerun complete. Outputs: ", out)
