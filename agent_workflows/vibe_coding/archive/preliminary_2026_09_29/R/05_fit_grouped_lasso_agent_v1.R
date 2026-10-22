# ==============================================================================
# Script: 05_fit_grouped_lasso_agent_v1.R
# Purpose: Fit annual-lnRR LASSO models with leave-one-study-out evaluation and
#          grouped inner cross-validation.
# Replaces, without modifying: R_scripts/Metaanalysis_LASSO.R.
# ==============================================================================

library(tidyverse)
library(here)
library(glmnet)

set.seed(20260820)

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
predictor_dictionary_path <- file.path(workflow_dir, "config", "predictor_dictionary.csv")
model_dir <- file.path(workflow_dir, "output", "models")
table_dir <- file.path(workflow_dir, "output", "tables")
log_dir <- file.path(workflow_dir, "output", "logs")

if (!file.exists(model_table_path) || !file.exists(predictor_dictionary_path)) {
  stop("Run scripts 02 and 03 before grouped LASSO.")
}
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

model_table <- read_csv(model_table_path, show_col_types = FALSE) %>%
  mutate(row_id = row_number()) %>%
  filter(is.finite(lnRR_mean))
predictor_dictionary <- read_csv(predictor_dictionary_path, show_col_types = FALSE)

include_value <- tolower(as.character(predictor_dictionary$include_primary)) %in% c("true", "t", "1")
primary_predictors <- predictor_dictionary$predictor[include_value]
primary_predictors <- intersect(primary_predictors, names(model_table))
if (length(primary_predictors) == 0) stop("No primary predictors are available.")

transform_predictors <- function(training_data, test_data, predictors, dictionary) {
  transformations <- setNames(dictionary$transformation, dictionary$predictor)

  for (predictor in predictors) {
    if (!is.numeric(training_data[[predictor]])) {
      training_data[[predictor]] <- parse_number(as.character(training_data[[predictor]]))
      test_data[[predictor]] <- parse_number(as.character(test_data[[predictor]]))
    }
    if (!is.na(transformations[[predictor]]) && transformations[[predictor]] == "log1p") {
      training_data[[predictor]] <- log1p(pmax(training_data[[predictor]], 0))
      test_data[[predictor]] <- log1p(pmax(test_data[[predictor]], 0))
    }
  }

  usable_predictors <- predictors[vapply(
    training_data[predictors],
    function(values) sum(is.finite(values)) >= 3 && sd(values, na.rm = TRUE) > 0,
    logical(1)
  )]

  if (length(usable_predictors) == 0) {
    return(list(training = training_data, test = test_data, predictors = character()))
  }

  for (predictor in usable_predictors) {
    training_median <- median(training_data[[predictor]], na.rm = TRUE)
    training_data[[predictor]][is.na(training_data[[predictor]])] <- training_median
    test_data[[predictor]][is.na(test_data[[predictor]])] <- training_median

    training_mean <- mean(training_data[[predictor]])
    training_sd <- sd(training_data[[predictor]])
    training_data[[predictor]] <- (training_data[[predictor]] - training_mean) / training_sd
    test_data[[predictor]] <- (test_data[[predictor]] - training_mean) / training_sd
  }

  list(training = training_data, test = test_data, predictors = usable_predictors)
}

outer_predictions <- list()
outer_coefficients <- list()
fold_records <- list()
failure_records <- list()
saved_fits <- list()

for (analyte in sort(unique(model_table$response_var))) {
  analyte_data <- model_table %>% filter(response_var == analyte)
  study_ids <- sort(unique(analyte_data$Study_ID))

  if (length(study_ids) < 4) {
    failure_records[[analyte]] <- tibble(
      response_var = analyte,
      held_out_study = NA_character_,
      error = "Fewer than four studies; grouped nested validation not fitted."
    )
    next
  }

  for (held_out_study in study_ids) {
    training_data <- analyte_data %>% filter(Study_ID != held_out_study)
    test_data <- analyte_data %>% filter(Study_ID == held_out_study)

    prepared <- transform_predictors(
      training_data, test_data, primary_predictors, predictor_dictionary
    )
    training_data <- prepared$training
    test_data <- prepared$test
    usable_predictors <- prepared$predictors

    if (length(usable_predictors) == 0) {
      failure_records[[paste(analyte, held_out_study)]] <- tibble(
        response_var = analyte,
        held_out_study = held_out_study,
        error = "No variable predictors in the outer training set."
      )
      next
    }

    training_studies <- sort(unique(training_data$Study_ID))
    n_inner_folds <- min(5, length(training_studies))
    if (n_inner_folds < 3) next

    study_fold_map <- tibble(
      Study_ID = training_studies,
      inner_fold = rep(seq_len(n_inner_folds), length.out = length(training_studies))
    )
    fold_id <- training_data %>%
      select(Study_ID) %>%
      left_join(study_fold_map, by = "Study_ID") %>%
      pull(inner_fold)

    x_training <- as.matrix(training_data %>% select(all_of(usable_predictors)))
    x_test <- as.matrix(test_data %>% select(all_of(usable_predictors)))
    y_training <- training_data$lnRR_mean

    precision <- if_else(
      is.finite(training_data$lnRR_var) & training_data$lnRR_var > 0,
      1 / training_data$lnRR_var,
      NA_real_
    )
    if (all(is.na(precision))) precision <- rep(1, nrow(training_data))
    precision_cap <- quantile(precision, 0.95, na.rm = TRUE)
    precision[is.na(precision)] <- median(precision, na.rm = TRUE)
    model_weights <- pmin(precision, precision_cap) * training_data$reference_family_weight
    model_weights <- model_weights / mean(model_weights)

    fitted_cv <- tryCatch(
      cv.glmnet(
        x = x_training,
        y = y_training,
        weights = model_weights,
        alpha = 1,
        foldid = fold_id,
        standardize = FALSE,
        intercept = TRUE,
        type.measure = "mse"
      ),
      error = function(error) error
    )

    if (inherits(fitted_cv, "error")) {
      failure_records[[paste(analyte, held_out_study)]] <- tibble(
        response_var = analyte,
        held_out_study = held_out_study,
        error = conditionMessage(fitted_cv)
      )
      next
    }

    lasso_prediction <- as.numeric(predict(fitted_cv, newx = x_test, s = "lambda.1se"))
    intercept_prediction <- rep(weighted.mean(y_training, model_weights), nrow(test_data))

    time_predictors <- intersect("post_fire_year", usable_predictors)
    fire_predictors <- intersect("burn_percent_fire_year", usable_predictors)

    if (length(time_predictors) == 1) {
      time_formula <- reformulate(time_predictors, response = "lnRR_mean")
      time_model <- lm(time_formula, data = training_data, weights = model_weights)
      time_prediction <- as.numeric(predict(time_model, newdata = test_data))
    } else {
      time_prediction <- intercept_prediction
    }

    time_fire_predictors <- unique(c(time_predictors, fire_predictors))
    if (length(time_fire_predictors) >= 1) {
      time_fire_formula <- reformulate(time_fire_predictors, response = "lnRR_mean")
      time_fire_model <- lm(time_fire_formula, data = training_data, weights = model_weights)
      time_fire_prediction <- as.numeric(predict(time_fire_model, newdata = test_data))
    } else {
      time_fire_prediction <- intercept_prediction
    }

    prediction_block <- bind_rows(
      tibble(model = "intercept_only", predicted = intercept_prediction),
      tibble(model = "time_only", predicted = time_prediction),
      tibble(model = "time_plus_fire", predicted = time_fire_prediction),
      tibble(model = "lasso", predicted = lasso_prediction)
    ) %>%
      mutate(
        response_var = analyte,
        held_out_study = held_out_study,
        row_id = rep(test_data$row_id, times = 4),
        observed = rep(test_data$lnRR_mean, times = 4)
      ) %>%
      select(response_var, held_out_study, row_id, model, observed, predicted)

    coefficient_matrix <- as.matrix(coef(fitted_cv, s = "lambda.1se"))
    coefficient_table <- tibble(
      response_var = analyte,
      held_out_study = held_out_study,
      predictor = rownames(coefficient_matrix),
      coefficient = as.numeric(coefficient_matrix),
      lambda = fitted_cv$lambda.1se
    )

    key <- paste(analyte, held_out_study, sep = "__")
    outer_predictions[[key]] <- prediction_block
    outer_coefficients[[key]] <- coefficient_table
    fold_records[[key]] <- tibble(
      response_var = analyte,
      held_out_study = held_out_study,
      training_study = training_studies,
      inner_fold = study_fold_map$inner_fold
    )
    saved_fits[[key]] <- fitted_cv
  }
}

heldout_predictions <- bind_rows(outer_predictions)
outer_fold_coefficients <- bind_rows(outer_coefficients)
grouped_fold_assignments <- bind_rows(fold_records)
lasso_failures <- bind_rows(failure_records)

if (nrow(heldout_predictions) == 0) stop("No grouped LASSO folds completed successfully.")

predictive_performance <- heldout_predictions %>%
  group_by(response_var, model) %>%
  summarise(
    n = n(),
    n_studies = n_distinct(held_out_study),
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE = mean(abs(observed - predicted)),
    R2 = 1 - sum((observed - predicted)^2) /
      sum((observed - mean(observed))^2),
    .groups = "drop"
  )

write_csv(heldout_predictions, file.path(table_dir, "grouped_lasso_predictions.csv"))
write_csv(predictive_performance, file.path(table_dir, "grouped_lasso_performance.csv"))
write_csv(outer_fold_coefficients, file.path(table_dir, "grouped_lasso_coefficients.csv"))
write_csv(grouped_fold_assignments, file.path(table_dir, "grouped_fold_assignments.csv"))
write_csv(lasso_failures, file.path(log_dir, "grouped_lasso_failures.csv"))
saveRDS(saved_fits, file.path(model_dir, "grouped_lasso_outer_fits.rds"))

message("Completed ", length(saved_fits), " leave-one-study-out LASSO fits.")

