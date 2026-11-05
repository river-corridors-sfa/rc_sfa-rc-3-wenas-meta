# ==============================================================================
# Script: 06_run_stability_sensitivity_agent_v1.R
# Purpose: Estimate study-bootstrap predictor stability for LASSO, elastic net,
#          and an unweighted LASSO sensitivity.
# ==============================================================================

library(tidyverse)
library(here)
library(glmnet)

set.seed(20260820)

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
predictor_dictionary_path <- file.path(workflow_dir, "config", "predictor_dictionary.csv")
table_dir <- file.path(workflow_dir, "output", "tables")
log_dir <- file.path(workflow_dir, "output", "logs")

if (!file.exists(model_table_path) || !file.exists(predictor_dictionary_path)) {
  stop("Run scripts 02 and 03 before stability analysis.")
}
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

n_bootstrap <- as.integer(Sys.getenv("N_BOOTSTRAP", unset = "100"))
if (!is.finite(n_bootstrap) || n_bootstrap < 10) stop("N_BOOTSTRAP must be at least 10.")

model_table <- read_csv(model_table_path, show_col_types = FALSE) %>%
  filter(is.finite(lnRR_mean))
predictor_dictionary <- read_csv(predictor_dictionary_path, show_col_types = FALSE)

include_value <- tolower(as.character(predictor_dictionary$include_primary)) %in% c("true", "t", "1")
primary_predictors <- predictor_dictionary$predictor[include_value]
primary_predictors <- intersect(primary_predictors, names(model_table))
transformations <- setNames(predictor_dictionary$transformation, predictor_dictionary$predictor)

scenarios <- tribble(
  ~scenario, ~alpha, ~use_family_weights,
  "lasso_family_balanced", 1.0, TRUE,
  "elastic_net_family_balanced", 0.5, TRUE,
  "lasso_unweighted", 1.0, FALSE
)

coefficient_records <- list()
failure_records <- list()
iteration_status <- list()

for (analyte in sort(unique(model_table$response_var))) {
  analyte_data <- model_table %>% filter(response_var == analyte)
  studies <- sort(unique(analyte_data$Study_ID))
  if (length(studies) < 4) next

  for (scenario_row in seq_len(nrow(scenarios))) {
    scenario_name <- scenarios$scenario[scenario_row]
    scenario_alpha <- scenarios$alpha[scenario_row]
    use_family_weights <- scenarios$use_family_weights[scenario_row]

    for (iteration in seq_len(n_bootstrap)) {
      sampled_studies <- sample(studies, length(studies), replace = TRUE)
      bootstrap_data <- map2_dfr(
        sampled_studies,
        seq_along(sampled_studies),
        function(study, draw_id) {
          analyte_data %>%
            filter(Study_ID == study) %>%
            mutate(bootstrap_cluster = paste0(study, "__draw_", draw_id))
        }
      )

      usable_predictors <- primary_predictors[vapply(
        bootstrap_data[primary_predictors],
        function(values) sum(is.finite(values)) >= 3 && sd(values, na.rm = TRUE) > 0,
        logical(1)
      )]

      if (length(usable_predictors) == 0) {
        failure_records[[length(failure_records) + 1]] <- tibble(
          response_var = analyte, scenario = scenario_name,
          iteration = iteration, error = "No usable predictors."
        )
        next
      }

      for (predictor in usable_predictors) {
        if (!is.numeric(bootstrap_data[[predictor]])) {
          bootstrap_data[[predictor]] <- parse_number(as.character(bootstrap_data[[predictor]]))
        }
        if (!is.na(transformations[[predictor]]) && transformations[[predictor]] == "log1p") {
          bootstrap_data[[predictor]] <- log1p(pmax(bootstrap_data[[predictor]], 0))
        }
        predictor_median <- median(bootstrap_data[[predictor]], na.rm = TRUE)
        bootstrap_data[[predictor]][is.na(bootstrap_data[[predictor]])] <- predictor_median
        bootstrap_data[[predictor]] <- as.numeric(scale(bootstrap_data[[predictor]]))
      }

      cluster_ids <- unique(bootstrap_data$bootstrap_cluster)
      n_inner_folds <- min(5, length(cluster_ids))
      if (n_inner_folds < 3) next
      cluster_fold_map <- tibble(
        bootstrap_cluster = cluster_ids,
        inner_fold = rep(seq_len(n_inner_folds), length.out = length(cluster_ids))
      )
      fold_id <- bootstrap_data %>%
        select(bootstrap_cluster) %>%
        left_join(cluster_fold_map, by = "bootstrap_cluster") %>%
        pull(inner_fold)

      fit_weights <- if (use_family_weights) {
        bootstrap_data$reference_family_weight
      } else {
        rep(1, nrow(bootstrap_data))
      }
      fit_weights <- fit_weights / mean(fit_weights)

      fitted_cv <- tryCatch(
        cv.glmnet(
          x = as.matrix(bootstrap_data %>% select(all_of(usable_predictors))),
          y = bootstrap_data$lnRR_mean,
          weights = fit_weights,
          alpha = scenario_alpha,
          foldid = fold_id,
          standardize = FALSE,
          intercept = TRUE,
          type.measure = "mse"
        ),
        error = function(error) error
      )

      if (inherits(fitted_cv, "error")) {
        failure_records[[length(failure_records) + 1]] <- tibble(
          response_var = analyte, scenario = scenario_name,
          iteration = iteration, error = conditionMessage(fitted_cv)
        )
        next
      }

      coefficient_matrix <- as.matrix(coef(fitted_cv, s = "lambda.1se"))
      coefficient_records[[length(coefficient_records) + 1]] <- tibble(
        response_var = analyte,
        scenario = scenario_name,
        iteration = iteration,
        predictor = rownames(coefficient_matrix),
        coefficient = as.numeric(coefficient_matrix),
        lambda = fitted_cv$lambda.1se
      ) %>%
        filter(predictor != "(Intercept)")

      iteration_status[[length(iteration_status) + 1]] <- tibble(
        response_var = analyte, scenario = scenario_name,
        iteration = iteration, completed = TRUE
      )
    }
  }
}

bootstrap_coefficients <- bind_rows(coefficient_records)
bootstrap_failures <- bind_rows(failure_records)
completed_iterations <- bind_rows(iteration_status)

if (nrow(bootstrap_coefficients) == 0) stop("No bootstrap models completed.")

complete_coefficient_grid <- completed_iterations %>%
  crossing(predictor = primary_predictors) %>%
  left_join(
    bootstrap_coefficients %>%
      select(response_var, scenario, iteration, predictor, coefficient),
    by = c("response_var", "scenario", "iteration", "predictor")
  ) %>%
  mutate(coefficient = replace_na(coefficient, 0))

selection_stability <- complete_coefficient_grid %>%
  group_by(response_var, scenario, predictor) %>%
  summarise(
    completed_iterations = n(),
    selection_frequency = mean(coefficient != 0),
    median_coefficient = median(coefficient),
    coefficient_q025 = quantile(coefficient, 0.025),
    coefficient_q975 = quantile(coefficient, 0.975),
    positive_frequency = mean(coefficient > 0),
    negative_frequency = mean(coefficient < 0),
    .groups = "drop"
  ) %>%
  mutate(
    stability_class = case_when(
      selection_frequency >= 0.75 ~ "stable",
      selection_frequency >= 0.40 ~ "conditional",
      TRUE ~ "weak"
    )
  )

sensitivity_summary <- selection_stability %>%
  select(response_var, scenario, predictor, selection_frequency,
         median_coefficient, stability_class)

write_csv(bootstrap_coefficients, file.path(table_dir, "bootstrap_coefficients.csv"))
write_csv(selection_stability, file.path(table_dir, "lasso_selection_stability.csv"))
write_csv(sensitivity_summary, file.path(table_dir, "lasso_sensitivity_summary.csv"))
write_csv(bootstrap_failures, file.path(log_dir, "bootstrap_failures.csv"))

message("Completed ", nrow(completed_iterations), " clustered bootstrap fits.")
message("Set N_BOOTSTRAP=1000 for final manuscript stability estimates.")

