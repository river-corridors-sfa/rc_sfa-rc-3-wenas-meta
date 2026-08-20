# ==============================================================================
# Script: 04_fit_meta_analysis_agent_v1.R
# Purpose: Fit provisional multilevel meta-analysis models for annual DOC and
#          nitrate lnRR, with study-clustered robust inference when available.
# Builds from: R_scripts/05_three_level_meta-analysis_models.R (unchanged).
# ==============================================================================

library(tidyverse)
library(here)
library(metafor)

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
table_dir <- file.path(workflow_dir, "output", "tables")
model_dir <- file.path(workflow_dir, "output", "models")
log_dir <- file.path(workflow_dir, "output", "logs")

if (!file.exists(model_table_path)) stop("Run scripts 02 and 03 first.")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

model_table <- read_csv(model_table_path, show_col_types = FALSE) %>%
  filter(is.finite(lnRR_mean), is.finite(lnRR_var), lnRR_var > 0, lnRR_n >= 2) %>%
  mutate(
    Study_ID = factor(Study_ID),
    Comparison_ID = factor(Comparison_ID),
    pair_key = factor(pair_key),
    shared_control_id = factor(shared_control_id),
    lnRR_var_family_adjusted = lnRR_var * pmax(n_pairs_sharing_reference, 1)
  )

if (nrow(model_table) == 0) stop("No rows have usable lnRR values and variances.")

fit_meta_model <- function(data, variance_column, include_time = FALSE) {
  model_formula <- if (include_time) ~ post_fire_year else ~ 1
  rma.mv(
    yi = lnRR_mean,
    V = data[[variance_column]],
    mods = model_formula,
    random = list(~ 1 | Study_ID, ~ 1 | pair_key, ~ 1 | shared_control_id),
    data = data,
    method = "REML",
    test = "t",
    sparse = TRUE
  )
}

tidy_meta_model <- function(model, robust_model, analyte, model_name, variance_approach) {
  reported_model <- if (is.null(robust_model)) model else robust_model
  term_names <- rownames(reported_model$beta)
  if (is.null(term_names)) term_names <- paste0("term_", seq_along(reported_model$beta))
  tibble(
    response_var = analyte,
    model = model_name,
    variance_approach = variance_approach,
    inference = if_else(is.null(robust_model), "model_based", "study_cluster_robust"),
    term = term_names,
    estimate = as.numeric(reported_model$beta),
    std_error = as.numeric(reported_model$se),
    ci_lower = as.numeric(reported_model$ci.lb),
    ci_upper = as.numeric(reported_model$ci.ub),
    p_value = as.numeric(reported_model$pval),
    k = model$k,
    n_studies = n_distinct(model$data$Study_ID),
    percent_change = 100 * (exp(as.numeric(reported_model$beta)) - 1)
  )
}

model_objects <- list()
model_summaries <- list()
model_failures <- list()

for (analyte in sort(unique(model_table$response_var))) {
  analyte_data <- model_table %>% filter(response_var == analyte)

  specifications <- tribble(
    ~model_name, ~variance_column, ~include_time,
    "intercept_only", "lnRR_var", FALSE,
    "intercept_only_family_adjusted", "lnRR_var_family_adjusted", FALSE,
    "time", "lnRR_var", TRUE,
    "time_family_adjusted", "lnRR_var_family_adjusted", TRUE
  )

  for (row_index in seq_len(nrow(specifications))) {
    specification <- specifications[row_index, ]
    fitting_data <- analyte_data

    if (specification$include_time) {
      fitting_data <- fitting_data %>% filter(is.finite(post_fire_year))
      if (n_distinct(fitting_data$post_fire_year) < 3) next
    }

    model_key <- paste(analyte, specification$model_name, sep = "__")
    fitted_model <- tryCatch(
      fit_meta_model(
        fitting_data,
        specification$variance_column,
        specification$include_time
      ),
      error = function(error) error
    )

    if (inherits(fitted_model, "error")) {
      model_failures[[model_key]] <- tibble(
        response_var = analyte,
        model = specification$model_name,
        error = conditionMessage(fitted_model)
      )
      next
    }

    robust_model <- NULL
    if (requireNamespace("clubSandwich", quietly = TRUE) &&
        n_distinct(fitting_data$Study_ID) >= 4) {
      robust_model <- tryCatch(
        metafor::robust(
          fitted_model,
          cluster = fitting_data$Study_ID,
          clubSandwich = TRUE
        ),
        error = function(error) NULL
      )
    }

    model_objects[[model_key]] <- fitted_model
    model_summaries[[model_key]] <- tidy_meta_model(
      fitted_model,
      robust_model,
      analyte,
      specification$model_name,
      specification$variance_column
    )
  }
}

meta_model_summary <- bind_rows(model_summaries)
meta_model_failures <- bind_rows(model_failures)

write_csv(meta_model_summary, file.path(table_dir, "meta_model_summary.csv"))
write_csv(meta_model_failures, file.path(log_dir, "meta_model_failures.csv"))
saveRDS(model_objects, file.path(model_dir, "meta_models.rds"))

message("Fitted ", length(model_objects), " meta-analysis models.")
message("Shared-reference family-adjusted models are sensitivity analyses, not exact covariance models.")

