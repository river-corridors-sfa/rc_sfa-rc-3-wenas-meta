# ==============================================================================
# Script: run_all_agent_v1.R
# Purpose: Run the provisional agent analysis workflow from pair adoption through
#          manuscript-facing outputs.
# ==============================================================================

library(here)

required_packages <- c(
  "tidyverse", "here", "lubridate", "metafor", "glmnet", "forcats"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

scripts <- c(
  "01a_create_provisional_pairings_agent_v1.R",
  "02_prepare_analysis_data_agent_v1.R",
  "03_audit_pairs_and_predictors_agent_v1.R",
  "04_fit_meta_analysis_agent_v1.R",
  "05_fit_grouped_lasso_agent_v1.R",
  "06_run_stability_sensitivity_agent_v1.R",
  "07_make_results_agent_v1.R"
)

for (script_name in scripts) {
  message("\n===== Running ", script_name, " =====")
  source(
    here("agent_workflows", "vibe_coding", "R", script_name),
    local = new.env(parent = globalenv())
  )
}

message("\nWorkflow completed.")

