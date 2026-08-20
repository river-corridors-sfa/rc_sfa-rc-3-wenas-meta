# ==============================================================================
# Script: source_all_analysis_scripts_agent_v1.R
# Purpose: Lightweight helper to re-run the provisional agent analysis workflow
#          by sourcing the analysis scripts in dependency order.
#
# Notes:
# - This helper does not generate the Markdown run report. Use
#   run_all_agent_v1.R when a full report with diagnostics is needed.
# - The pairing inventory and review workbook scripts are off by default because
#   they are setup/review utilities, not routine analysis rerun steps.
# ==============================================================================

library(here)

source_pairing_inventory <- FALSE
source_review_workbook <- FALSE

required_packages <- c(
  "tidyverse", "here", "lubridate", "metafor", "glmnet", "forcats"
)

if (source_review_workbook) {
  required_packages <- c(required_packages, "openxlsx")
}

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

script_dir <- here("agent_workflows", "vibe_coding", "R")

analysis_scripts <- c(
  "01a_create_provisional_pairings_agent_v1.R",
  "02_prepare_analysis_data_agent_v1.R",
  "03_audit_pairs_and_predictors_agent_v1.R",
  "04_fit_meta_analysis_agent_v1.R",
  "05_fit_grouped_lasso_agent_v1.R",
  "06_run_stability_sensitivity_agent_v1.R",
  "07_make_results_agent_v1.R"
)

scripts_to_source <- c(
  if (source_pairing_inventory) "00_build_pairing_decisions_agent_v1.R",
  if (source_review_workbook) "01_pairing_review_workbook_agent_v1.R",
  analysis_scripts
)

missing_scripts <- scripts_to_source[
  !file.exists(file.path(script_dir, scripts_to_source))
]

if (length(missing_scripts) > 0) {
  stop("Missing workflow scripts: ", paste(missing_scripts, collapse = ", "))
}

message("Sourcing ", length(scripts_to_source), " workflow scripts from: ", script_dir)
message("N_BOOTSTRAP = ", Sys.getenv("N_BOOTSTRAP", unset = "100"))

for (script_name in scripts_to_source) {
  script_path <- file.path(script_dir, script_name)
  message("\n===== Sourcing ", script_name, " =====")
  source(script_path, local = new.env(parent = globalenv()))
}

message("\nAnalysis workflow complete.")
