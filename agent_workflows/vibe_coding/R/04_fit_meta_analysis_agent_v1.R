# ==============================================================================
# Script: 04_fit_meta_analysis_agent_v1.R
# Status: PLACEHOLDER — requires approved pairings and a finalized variance plan
# Purpose: Estimate pooled wildfire effects, temporal recovery, and heterogeneity.
# Builds from: non-normalized portions of
#              R_scripts/05_three_level_meta-analysis_models.R.
# ==============================================================================

library(tidyverse)
library(here)
library(metafor)

# ---- 1. Paths and prerequisites --------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
table_dir <- file.path(workflow_dir, "output", "tables")
model_dir <- file.path(workflow_dir, "output", "models")

if (!file.exists(model_table_path)) {
  stop("Run scripts 02 and 03 before fitting meta-analysis models.")
}

stop(
  "PLACEHOLDER: finalize effect-size variance and shared-control handling before fitting."
)

# ---- 2. Read final non-normalized annual effect sizes -----------------------

# TODO: Read annual lnRR, sampling variance/weight, post-fire year, analyte,
#       Study_ID, Fire_ID_Final, Comparison_ID, candidate_pair_id, and
#       shared_control_id.
# TODO: Exclude area-normalized response columns from the active analysis.

# ---- 3. Fit separate DOC and nitrate models --------------------------------

# TODO: Fit intercept-only multilevel models with study, comparison/fire, and
#       watershed-pair dependence represented at defensible levels.
# TODO: Fit prespecified time-since-fire models.
# TODO: Use study-clustered robust inference where supported.
# TODO: Do not treat repeated annual rows or shared references as independent.

# Candidate structure to evaluate after the pair audit:
# rma.mv(
#   yi = lnRR_mean,
#   V = lnRR_var,
#   mods = ~ post_fire_year,
#   random = ~ 1 | Study_ID / Comparison_ID / candidate_pair_id,
#   data = analyte_data,
#   method = "REML"
# )

# ---- 4. Diagnostics and summaries ------------------------------------------

# TODO: Report pooled lnRR and percent change, confidence intervals,
#       heterogeneity components, influence diagnostics, and residual checks.
# TODO: Save model objects as RDS and tidy summaries as CSV.

