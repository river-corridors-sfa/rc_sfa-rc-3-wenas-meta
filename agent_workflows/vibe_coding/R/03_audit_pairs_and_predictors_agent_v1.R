# ==============================================================================
# Script: 03_audit_pairs_and_predictors_agent_v1.R
# Status: PLACEHOLDER — requires the model table from script 02
# Purpose: Audit pair structure, dependence, missingness, interpolation, and
#          predictor redundancy before final predictor decisions.
# Builds from: diagnostic portions of R_scripts/03.1_variable_selection_for_LASSO.R.
# ==============================================================================

library(tidyverse)
library(here)

# ---- 1. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
predictor_dictionary_path <- file.path(workflow_dir, "config", "predictor_dictionary.csv")
audit_dir <- file.path(workflow_dir, "data", "audit")

if (!file.exists(model_table_path)) {
  stop("Run 02_prepare_analysis_data_agent_v1.R after pairing review.")
}

stop("PLACEHOLDER: complete the TODO sections below before running the audit.")

# ---- 2. Read and validate the annual model table ----------------------------

# TODO: Confirm one row per Study_ID x Fire_ID_Final x Comparison_ID x
#       candidate_pair_id x response_var x post_fire_year.
# TODO: Report counts of studies, fires, comparisons, pairs, and years by analyte.

# ---- 3. Audit dependence and pair construction ------------------------------

# TODO: Summarize shared references and composite references.
# TODO: Show study -> fire/comparison -> pair -> year structure.
# TODO: Flag any pair IDs that occur under multiple studies or fires.

# ---- 4. Audit response and interpolation -----------------------------------

# TODO: Summarize lnRR, variance, sample size, and interpolation proportion.
# TODO: Compare complete annual summaries with observed-only availability.
# TODO: Flag implausible or infinite lnRR values caused by zero concentrations.

# ---- 5. Audit candidate predictors -----------------------------------------

# Candidate names below come from the human variable-screening script and are
# starting points only; they are not an approved final predictor set.
candidate_predictors <- c(
  "burn_percent_fire_year", "burn_sev_high", "AI_longterm", "runoffws",
  "bfiws", "permws", "rckdepws", "forest_cover", "grassland_cover",
  "wetland_cover", "ag_cover", "urban_cover", "clayws", "omws",
  "Area_watershed_km"
)

# TODO: Reconcile these names with the final model-table schema.
# TODO: Report missingness, ranges, unique values, skewness, and analyte coverage.
# TODO: Produce Spearman correlations and identify near-redundancy (e.g., |r| > .90).
# TODO: Use PCA only as a descriptive covariance diagnostic, never as the rule
#       for selecting the final predictive model.

# ---- 6. Export review materials --------------------------------------------

# TODO: Create data/audit/pair_structure.csv.
# TODO: Create data/audit/predictor_missingness.csv.
# TODO: Create data/audit/interpolation_summary.csv.
# TODO: Create data/audit/predictor_correlations.csv.
# TODO: Create config/predictor_dictionary.csv with author-review fields for
#       scientific role, transformation, primary inclusion, and exclusion reason.

