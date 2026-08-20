# ==============================================================================
# Script: 02_prepare_analysis_data_agent_v1.R
# Status: PLACEHOLDER — complete after pairing review
# Purpose: Build one non-area-normalized annual lnRR row per approved watershed
#          pair x analyte x post-fire year.
# Builds from: R_scripts/03_merge_geospatial.R and
#              R_scripts/04_calculate_effect_sizes.R (human scripts unchanged).
# ==============================================================================

library(tidyverse)
library(here)

# ---- 1. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")

pairing_path <- file.path(workflow_dir, "config", "pairing_decisions_reviewed.csv")
daily_path <- file.path(workflow_dir, "data", "source", "01_daily_time_series_paired.csv")
geospatial_path <- file.path(workflow_dir, "data", "source", "geospatial_variables_bp_severity_pull.csv")
metadata_path <- file.path(workflow_dir, "data", "source", "Sites_meta_data.csv")
qc_effect_size_path <- file.path(workflow_dir, "data", "source", "effect_sizes_yearly.csv")
output_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")

# ---- 2. Block analysis until pair review is final ---------------------------

if (!file.exists(pairing_path)) {
  stop(
    "Pair review is incomplete. Run 01_pairing_review_workbook_agent_v1.R ",
    "in import mode to create pairing_decisions_reviewed.csv."
  )
}

pairing_decisions <- read_csv(pairing_path, show_col_types = FALSE)

required_pair_fields <- c(
  "Study_ID", "Comparison_ID", "Pair_Burn", "Pair_Unburn",
  "Fire_ID_Final", "Pairing_Type_Final", "Include", "Decision_Status"
)

stopifnot(all(required_pair_fields %in% names(pairing_decisions)))

unresolved_pairs <- pairing_decisions %>%
  mutate(include_value = str_to_upper(as.character(Include)) == "TRUE") %>%
  filter(
    is.na(Include) |
      !Decision_Status %in% c("approved", "excluded") |
      (include_value & (is.na(Fire_ID_Final) | is.na(Pairing_Type_Final)))
  )

if (nrow(unresolved_pairs) > 0) {
  stop(nrow(unresolved_pairs), " pairing decisions remain unresolved.")
}

# This explicit stop prevents a partially implemented effect-size calculation
# from being mistaken for the final model table.
stop(
  "PLACEHOLDER: pairing gate passed. Complete the TODO sections below before running."
)

# ---- 3. Read immutable source snapshots ------------------------------------

# TODO: Read the daily chemistry, geospatial, metadata, and QC effect-size files.
# TODO: Assert expected columns and preserve original identifiers.

# ---- 4. Apply approved watershed pairs -------------------------------------

# TODO: Keep Include == TRUE only.
# TODO: Map Pair_Burn and Pair_Unburn to the daily source's site columns.
# TODO: Retain Fire_ID_Final, Pairing_Type_Final, and shared_control_id.
# TODO: Never regenerate a burned x reference cross-product here.

# ---- 5. Calculate daily paired responses -----------------------------------

# TODO: For DOC and nitrate separately, calculate:
#       lnRR = log(concentration_burned / concentration_reference)
# TODO: Keep indicators for observed versus interpolated concentrations.
# TODO: Do not calculate area-normalized effect sizes.

# ---- 6. Aggregate by post-fire year ----------------------------------------

# TODO: Resolve the temporal definition recorded in .codex/decisions.md.
# TODO: Produce pair x analyte x post-fire-year means, SDs, n, and candidate
#       sampling variances. Preserve calendar year as a descriptive field.

# ---- 7. Join burned-watershed predictors -----------------------------------

# TODO: Join geospatial attributes to the burned watershed only.
# TODO: Document the manual Gaviota COMID correction.
# TODO: Keep raw predictor values; transformations belong inside model folds.

# ---- 8. Quality checks and export ------------------------------------------

# TODO: Check uniqueness at pair x analyte x post-fire year.
# TODO: Compare rebuilt lnRR values with effect_sizes_yearly.csv for QC only.
# TODO: Write data/derived/lasso_model_table.csv and a QC comparison table.
