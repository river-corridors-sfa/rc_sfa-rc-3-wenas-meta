# ==============================================================================
# Script: 07_make_results_agent_v1.R
# Status: PLACEHOLDER — manuscript outputs only
# Purpose: Create final tables and figures from saved model outputs.
# Builds from: plotting conventions in the existing human analysis scripts;
#              it does not refit models or recalculate effect sizes.
# ==============================================================================

library(tidyverse)
library(here)

# ---- 1. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")
audit_dir <- file.path(workflow_dir, "data", "audit")
model_dir <- file.path(workflow_dir, "output", "models")
table_dir <- file.path(workflow_dir, "output", "tables")
figure_dir <- file.path(workflow_dir, "output", "figures")

stop("PLACEHOLDER: run finalized scripts 02 through 06 before making results.")

# ---- 2. Dataset and pair-structure table -----------------------------------

# TODO: Report observations, years, pairs, comparisons/fires, studies,
#       shared controls, and interpolation coverage separately by analyte.

# ---- 3. Meta-analysis outputs ----------------------------------------------

# TODO: Plot pooled DOC and nitrate lnRR and temporal recovery with uncertainty.
# TODO: Create a concise heterogeneity and model-diagnostics table.

# ---- 4. Predictive-performance outputs -------------------------------------

# TODO: Compare held-out RMSE, MAE, and R2 for intercept-only, time-only,
#       time-plus-fire, and full LASSO models.

# ---- 5. Stability and sensitivity outputs ----------------------------------

# TODO: Plot selection frequency with standardized coefficient distributions.
# TODO: Clearly distinguish stable, conditional, and weak descriptive categories.
# TODO: Summarize elastic-net, weighting, shared-control, interpolation,
#       matched-subset, and influential-study sensitivities.

# ---- 6. Export rules --------------------------------------------------------

# TODO: Use explicit filenames, dimensions, units, and 300+ dpi for figures.
# TODO: Write source data for every manuscript figure as CSV.
# TODO: Do not use clipboard output or regenerate historical normalization figures.

