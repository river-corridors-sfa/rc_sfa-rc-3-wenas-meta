# Rebuild the paper LASSOs and five figures from the prepared model table.
# Set PAPER_FROM_SOURCE=1 to rebuild reviewed annual data first.
# Set N_BOOTSTRAP=1000 for the manuscript stability run (the model defaults to 1000).
library(here)

script_dir <- here("agent_workflows", "vibe_coding", "R")
from_source <- Sys.getenv("PAPER_FROM_SOURCE", "0") == "1"
steps <- c(
  if (from_source) c(
    "01_read_and_gapfill_preserve_observations_agent_v1.R",
    "01b_promote_reviewed_pairings_agent_v1.R",
    "01c_apply_reviewed_sites_agent_v1.R",
    "02_prepare_analysis_data_agent_v1.R",
    "03_audit_pairs_and_predictors_agent_v1.R"
  ),
  "11_fit_issue6_revised_primary_agent_v1.R",
  "13_plot_paper_maps_agent_v1.R",
  "08_plot_response_boxplots_agent_v1.R",
  "14_plot_paper_lasso_results_agent_v1.R",
  "15_plot_paper_predictor_correlations_agent_v1.R",
  "12_plot_issue6_revised_primary_violins_agent_v1.R"
)

for (step in steps) {
  path <- file.path(script_dir, step)
  if (!file.exists(path)) stop("Missing workflow script: ", path)
  message("Running ", step)
  source(path, local = new.env(parent = globalenv()))
}
message("Paper workflow complete: agent_workflows/vibe_coding/output/paper/figures")
