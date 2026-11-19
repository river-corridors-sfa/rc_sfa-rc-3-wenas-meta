# Run from the repository root.
library(dplyr)
library(readr)

here::i_am("agent_workflows/vibe_coding/R/rebuild_18_pairs_from_source.R")
workflow <- here::here("agent_workflows", "vibe_coding")
derived <- file.path(workflow, "data/derived")
audit <- file.path(workflow, "data/audit")
dir.create(audit, recursive = TRUE, showWarnings = FALSE)

steps <- c(
  "01_read_and_gapfill_preserve_observations_agent_v1.R",
  "02_pairing_review_workbook_agent_v1.R",
  "03_promote_reviewed_pairings_agent_v1.R",
  "04_apply_reviewed_sites_agent_v1.R",
  "05_prepare_analysis_data_agent_v1.R",
  "07_prepare_first_water_year_agent_v1.R"
)
for (step in steps) {
  message("Running ", step)
  source(file.path(workflow, "R/preparation", step), local = new.env(parent = globalenv()))
}

# Feed freshly prepared inputs into the same R rebuild used by the portable bundle.
rebuilt <- file.path(derived, "rebuild_18_pairs")
inputs <- file.path(rebuilt, "inputs")
dir.create(inputs, recursive = TRUE, showWarnings = FALSE)

pairs <- read_csv(file.path(audit, "paper_first_water_year_coverage.csv"), show_col_types = FALSE) %>%
  transmute(candidate_pair_id, Study_ID, Comparison_ID, Pair_Burn, Pair_Unburn,
            shared_control_id, Fire_ID_Analysis, fire_water_year, Site_Burn,
            Site_Reference = Site_Unburn) %>%
  distinct()
calendar_model <- read_csv(file.path(derived, "lasso_model_table.csv"), show_col_types = FALSE)
predictors <- calendar_model %>%
  semi_join(pairs, by = "candidate_pair_id") %>%
  select(candidate_pair_id, Area_watershed_km:burn_sev_low) %>%
  distinct()
daily <- read_csv(file.path(derived, "reviewed_sites_agent_v1/effect_sizes_daily.csv"),
                  show_col_types = FALSE) %>%
  semi_join(pairs, by = "candidate_pair_id") %>%
  select(candidate_pair_id, Sampling_Date, response_var, Site_Burn, Site_Unburn,
         burn, reference, burn_observed, reference_observed, valid, year)
exclusions <- read_csv(file.path(workflow, "config/paper_record_exclusions.csv"),
                       show_col_types = FALSE) %>%
  semi_join(pairs, by = "candidate_pair_id")

write_csv(pairs, file.path(inputs, "pairs.csv"), na = "")
write_csv(predictors, file.path(inputs, "watershed_predictors.csv"), na = "")
write_csv(daily, file.path(inputs, "daily_paired_chemistry.csv"), na = "")
write_csv(exclusions, file.path(inputs, "calendar_year_exclusions.csv"), na = "")

rebuild_script <- file.path(rebuilt, "rebuild_inputs.R")
stopifnot(file.copy(file.path(workflow, "rebuild_inputs.R"), rebuild_script, overwrite = TRUE))
status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(rebuild_script))
if (status != 0L) stop("The standalone water-year rebuild failed.")
message("Source rebuild complete. Model inputs: ", file.path(rebuilt, "model_inputs"))
