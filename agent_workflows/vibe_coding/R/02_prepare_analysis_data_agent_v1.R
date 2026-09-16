# ==============================================================================
# Script: 02_prepare_analysis_data_agent_v1.R
# Purpose: Build the provisional annual lnRR modeling table from the established
#          pair inventory, annual effect sizes, and burned-watershed predictors.
# Builds from: R_scripts/03_merge_geospatial.R and
#              R_scripts/04_calculate_effect_sizes.R (unchanged).
# ==============================================================================

library(tidyverse)
library(here)
library(lubridate)

workflow_dir <- here("agent_workflows", "vibe_coding")
pairing_path <- file.path(workflow_dir, "config", "pairing_decisions_analysis.csv")
daily_path <- file.path(workflow_dir, "data", "source", "01_daily_time_series_paired.csv")
geospatial_path <- file.path(workflow_dir, "data", "source", "geospatial_variables_bp_severity_pull.csv")
effect_size_path <- file.path(workflow_dir, "data", "source", "effect_sizes_yearly.csv")
derived_dir <- file.path(workflow_dir, "data", "derived")
audit_dir <- file.path(workflow_dir, "data", "audit")
output_path <- file.path(derived_dir, "lasso_model_table.csv")

required_files <- c(pairing_path, daily_path, geospatial_path, effect_size_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) stop("Missing required files: ", paste(missing_files, collapse = ", "))

dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

pairing_decisions <- read_csv(pairing_path, show_col_types = FALSE)
daily_data <- read_csv(daily_path, show_col_types = FALSE, na = c("", "NA", "N/A", "-9999"))
geospatial_data <- read_csv(geospatial_path, show_col_types = FALSE, na = c("", "NA", "N/A", "-9999"))
annual_effect_sizes <- read_csv(effect_size_path, show_col_types = FALSE)

required_pair_fields <- c(
  "Study_ID", "Comparison_ID", "Pair_Burn", "Pair_Unburn",
  "candidate_pair_id", "shared_control_id", "n_pairs_sharing_reference",
  "Fire_ID_Analysis", "Pairing_Type_Analysis", "Include_Analysis",
  "Analysis_Decision_Status", "Coauthor_Confirmation"
)
required_daily_fields <- c(
  "Study_ID", "Comparison_ID", "Pair", "Site", "Sampling_Date",
  "Burn_Unburn", "Area_watershed_km", "Time_Since_Fire"
)
required_effect_fields <- c(
  "Study_ID", "Comparison_ID", "Pair_Burn", "Pair_Unburn",
  "response_var", "year", "lnRR_mean", "lnRR_var", "lnRR_n"
)

if (!all(required_pair_fields %in% names(pairing_decisions))) stop("Analysis pairing fields are incomplete.")
if (!all(required_daily_fields %in% names(daily_data))) stop("Daily source fields are incomplete.")
if (!all(required_effect_fields %in% names(annual_effect_sizes))) stop("Annual effect-size fields are incomplete.")

approved_pairs <- pairing_decisions %>%
  mutate(Include_Analysis = as.logical(Include_Analysis)) %>%
  filter(Include_Analysis, Analysis_Decision_Status %in% c("provisionally_approved", "confirmed"))

if (nrow(approved_pairs) == 0) stop("No analysis pairs are included.")
if (anyDuplicated(approved_pairs$candidate_pair_id)) stop("Duplicate candidate_pair_id values found.")
if (any(approved_pairs$Coauthor_Confirmation == "pending")) {
  message("Proceeding provisionally; co-author pairing confirmation remains pending.")
}

geospatial_site <- geospatial_data %>%
  rename(Site = site) %>%
  arrange(Site) %>%
  distinct(Site, .keep_all = TRUE) %>%
  select(-any_of(c("latitude", "longitude")))

site_join_audit <- tibble(
  Site = sort(unique(daily_data$Site)),
  found_in_geospatial = Site %in% geospatial_site$Site
)
write_csv(site_join_audit, file.path(audit_dir, "geospatial_site_join.csv"))

row_sum_or_na <- function(data, variables) {
  values <- data %>% select(any_of(variables))
  if (ncol(values) == 0) return(rep(NA_real_, nrow(data)))
  output <- rowSums(values, na.rm = TRUE)
  output[rowSums(!is.na(values)) == 0] <- NA_real_
  output
}

burned_daily <- daily_data %>%
  filter(Burn_Unburn == "Burn") %>%
  mutate(
    Sampling_Date = as.Date(Sampling_Date),
    year = lubridate::year(Sampling_Date),
    time_since_fire = parse_number(as.character(Time_Since_Fire))
  ) %>%
  left_join(geospatial_site, by = "Site")

burned_daily$forest_cover <- row_sum_or_na(burned_daily, c("pctconif2019ws", "pctdecid2019ws", "pctmxfst2019ws"))
burned_daily$urban_cover <- row_sum_or_na(burned_daily, c("pcturbhi2019ws", "pcturbmd2019ws", "pcturblo2019ws", "pcturbop2019ws"))
burned_daily$wetland_cover <- row_sum_or_na(burned_daily, c("pcthbwet2019ws", "pctwdwet2019ws"))
burned_daily$ag_cover <- row_sum_or_na(burned_daily, c("pctcrop2019ws", "pcthay2019ws"))
burned_daily$glacial_till <- row_sum_or_na(burned_daily, c("pctglactilloamws", "pctglactilcrsws", "pctglactilclayws"))
burned_daily$grassland_cover <- burned_daily$pctgrs2019ws

candidate_predictors <- intersect(c(
  "time_since_fire", "Area_watershed_km", "maxelevsmo", "slope",
  "tmean8110ws", "precip8110ws", "forest_cover", "urban_cover",
  "grassland_cover", "wetland_cover", "ag_cover", "omws", "rckdepws",
  "clayws", "glacial_till", "bfiws", "permws", "runoffws",
  "burn_percent_fire_year", "burn_sev_high", "burn_sev_mod", "burn_sev_low"
), names(burned_daily))

burned_annual <- burned_daily %>%
  filter(!is.na(year)) %>%
  group_by(Study_ID, Comparison_ID, Pair_Burn = Pair, year) %>%
  summarise(
    Site_Burn = first(Site),
    across(all_of(candidate_predictors), ~ if (all(is.na(.x))) NA_real_ else median(.x, na.rm = TRUE)),
    .groups = "drop"
  )

model_table <- annual_effect_sizes %>%
  select(all_of(required_effect_fields), any_of(c("lnRR_sd", "pair_key"))) %>%
  inner_join(
    approved_pairs %>%
      select(all_of(required_pair_fields), any_of(c("shared_reference", "Composite_Reference_Candidate"))),
    by = c("Study_ID", "Comparison_ID", "Pair_Burn", "Pair_Unburn")
  ) %>%
  left_join(burned_annual, by = c("Study_ID", "Comparison_ID", "Pair_Burn", "year")) %>%
  group_by(candidate_pair_id) %>%
  mutate(followup_year = year - min(year, na.rm = TRUE) + 1) %>%
  ungroup() %>%
  mutate(
    pair_key = coalesce(pair_key, candidate_pair_id),
    post_fire_year = coalesce(time_since_fire, as.numeric(followup_year)),
    shared_control_year_id = paste(shared_control_id, response_var, year, sep = " | "),
    reference_family_weight = 1 / pmax(n_pairs_sharing_reference, 1),
    effect_size_source = "established_effect_sizes_yearly_provisional",
    variance_status = if_else(is.finite(lnRR_var) & lnRR_var > 0, "usable", "missing_or_nonpositive"),
    pairing_confirmation_pending = Coauthor_Confirmation == "pending"
  ) %>%
  group_by(candidate_pair_id, year) %>%
  mutate(matched_doc_no3 = all(c("DOC", "NO3") %in% response_var)) %>%
  ungroup() %>%
  arrange(response_var, Study_ID, Comparison_ID, Pair_Burn, year)

duplicate_rows <- model_table %>% count(candidate_pair_id, response_var, year) %>% filter(n > 1)
if (nrow(duplicate_rows) > 0) stop("Model table is not unique at pair x analyte x year.")

unmatched_pairs <- approved_pairs %>%
  anti_join(model_table %>% distinct(candidate_pair_id), by = "candidate_pair_id")

write_csv(unmatched_pairs, file.path(audit_dir, "approved_pairs_without_effect_sizes.csv"))
write_csv(model_table, output_path, na = "")

message("Wrote ", nrow(model_table), " annual effect-size rows to: ", output_path)
message("Predictor columns joined: ", paste(candidate_predictors, collapse = ", "))

