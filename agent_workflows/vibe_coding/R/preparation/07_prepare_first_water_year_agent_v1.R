library(dplyr)
library(here)
library(lubridate)
library(readr)
library(tidyr)

workflow <- here("agent_workflows", "vibe_coding")
selection <- read_csv(file.path(workflow, "config/paper_first_water_year_pairs.csv"),
                      show_col_types = FALSE)
pairings <- read_csv(file.path(workflow, "config/pairing_decisions_analysis.csv"),
                     show_col_types = FALSE)
sites <- read_csv(file.path(workflow, "config/reviewed_site_selection.csv"),
                  show_col_types = FALSE)
daily <- read_csv(file.path(workflow, "data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv"),
                  show_col_types = FALSE)
exclusions <- read_csv(file.path(workflow, "config/paper_record_exclusions.csv"),
                        show_col_types = FALSE)
calendar_model <- read_csv(file.path(workflow, "data/derived/lasso_model_table.csv"),
                           show_col_types = FALSE)

pairs <- selection %>%
  inner_join(filter(pairings, Include_Analysis, Analysis_Decision_Status == "confirmed"),
             by = "Comparison_ID", relationship = "one-to-one", unmatched = c("error", "drop")) %>%
  select(Study_ID, Comparison_ID, candidate_pair_id, Pair_Burn, Pair_Unburn,
         shared_control_id, Fire_ID_Analysis, fire_water_year)
stopifnot(!anyNA(pairs$fire_water_year), !anyDuplicated(pairs$shared_control_id))

site_roles <- sites %>%
  filter(Include_Site) %>%
  semi_join(pairs, by = "candidate_pair_id") %>%
  mutate(Site = if_else(Study_ID == "Uzun et al. 2020",
                        sub(" (First Rainy Season|Second Rainy Season|Subsequent Flush).*$", "", Site),
                        Site)) %>%
  distinct(Study_ID, candidate_pair_id, Burn_Unburn, Site)
# Check physical-site labels independently of the workbook's control identifiers.
stopifnot(
  nrow(site_roles) == 2 * nrow(pairs),
  !anyDuplicated(site_roles[c("candidate_pair_id", "Burn_Unburn")]),
  !anyDuplicated(site_roles[c("Study_ID", "Site")])
)
site_names <- site_roles %>%
  select(-Study_ID) %>%
  pivot_wider(names_from = Burn_Unburn, values_from = Site, names_prefix = "Site_")
pairs <- pairs %>%
  left_join(site_names, by = "candidate_pair_id", relationship = "one-to-one")

selected_days <- daily %>%
  filter(valid) %>%
  inner_join(select(pairs, candidate_pair_id, fire_water_year),
             by = "candidate_pair_id", relationship = "many-to-one") %>%
  # Apply the existing calendar-key exclusion before rebucketing dates.
  anti_join(exclusions, by = c("candidate_pair_id", "response_var", "year")) %>%
  mutate(water_year = year + as.integer(month(Sampling_Date) >= 10L)) %>%
  filter(water_year == fire_water_year + 1L)
stopifnot(!anyDuplicated(selected_days[c("candidate_pair_id", "response_var", "Sampling_Date")]))

annual <- selected_days %>%
  group_by(candidate_pair_id, response_var, year = water_year) %>%
  summarise(
    first_matched_date = min(Sampling_Date),
    last_matched_date = max(Sampling_Date),
    n_both_observed = sum(burn_observed & reference_observed),
    n_with_interpolation = sum(!burn_observed | !reference_observed),
    lnRR_n = n(),
    annual_mean_burn = mean(burn),
    annual_mean_reference = mean(reference),
    lnRR_mean = log(annual_mean_burn / annual_mean_reference),
    lnRR_var = var(burn / annual_mean_burn - reference / annual_mean_reference) / lnRR_n,
    .groups = "drop"
  )

# These watershed and wildfire covariates do not depend on the sampling window.
# The one-row-per-pair check catches changes to that assumption upstream.
covariates <- calendar_model %>%
  semi_join(pairs, by = "candidate_pair_id") %>%
  distinct(candidate_pair_id, Area_watershed_km, maxelevsmo, slope, tmean8110ws,
           precip8110ws, forest_cover, urban_cover, grassland_cover, wetland_cover,
           ag_cover, omws, rckdepws, clayws, glacial_till, bfiws, permws, runoffws,
           burn_percent_fire_year, burn_sev_high, burn_sev_mod, burn_sev_low)
stopifnot(nrow(covariates) == nrow(pairs), !anyDuplicated(covariates$candidate_pair_id))

model <- annual %>%
  filter(lnRR_n >= 2L) %>%
  left_join(pairs, by = "candidate_pair_id", relationship = "many-to-one") %>%
  left_join(covariates, by = "candidate_pair_id", relationship = "many-to-one") %>%
  rename(Site_Reference = Site_Unburn) %>%
  mutate(
    water_year_start = make_date(year - 1L, 10L, 1L),
    water_year_end = make_date(year, 9L, 30L),
    post_fire_year = year - fire_water_year,
    shared_control_year_id = paste(shared_control_id, response_var, year, sep = " | "),
    n_pairs_sharing_reference = 1L,
    reference_family_weight = 1,
    effect_size_definition = "log_ratio_annual_arithmetic_means",
    variance_method = "paired_delta_daily_independence_provisional"
  ) %>%
  group_by(candidate_pair_id) %>%
  mutate(matched_doc_no3 = n() == 2L) %>%
  ungroup() %>%
  arrange(response_var, Study_ID, Comparison_ID)
stopifnot(all(model$post_fire_year == 1L),
          !anyDuplicated(model[c("candidate_pair_id", "response_var")]))

coverage <- expand_grid(candidate_pair_id = pairs$candidate_pair_id,
                        response_var = c("DOC", "NO3")) %>%
  left_join(pairs, by = "candidate_pair_id", relationship = "many-to-one") %>%
  left_join(select(annual, candidate_pair_id, response_var, lnRR_n, n_both_observed,
                   first_matched_date, last_matched_date),
            by = c("candidate_pair_id", "response_var"), relationship = "one-to-one") %>%
  mutate(across(c(lnRR_n, n_both_observed), ~ replace_na(.x, 0L)),
         included = lnRR_n >= 2L,
         observed_only_eligible = n_both_observed >= 2L,
         exclusion_reason = case_when(
           lnRR_n == 0L ~ "No eligible paired dates in first post-fire water year",
           lnRR_n == 1L ~ "Only one eligible paired date in first post-fire water year"
         ))

write_csv(model, file.path(workflow, "data/derived/paper_first_water_year_model_table.csv"), na = "")
write_csv(coverage, file.path(workflow, "data/audit/paper_first_water_year_coverage.csv"), na = "")
print(model %>% group_by(response_var) %>%
        summarise(effect_sizes = n(), studies = n_distinct(Study_ID),
                  fires = n_distinct(Fire_ID_Analysis), .groups = "drop"))
