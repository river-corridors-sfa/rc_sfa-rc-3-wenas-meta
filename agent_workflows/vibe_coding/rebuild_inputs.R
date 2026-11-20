# Run with Rscript or source() to rebuild the model inputs.
library(dplyr)
library(readr)
library(tidyr)

# Prefer the sourced file over the path of an outer Rscript driver.
sourced_files <- as.character(unlist(lapply(sys.frames(), function(frame) frame$ofile)))
command_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
# Rscript escapes spaces in the --file value on some platforms.
script_paths <- c(rev(sourced_files), gsub("~+~", " ", command_file, fixed = TRUE))
script_paths <- script_paths[basename(script_paths) == "rebuild_inputs.R" & file.exists(script_paths)]
if (length(script_paths)) {
  folder <- dirname(normalizePath(script_paths[1], mustWork = TRUE))
} else {
  folder <- getwd()
  repository_folder <- file.path(folder, "agent_workflows", "vibe_coding")
  if (file.exists(file.path(repository_folder, "rebuild_inputs.R"))) folder <- repository_folder
}
inputs <- file.path(folder, "inputs")
output <- file.path(folder, "model_inputs")

input_files <- c(pairs = "pairs.csv", predictors = "watershed_predictors.csv",
                 daily = "daily_paired_chemistry.csv", exclusions = "calendar_year_exclusions.csv")
input_paths <- setNames(file.path(inputs, input_files), names(input_files))
missing <- input_paths[!file.exists(input_paths)]
if (length(missing)) {
  stop("Missing prepared CSVs in ", inputs, ": ", paste(basename(missing), collapse = ", "),
       call. = FALSE)
}
pairs <- read_csv(input_paths[["pairs"]], show_col_types = FALSE)
predictors <- read_csv(input_paths[["predictors"]], show_col_types = FALSE)
daily <- read_csv(input_paths[["daily"]], show_col_types = FALSE)
exclusions <- read_csv(input_paths[["exclusions"]], show_col_types = FALSE)

analytes <- c("DOC", "NO3")
pair_keys <- c("candidate_pair_id", "response_var")
scenarios <- c("first_water_year", "first_water_year_observed_only")
stopifnot(
  !anyNA(pairs$fire_water_year),
  !anyDuplicated(pairs$candidate_pair_id),
  !anyDuplicated(predictors$candidate_pair_id),
  setequal(pairs$candidate_pair_id, predictors$candidate_pair_id),
  all(daily$response_var %in% analytes),
  !anyDuplicated(daily[c(pair_keys, "Sampling_Date")]),
  all(daily$year == as.integer(format(daily$Sampling_Date, "%Y")))
)

selected <- daily %>%
  inner_join(select(pairs, candidate_pair_id, fire_water_year),
             by = "candidate_pair_id", relationship = "many-to-one",
             unmatched = c("error", "drop")) %>%
  # The exclusion is keyed to the original calendar year.
  anti_join(exclusions, by = c(pair_keys, "year")) %>%
  mutate(water_year = year + as.integer(format(Sampling_Date, "%m") >= "10"),
         directly_observed = burn_observed & reference_observed) %>%
  filter(valid, water_year == fire_water_year + 1L)
stopifnot(all(is.finite(selected$burn) & selected$burn > 0),
          all(is.finite(selected$reference) & selected$reference > 0),
          !anyNA(selected$directly_observed))

date_counts <- selected %>%
  group_by(candidate_pair_id, response_var) %>%
  summarise(n_valid_paired_dates = n(), n_observed_paired_dates = sum(directly_observed),
            .groups = "drop")
coverage <- expand_grid(pairs, response_var = analytes) %>%
  left_join(date_counts, by = pair_keys, relationship = "one-to-one") %>%
  mutate(year = fire_water_year + 1L,
         across(c(n_valid_paired_dates, n_observed_paired_dates), ~ replace_na(.x, 0L)),
         included = n_valid_paired_dates >= 2L,
         observed_only_eligible = n_observed_paired_dates >= 2L) %>%
  relocate(year, .after = response_var)

model <- bind_rows(
  mutate(selected, scenario = scenarios[1]),
  mutate(filter(selected, directly_observed), scenario = scenarios[2])
) %>%
  group_by(scenario, candidate_pair_id, response_var, year = water_year) %>%
  filter(n() >= 2L) %>%
  summarise(
    first_matched_date = min(Sampling_Date),
    last_matched_date = max(Sampling_Date),
    lnRR_n = n(),
    n_both_observed = sum(directly_observed),
    n_with_interpolation = sum(!directly_observed),
    annual_mean_burn = mean(burn),
    annual_mean_reference = mean(reference),
    lnRR_mean = log(annual_mean_burn / annual_mean_reference),
    lnRR_var = var(burn / annual_mean_burn - reference / annual_mean_reference) / lnRR_n,
    .groups = "drop"
  ) %>%
  left_join(pairs, by = "candidate_pair_id", relationship = "many-to-one") %>%
  left_join(predictors, by = "candidate_pair_id", relationship = "many-to-one") %>%
  mutate(
    water_year_start = as.Date(sprintf("%04d-10-01", year - 1L)),
    water_year_end = as.Date(sprintf("%04d-09-30", year)),
    post_fire_year = year - fire_water_year,
    effect_size_definition = "log_ratio_annual_arithmetic_means",
    variance_method = "paired_delta_daily_independence_provisional",
    shared_control_year_id = paste(shared_control_id, response_var, year, sep = " | "),
    n_pairs_sharing_reference = 1L,
    reference_family_weight = 1
  ) %>%
  group_by(scenario, candidate_pair_id) %>%
  mutate(matched_doc_no3 = n() == 2L) %>%
  ungroup() %>%
  arrange(response_var, Study_ID, Comparison_ID)
stopifnot(setequal(model$scenario, scenarios))

dir.create(output, showWarnings = FALSE)
write_csv(coverage, file.path(output, "coverage.csv"), na = "")
for (output_scenario in scenarios) {
  rows <- model %>%
    filter(scenario == output_scenario) %>%
    select(all_of(names(pairs)), all_of(setdiff(names(predictors), "candidate_pair_id")),
           response_var, year, water_year_start, water_year_end, post_fire_year,
           first_matched_date, last_matched_date, lnRR_n, n_both_observed,
           n_with_interpolation, annual_mean_burn, annual_mean_reference, lnRR_mean,
           lnRR_var, effect_size_definition, variance_method, shared_control_year_id,
           n_pairs_sharing_reference, reference_family_weight, matched_doc_no3)
  write_csv(rows, file.path(output, paste0(output_scenario, ".csv")), na = "")
}
print(count(model, scenario, response_var, name = "effect_sizes"))
