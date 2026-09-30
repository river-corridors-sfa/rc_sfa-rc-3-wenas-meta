# ==============================================================================
# Script: 03_audit_pairs_and_predictors_agent_v1.R
# Purpose: Audit analysis structure and create a provisional predictor dictionary.
# Builds from: diagnostic portions of R_scripts/03.1_variable_selection_for_LASSO.R.
# ==============================================================================

library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
predictor_dictionary_path <- file.path(workflow_dir, "config", "predictor_dictionary.csv")
audit_dir <- file.path(workflow_dir, "data", "audit")

if (!file.exists(model_table_path)) stop("Run 02_prepare_analysis_data_agent_v1.R first.")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

model_table <- read_csv(model_table_path, show_col_types = FALSE)

pair_structure <- model_table %>%
  group_by(response_var) %>%
  summarise(
    n_rows = n(), n_studies = n_distinct(Study_ID),
    n_comparisons = n_distinct(Comparison_ID), n_pairs = n_distinct(candidate_pair_id),
    n_shared_control_families = n_distinct(shared_control_id),
    n_calendar_years = n_distinct(year),
    n_pending_pair_rows = sum(pairing_confirmation_pending), .groups = "drop"
  )

shared_reference_structure <- model_table %>%
  distinct(Study_ID, shared_control_id, candidate_pair_id, Pair_Burn, Pair_Unburn,
           n_pairs_sharing_reference, Pairing_Type_Analysis, Composite_Reference_Candidate) %>%
  group_by(Study_ID, shared_control_id, Pair_Unburn) %>%
  summarise(
    n_pairs = n_distinct(candidate_pair_id),
    burned_pairs = paste(sort(unique(Pair_Burn)), collapse = ";"),
    pairing_types = paste(sort(unique(Pairing_Type_Analysis)), collapse = ";"),
    composite_reference_candidate = any(Composite_Reference_Candidate, na.rm = TRUE),
    .groups = "drop"
  )

response_audit <- model_table %>%
  group_by(response_var) %>%
  summarise(
    lnRR_min = min(lnRR_mean, na.rm = TRUE),
    lnRR_median = median(lnRR_mean, na.rm = TRUE),
    lnRR_max = max(lnRR_mean, na.rm = TRUE),
    n_finite = sum(is.finite(lnRR_mean)),
    n_usable_variances = sum(variance_status == "usable"),
    n_matched_doc_no3 = sum(matched_doc_no3), .groups = "drop"
  )

predictor_definitions <- tribble(
  ~predictor, ~predictor_group, ~transformation, ~primary_candidate, ~definition,
  "post_fire_year", "recovery", "none", TRUE, "Reported time since fire; follow-up year used when unavailable",
  "burn_percent_fire_year", "fire", "none", TRUE, "Percent of burned watershed affected in the fire year",
  "burn_sev_high", "fire", "none", TRUE, "Percent high-severity burn",
  "Area_watershed_km", "topography", "log1p", TRUE, "Burned watershed area",
  "runoffws", "hydrology", "log1p", TRUE, "Mean annual runoff",
  "bfiws", "hydrology", "none", FALSE, "Baseflow index",
  "permws", "hydrology", "log1p", FALSE, "Soil permeability",
  "forest_cover", "landscape", "none", TRUE, "Combined forest-cover percentage",
  "grassland_cover", "landscape", "none", FALSE, "Grassland-cover percentage",
  "wetland_cover", "landscape", "none", FALSE, "Combined wetland-cover percentage",
  "ag_cover", "landscape", "none", FALSE, "Combined agricultural-cover percentage",
  "urban_cover", "landscape", "none", FALSE, "Combined urban-cover percentage",
  "omws", "soil", "none", TRUE, "Watershed soil organic matter",
  "clayws", "soil", "none", FALSE, "Watershed clay content",
  "rckdepws", "geology", "log1p", FALSE, "Depth to bedrock",
  "glacial_till", "geology", "none", FALSE, "Combined glacial-till percentage",
  "precip8110ws", "climate", "none", FALSE, "1981-2010 precipitation normal",
  "tmean8110ws", "climate", "none", FALSE, "1981-2010 mean temperature",
  "slope", "topography", "none", FALSE, "Watershed slope",
  "maxelevsmo", "topography", "none", FALSE, "Maximum smoothed elevation"
)

predictor_labels <- tribble(
  ~predictor, ~predictor_label,
  "post_fire_year", "Post-fire year",
  "time_since_fire", "Time since fire",
  "burn_percent_fire_year", "Burned watershed area (%)",
  "burn_sev_high", "High-severity burn (%)",
  "burn_sev_mod", "Moderate-severity burn (%)",
  "burn_sev_low", "Low-severity burn (%)",
  "Area_watershed_km", "Watershed area (km)",
  "runoffws", "Mean annual runoff",
  "bfiws", "Baseflow index",
  "permws", "Soil permeability",
  "forest_cover", "Forest cover (%)",
  "grassland_cover", "Grassland cover (%)",
  "wetland_cover", "Wetland cover (%)",
  "ag_cover", "Agricultural cover (%)",
  "urban_cover", "Urban cover (%)",
  "omws", "Soil organic matter",
  "clayws", "Soil clay content",
  "rckdepws", "Depth to bedrock",
  "glacial_till", "Glacial till (%)",
  "precip8110ws", "1981-2010 precipitation",
  "tmean8110ws", "1981-2010 mean temperature",
  "slope", "Watershed slope",
  "maxelevsmo", "Maximum smoothed elevation"
)

available_predictors <- intersect(predictor_definitions$predictor, names(model_table))
predictor_missingness <- map_dfr(available_predictors, function(variable) {
  values <- model_table[[variable]]
  tibble(
    predictor = variable, n = length(values), n_missing = sum(is.na(values)),
    proportion_missing = mean(is.na(values)), n_unique = n_distinct(values, na.rm = TRUE),
    minimum = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
    median = if (all(is.na(values))) NA_real_ else median(values, na.rm = TRUE),
    maximum = if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
  )
})

predictor_dictionary <- predictor_definitions %>%
  filter(predictor %in% available_predictors) %>%
  left_join(predictor_labels, by = "predictor") %>%
  left_join(predictor_missingness, by = "predictor") %>%
  mutate(
    include_primary = primary_candidate & proportion_missing <= 0.50 & n_unique >= 3,
    decision_status = if_else(include_primary, "provisional_include", "available_sensitivity_or_excluded"),
    decision_note = case_when(
      proportion_missing > 0.50 ~ "Excluded from default primary set: >50% missing.",
      n_unique < 3 ~ "Excluded from default primary set: insufficient variation.",
      include_primary ~ "Provisional primary predictor; confirm after audit review.",
      TRUE ~ "Available for sensitivity analysis."
    )
  )

predictor_selection_diagnostics <- predictor_definitions %>%
  left_join(predictor_labels, by = "predictor") %>%
  left_join(predictor_missingness, by = "predictor") %>%
  mutate(
    predictor_label = coalesce(predictor_label, predictor),
    available_in_model_table = predictor %in% available_predictors,
    passes_missingness = available_in_model_table & proportion_missing <= 0.50,
    passes_variation = available_in_model_table & n_unique >= 3,
    include_primary = primary_candidate & passes_missingness & passes_variation,
    decision_status = case_when(
      include_primary ~ "provisional_include",
      !available_in_model_table ~ "excluded_unavailable",
      !primary_candidate ~ "available_sensitivity",
      !passes_missingness ~ "excluded_missingness",
      !passes_variation ~ "excluded_low_variation",
      TRUE ~ "available_sensitivity_or_excluded"
    ),
    decision_note = case_when(
      include_primary ~ "Included in the primary predictor set.",
      !available_in_model_table ~ "Not present in the model table.",
      !primary_candidate ~ "Available, but not pre-specified as a primary candidate.",
      !passes_missingness ~ "Excluded from primary set: >50% missing.",
      !passes_variation ~ "Excluded from primary set: fewer than 3 unique values.",
      TRUE ~ "Available for sensitivity analysis."
    )
  ) %>%
  select(
    predictor_label,
    predictor,
    predictor_group,
    transformation,
    definition,
    primary_candidate,
    available_in_model_table,
    n,
    n_missing,
    proportion_missing,
    n_unique,
    minimum,
    median,
    maximum,
    passes_missingness,
    passes_variation,
    include_primary,
    decision_status,
    decision_note
  ) %>%
  arrange(desc(include_primary), predictor_group, predictor_label)

correlation_variables <- predictor_dictionary %>%
  filter(proportion_missing < 0.80, n_unique >= 3) %>% pull(predictor)

if (length(correlation_variables) >= 2) {
  correlation_matrix <- cor(
    model_table %>% select(all_of(correlation_variables)),
    use = "pairwise.complete.obs", method = "spearman"
  )
  predictor_correlations <- as.data.frame(correlation_matrix) %>%
    rownames_to_column("predictor_1") %>%
    pivot_longer(-predictor_1, names_to = "predictor_2", values_to = "rho") %>%
    filter(predictor_1 < predictor_2) %>% arrange(desc(abs(rho)))
  correlation_matrix_print <- correlation_matrix
  rownames(correlation_matrix_print) <- predictor_labels$predictor_label[
    match(rownames(correlation_matrix_print), predictor_labels$predictor)
  ]
  colnames(correlation_matrix_print) <- predictor_labels$predictor_label[
    match(colnames(correlation_matrix_print), predictor_labels$predictor)
  ]
  correlation_matrix_output <- as.data.frame(correlation_matrix_print, check.names = FALSE) %>%
    rownames_to_column("predictor")
} else {
  predictor_correlations <- tibble(predictor_1 = character(), predictor_2 = character(), rho = double())
  correlation_matrix_print <- matrix(numeric(), nrow = 0, ncol = 0)
  correlation_matrix_output <- tibble()
}

predictor_correlations_print <- predictor_correlations %>%
  left_join(predictor_labels, by = c("predictor_1" = "predictor")) %>%
  rename(predictor_1_label = predictor_label) %>%
  left_join(predictor_labels, by = c("predictor_2" = "predictor")) %>%
  rename(predictor_2_label = predictor_label) %>%
  mutate(
    predictor_1_label = coalesce(predictor_1_label, predictor_1),
    predictor_2_label = coalesce(predictor_2_label, predictor_2)
  ) %>%
  select(predictor_1_label, predictor_2_label, predictor_1, predictor_2, rho)

write_csv(pair_structure, file.path(audit_dir, "pair_structure.csv"))
write_csv(shared_reference_structure, file.path(audit_dir, "shared_reference_structure.csv"))
write_csv(response_audit, file.path(audit_dir, "response_audit.csv"))
write_csv(predictor_missingness, file.path(audit_dir, "predictor_missingness.csv"))
write_csv(predictor_correlations, file.path(audit_dir, "predictor_correlations.csv"))
write_csv(correlation_matrix_output, file.path(audit_dir, "predictor_correlation_matrix.csv"))
write_csv(predictor_selection_diagnostics, file.path(audit_dir, "predictor_selection_diagnostics.csv"))
write_csv(predictor_dictionary, predictor_dictionary_path)

message("Wrote audit tables to: ", audit_dir)
message("Primary predictor rule: pre-specified primary candidate, <=50% missing, and at least 3 unique values.")

message("\nAnalysis structure by analyte:")
print(pair_structure, n = Inf, width = Inf)

message("\nResponse-value and variance audit:")
print(response_audit, n = Inf, width = Inf)

message("\nPredictor selection diagnostics:")
print(
  predictor_selection_diagnostics %>%
    select(
      predictor_label,
      predictor_group,
      primary_candidate,
      proportion_missing,
      n_unique,
      passes_missingness,
      passes_variation,
      include_primary,
      decision_status
    ),
  n = Inf,
  width = Inf
)

message("\nPrimary provisional predictors:")
print(
  predictor_selection_diagnostics %>%
    filter(include_primary) %>%
    select(predictor_label, predictor_group, transformation, proportion_missing, n_unique),
  n = Inf,
  width = Inf
)

message("\nPredictors not included in the primary set:")
print(
  predictor_selection_diagnostics %>%
    filter(!include_primary) %>%
    select(predictor_label, predictor_group, decision_status, decision_note),
  n = Inf,
  width = Inf
)

if (nrow(predictor_correlations_print) > 0) {
  message("\nSpearman correlation matrix among screened predictors:")
  print(round(correlation_matrix_print, 3))

  message("\nLargest absolute Spearman correlations among screened predictors:")
  print(
    predictor_correlations_print %>%
      arrange(desc(abs(rho))) %>%
      select(predictor_1_label, predictor_2_label, rho) %>%
      head(10),
    n = 10,
    width = Inf
  )
}

message("Primary provisional predictors: ", paste(
  predictor_selection_diagnostics %>% filter(include_primary) %>% pull(predictor_label), collapse = ", "
))
