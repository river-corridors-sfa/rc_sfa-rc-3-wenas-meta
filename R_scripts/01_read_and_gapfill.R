# =================================== Objectives =================================
#
# Script : 01_read_and_gapfill.R
# 
# Project: rc_sfa-rc-3-wenas-meta
#
# Purpose: Read all per-study CSVs from inputs/Studies/meta_final,
#          harmonize units (mg/L and µM), flag large sampling gaps,
#          build a daily gap-filled time series, and export a
#          single master data frame used by all downstream scripts.
#
# Output : Output_for_analysis/01_read_and_gapfill/01_daily_time_series_paired
#          
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 30 April 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")

rm(list=ls(all=T)) #this clears your Environment


library(pacman)
p_load(tidyverse,
       lubridate,
       here,
       zoo,
       janitor)

# ---- 1. Paths -----------------------------------------------
input_dir  <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/inputs/Studies/meta_final"
output_dir <- here("Output_for_analysis/01_read_and_gapfill")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 2. Read all per-study CSVs -----------------------------
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
message("Found ", length(csv_files), " study files in meta_final/.")

read_study <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      Source_File   = basename(path),
      Sampling_Date = as.Date(str_trim(as.character(Sampling_Date)))
    )
}

all_studies <- map_dfr(csv_files, read_study)

# Filter out Abbott:
all_studies <- all_studies %>% 
  filter(Study_ID != "Abbott et al. 2021")

# Replace sentinel NA codes (-9999) with real NAs in numeric fields
all_studies <- all_studies %>%
  mutate(
    DOC = ifelse(DOC == -9999, NA_real_, DOC),
    NO3 = ifelse(NO3 == -9999, NA_real_, NO3)
  )

message("Total rows read: ", nrow(all_studies))
message("Studies present: ", paste(unique(all_studies$Study_ID), collapse = "; "))

# ---- 3. Unit Harmonization ----------------------------------
# Convert all concentrations to BOTH mg/L and µM so that downstream
# analyses can pick whichever is appropriate (e.g., stoichiometry
# benefits from molar units).
#
# Conversion constants:
#   Atomic mass C   = 12.011 g/mol  -> 1 µM C = 0.012011 mg C/L
#   Atomic mass N   = 14.007 g/mol  -> 1 µM N = 0.014007 mg N/L
#   MW NO3-         = 62.0049 g/mol
#     mg NO3/L -> mg N/L : x * (14.007 / 62.0049) = x * 0.225905
#     mg NO3/L -> µM     : x * (1000 / 62.0049)   = x * 16.1278
#     mg N/L   -> µM N   : x * (1000 / 14.007)    = x * 71.3929
#     ug N/L   -> µM N   : x * 0.001 / 0.014007   = x * 0.071393
#
# NOTE on Abbott et al. 2021: DOC_unit / NO3_unit column reads "um"
# but the supplemental Notes field clarifies the values are µM [1].
# The conversions below treat "um" as µM, matching the authors' intent.

control_impact_units <- all_studies %>%
  mutate(
    # --- Watershed area -> km^2 ------------------------------
    Area_watershed_km = case_when(
      Area_unit == "ha" ~ Area_watershed * 0.01,
      Area_unit == "km" ~ Area_watershed,
      TRUE              ~ NA_real_
    ),
    
    # --- DOC in mg C / L -------------------------------------
    DOC_mg_C_L = case_when(
      DOC_unit %in% c("mg_C_L", "mg_L")     ~ DOC,
      DOC_unit %in% c("um", "uM", "umol_L") ~ DOC * 0.012011,
      is.na(DOC) ~ NA_real_,
      TRUE       ~ NA_real_
    ),
    
    # --- DOC in µM C -----------------------------------------
    DOC_uM_C = case_when(
      DOC_unit %in% c("um", "uM", "umol_L") ~ DOC,
      DOC_unit %in% c("mg_C_L", "mg_L")     ~ DOC / 0.012011,
      is.na(DOC) ~ NA_real_,
      TRUE       ~ NA_real_
    ),
    
    # --- NO3 in mg N / L -------------------------------------
    NO3_mg_N_L = case_when(
      NO3_unit %in% c("um", "uM", "umol_L", "umol_NO2_NO3_L") ~ NO3 * 0.014007,
      NO3_unit == "mg_N_L" ~ NO3,
      NO3_unit == "ug_N_L" ~ NO3 * 0.001,
      NO3_unit == "mg_L"   ~ NO3 * 0.225905,    # mg NO3/L -> mg N/L
      is.na(NO3) ~ NA_real_,
      TRUE       ~ NA_real_
    ),
    
    # --- NO3 in µM N -----------------------------------------
    NO3_uM_N = case_when(
      NO3_unit %in% c("um", "uM", "umol_L", "umol_NO2_NO3_L") ~ NO3,
      NO3_unit == "mg_N_L" ~ NO3 * 71.3929,        # mg N/L -> µM N
      NO3_unit == "ug_N_L" ~ NO3 * 0.071393,       # ug N/L -> µM N
      NO3_unit == "mg_L"   ~ NO3 * 16.1278,        # mg NO3/L -> µM
      is.na(NO3) ~ NA_real_,
      TRUE       ~ NA_real_
    )
  )

# ---- 3b. Unit audit: flag rows where conversion failed ------
unit_audit <- control_impact_units %>%
  filter(
    (!is.na(DOC) & is.na(DOC_mg_C_L)) |
      (!is.na(NO3) & is.na(NO3_mg_N_L)) |
      (!is.na(Area_watershed) & is.na(Area_watershed_km))
  ) %>%
  distinct(Study_ID, DOC_unit, NO3_unit, Area_unit)

if (nrow(unit_audit) > 0) {
  warning("Unrecognized unit strings detected — see outputs/01_unit_audit_warnings.csv")
  write_csv(unit_audit, file.path(output_dir, "01_unit_audit_warnings.csv"))
} else {
  message("Unit harmonization: all unit strings recognized.")
}

# ---- 4. Compute days between samples & flag large gaps ------
control_impact_units <- control_impact_units %>%
  group_by(Study_ID, Site, Pair) %>%
  arrange(Sampling_Date, .by_group = TRUE) %>%
  mutate(
    Days_Between_Samples = c(NA, diff(Sampling_Date)),
    Large_Gap_Flag = Days_Between_Samples > 40
  ) %>%
  ungroup()

# ---- 5. Build a daily time series per Study × Site × Pair ----
create_time_series <- function(df) {
  if (all(is.na(df$Sampling_Date))) return(df)
  all_dates <- tibble(
    Sampling_Date = seq(min(df$Sampling_Date, na.rm = TRUE),
                        max(df$Sampling_Date, na.rm = TRUE),
                        by = "day")
  )
  all_dates %>% left_join(df, by = "Sampling_Date")
}

daily_time_series <- control_impact_units %>%
  group_by(Study_ID, Site, Pair) %>%
  nest() %>%
  mutate(data = map(data, create_time_series)) %>%
  unnest(cols = c(data)) %>%
  arrange(Study_ID, Site, Pair, Sampling_Date)

# ---- 6. Propagate Large_Gap_Flag across daily rows ----------
fill_gap_flags <- function(data) {
  data %>%
    arrange(Sampling_Date) %>%
    mutate(
      Large_Gap_Flag = zoo::na.locf(Large_Gap_Flag, na.rm = FALSE, fromLast = TRUE),
      Large_Gap_Flag = zoo::na.locf(Large_Gap_Flag, na.rm = FALSE)
    )
}

daily_time_series <- daily_time_series %>%
  group_by(Study_ID, Site, Pair) %>%
  nest() %>%
  mutate(data = map(data, fill_gap_flags)) %>%
  unnest(cols = c(data))

# ---- 7. Interpolate DOC & NO3 where gap is acceptable -------
interpolate_values_based_on_flag <- function(data, analyte) {
  new_col <- paste0(analyte, "_Interp")
  data %>%
    arrange(Sampling_Date) %>%
    mutate(!!new_col := ifelse(
      Large_Gap_Flag,
      NA_real_,
      zoo::na.approx(.data[[analyte]], na.rm = FALSE, maxgap = 40)
    ))
}

daily_time_series <- daily_time_series %>%
  group_by(Study_ID, Site, Pair) %>%
  nest() %>%
  mutate(
    data = map(data, ~ interpolate_values_based_on_flag(.x, "DOC_mg_C_L")),
    data = map(data, ~ interpolate_values_based_on_flag(.x, "NO3_mg_N_L")),
    data = map(data, ~ interpolate_values_based_on_flag(.x, "DOC_uM_C")),
    data = map(data, ~ interpolate_values_based_on_flag(.x, "NO3_uM_N"))
  ) %>%
  unnest(cols = c(data)) %>%
  ungroup()

# ---- 8. Tidy up date parts & rename for downstream use ------
daily_time_series <- daily_time_series %>%
  mutate(
    year  = year(Sampling_Date),
    month = month(Sampling_Date),
    day   = day(Sampling_Date)
  ) %>%
  rename(
    DOC_Interp_mg_C_L = DOC_mg_C_L_Interp,
    NO3_Interp_mg_N_L = NO3_mg_N_L_Interp,
    DOC_Interp_uM_C   = DOC_uM_C_Interp,
    NO3_Interp_uM_N   = NO3_uM_N_Interp
  )

# ---- 9. Fill static site metadata & select final columns ----
daily_time_series_filter <- daily_time_series %>%
  group_by(Study_ID, Site, Pair) %>%
  fill(latitude, longitude, Area_watershed_km, Climate, Burn_Unburn,
       Time_Since_Fire) %>% 
  select(Study_ID, Pair, Site, Sampling_Date, latitude, longitude, Area_watershed_km,
         Climate, Burn_Unburn, Time_Since_Fire, Days_Between_Samples, 
         DOC_Interp_mg_C_L, NO3_Interp_mg_N_L)
       
# SUMMARY of interpolated data ####
# ============================================================
# Summary: observations per Study_ID × Site × Time_Since_Fire
# ============================================================

# Make sure we're not operating on a grouped tibble
dts <- daily_time_series_filter %>% ungroup()

# ---- 10. Rolled-up summary at the Study × TSF level ----------
summary_by_study_tsf <- dts %>%
  group_by(Study_ID, Time_Since_Fire, Burn_Unburn) %>%
  summarise(
    n_sites = n_distinct(Site),
    n_days_total = n(),
    n_DOC_nonNA = sum(!is.na(DOC_Interp_mg_C_L)),
    n_NO3_nonNA = sum(!is.na(NO3_Interp_mg_N_L)),
    .groups = "drop"
  ) %>%
  arrange(Study_ID, Time_Since_Fire)

# ============================================================
# Script : 01b_assign_comparisons.R
# Purpose: Derive Comparison_ID directly from the Pair column
#          to link each burn site with its matched unburn control.
# Input  : daily_time_series_filter (from 01_read_and_gapfill.R)
# Output : outputs/01c_master_gapfilled_paired.csv
#          outputs/01c_comparison_key.csv
# ============================================================

# ---- 11. Build the comparison key ---------------------------
build_comparison_key <- function(df) {
  
  # Unique Study × Pair × Burn_Unburn combinations
  pairs <- df %>%
    ungroup() %>%
    distinct(Study_ID, Pair, Burn_Unburn) %>%
    mutate(
      # Strip the "Site_" or "Control_" prefix (case-insensitive, with optional underscore)
      suffix = str_remove(Pair, regex("^(Site|Control)_?", ignore_case = TRUE)),
      suffix = if_else(suffix == "", NA_character_, suffix)
    )
  
  # --- Burn side: one row per burn-site token ---------------
  burns <- pairs %>%
    filter(Burn_Unburn == "Burn") %>%
    mutate(token = str_split(suffix, "_")) %>%
    unnest(token) %>%
    select(Study_ID, Burn_Pair = Pair, token)
  
  # --- Unburn side: one row per control token ---------------
  unburns_tokenized <- pairs %>%
    filter(Burn_Unburn == "Unburn", !is.na(suffix)) %>%
    mutate(token = str_split(suffix, "_")) %>%
    unnest(token) %>%
    select(Study_ID, Unburn_Pair = Pair, token)
  
  # --- Pooled "Control" (no suffix): pairs with ALL burns ---
  unburns_pooled <- pairs %>%
    filter(Burn_Unburn == "Unburn", is.na(suffix)) %>%
    select(Study_ID, Unburn_Pair = Pair) %>%
    inner_join(burns %>% distinct(Study_ID, token), by = "Study_ID")
  
  unburn_full <- bind_rows(unburns_tokenized, unburns_pooled)
  
  # --- Join burn and unburn sides on Study_ID + token -------
  key <- burns %>%
    inner_join(unburn_full,
               by = c("Study_ID", "token"),
               relationship = "many-to-many") %>%
    mutate(
      study_slug    = str_replace_all(Study_ID, "[^A-Za-z0-9]+", "_"),
      study_slug    = str_remove(study_slug, "_+$"),
      Comparison_ID = paste0(study_slug, "_C", token)
    ) %>%
    select(Study_ID, Comparison_ID, Burn_Pair, Unburn_Pair, token)
  
  key
}

comparison_key <- build_comparison_key(daily_time_series_filter)

# ---- 12. Reshape the key to long form for joining -----------
cw_long <- comparison_key %>%
  pivot_longer(
    cols      = c(Burn_Pair, Unburn_Pair),
    names_to  = "role",
    values_to = "Pair"
  ) %>%
  mutate(Burn_Unburn = if_else(role == "Burn_Pair", "Burn", "Unburn")) %>%
  distinct(Study_ID, Pair, Burn_Unburn, Comparison_ID)

# ---- 13. Join Comparison_ID back onto daily data ------------
daily_time_series_paired <- daily_time_series_filter %>%
  ungroup() %>%
  left_join(cw_long, by = c("Study_ID", "Pair", "Burn_Unburn"),
            relationship = "many-to-many")

# ---- 14. Audit: flag rows that didn't link ------------------
missing_links <- daily_time_series_paired %>%
  filter(is.na(Comparison_ID)) %>%
  distinct(Study_ID, Site, Pair, Burn_Unburn)

if (nrow(missing_links) > 0) {
  warning("Some Pair values did not link — see 01c_missing_comparison_links.csv")
} else {
  message("All Pair values successfully linked to a Comparison_ID.")
}

# ---- 15. Export ---------------------------------------------
out_dir <- here("Output_for_analysis/01_read_and_gapfill")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(daily_time_series_paired,
          file.path(out_dir, "01_daily_time_series_paired.csv"))

       








