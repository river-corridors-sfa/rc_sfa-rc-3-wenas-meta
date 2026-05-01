# =================================== Objectives =================================
#
# Script : 03_merge_geospatial.R
# Project: rc_sfa-rc-3-wenas-meta
# Purpose: Merge daily gap-filled time series (from 01) with geospatial 
#          variables pulled per site COMID (from 02).
#
# Inputs :
#   - Output_for_analysis/01_read_and_gapfill/01_daily_time_series_paired.csv
#   - Output_for_analysis/02_geospatial_comid_extraction/geospatial_variables_bp_severity_pull.csv
# Output :
#   - Output_for_analysis/03_merge_geospatial/03_daily_time_series_with_geospatial.csv
#
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
       here,
       janitor)

# ---- 1. Read inputs --------------------------------------------------------

daily_ts <- read_csv(
  here("Output_for_analysis", "01_read_and_gapfill",
       "01_daily_time_series_paired.csv"),
  na = c("-9999", "N/A")
)

geo_vars <- read_csv(
  here("Output_for_analysis", "02_geospatial_comid_extraction",
       "geospatial_variables_bp_severity_pull.csv"),
  na = c("", "NA", "-9999", "N/A"),              # add "NA" explicitly
  show_col_types = FALSE
)

# ---- 2. Harmonize the join key --------------------------------------------
# daily_ts uses "Site" (capital S, with underscores e.g., "Akokala_Creek")
# geo_vars uses "site" (lowercase, with underscores too based on your file)

geo_vars <- geo_vars %>%
  rename(Site = site) %>%
  dplyr::select(-any_of(c("latitude", "longitude")))   # drop duplicate coords

# ---- 3. Diagnostic check for name mismatches ------------------------------

missing_in_geo <- setdiff(unique(daily_ts$Site), unique(geo_vars$Site))
missing_in_ts  <- setdiff(unique(geo_vars$Site), unique(daily_ts$Site))

if(length(missing_in_geo) > 0){
  message("Sites in time series but NOT in geospatial file (", 
          length(missing_in_geo), "):")
  message(paste(missing_in_geo, collapse = ", "))
}
if(length(missing_in_ts) > 0){
  message("Sites in geospatial file but NOT in time series (", 
          length(missing_in_ts), "):")
  message(paste(missing_in_ts, collapse = ", "))
}

# ---- 4. Check for duplicates on the join key ------------------------------

dup_check <- geo_vars %>% count(Site) %>% filter(n > 1)
if(nrow(dup_check) > 0){
  warning("Duplicate sites in geo_vars — will inflate the join!")
  print(dup_check)
}

# checking <- geo_vars %>% 
#   filter(Site %in% c("PBR", "PNF", "PSF")) %>% 
#   select(Site, comid, burn_percent_fire_year, fire_years_used, burn_sev_high)

# ---- 5. Left join ---------------------------------------------------------

merged <- daily_ts %>%
  left_join(geo_vars, by = "Site")

# ---- 6. Sanity checks -----------------------------------------------------

stopifnot(nrow(merged) == nrow(daily_ts))   # row count must match

message("Daily time series rows : ", nrow(daily_ts))
message("Merged rows            : ", nrow(merged))
message("Unique sites in merged : ", length(unique(merged$Site)))
message("Sites w/ COMID         : ",
        length(unique(merged$Site[!is.na(merged$comid)])))
message("Sites w/ burn_percent  : ",
        length(unique(merged$Site[!is.na(merged$burn_percent_fire_year)])))
message("Sites w/ burn severity : ",
        length(unique(merged$Site[!is.na(merged$burn_sev_high)])))

# ---- 7. Reorder columns for readability -----------------------------------
# Put key identifiers first, then response variables, then predictors

merged <- merged %>%
  dplyr::relocate(
    Study_ID, Comparison_ID, Pair, Site, Sampling_Date,
    latitude, longitude, comid,
    Burn_Unburn, Time_Since_Fire, Climate, Area_watershed_km,
    DOC_Interp_mg_C_L, NO3_Interp_mg_N_L,
    burn_percent_fire_year, fire_years_used,
    burn_sev_high, burn_sev_mod, burn_sev_low
  )

# ---- 8. Sanity Check  ------------------------------------------------------------
vars_to_check <- c("Climate", "Area_watershed_km", "Time_Since_Fire", "comid",
                   "burn_percent_fire_year", "burn_sev_high", "burn_sev_mod",
                   "burn_sev_low")

# ---- Row-level NA summary ---------------------------------------
na_summary <- merged %>%
  summarise(across(all_of(vars_to_check),
                   ~ sum(is.na(.)),
                   .names = "NAs_{.col}")) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_NA") %>%
  mutate(
    variable = str_remove(variable, "^NAs_"),
    n_total = nrow(merged),
    pct_NA = round(100 * n_NA / n_total, 2)
  )

print(na_summary)

# ---- Site-level NA summary (more interpretable for meta-analysis) ---
na_by_site <- merged %>%
  group_by(Study_ID, Site) %>%
  summarise(across(all_of(vars_to_check),
                   ~ all(is.na(.)),
                   .names = "all_NA_{.col}"),
            .groups = "drop")

# Count how many SITES have all-NA values for each variable
na_by_site %>%
  summarise(across(starts_with("all_NA_"),
                   ~ sum(.),
                   .names = "n_sites_{.col}")) %>%
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_sites_all_NA")

# ---- Which specific sites are NA for each variable? -----------------
for(v in vars_to_check){
  sites_na <- merged %>%
    filter(is.na(.data[[v]])) %>%
    distinct(Study_ID, Site) %>%
    arrange(Study_ID, Site)
  
  message("\n--- Sites with NA for '", v, "' (", nrow(sites_na), " sites) ---")
  if(nrow(sites_na) > 0) print(sites_na, n = Inf)
}
# ---- 9. Export ------------------------------------------------------------

out_dir <- here("Output_for_analysis", "03_merge_geospatial")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(merged, file.path(out_dir, "03_master_merged.csv"))

message("Wrote: ", file.path(out_dir, "03_master_merged.csv"))






