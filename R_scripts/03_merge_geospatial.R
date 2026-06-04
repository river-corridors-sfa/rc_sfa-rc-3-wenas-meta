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

# wally <- geo_vars %>%
#   filter(site == "Wally_Creek") %>%
#   select(burn_percent_fire_year)

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
# Are there any unburned watersheds that have a burn percent?
control <- merged %>% 
  select(Study_ID, Site, Burn_Unburn, fire_years_used, burn_percent_fire_year, totdasqkm) %>%
  distinct(Site, .keep_all = TRUE)

nobueno <- control %>% 
  filter(Burn_Unburn == "Unburn") %>% 
  filter(burn_percent_fire_year > 0)

nobuenoburn <- control %>% 
  filter(Burn_Unburn == "Burn") %>% 
  filter(burn_percent_fire_year == 0)

# Are there any unburned watersheds that have a burn severity?
MTBS <- merged %>% 
  select(Study_ID, Site, Burn_Unburn, fire_years_used, burn_sev_high, burn_sev_mod, burn_sev_low) %>%
  distinct(Site, .keep_all = TRUE)

bad <- MTBS %>% 
  filter(Burn_Unburn == "Unburn") %>% 
  filter(burn_sev_high > 0 | burn_sev_mod > 0 | burn_sev_low > 0)

badburn <- MTBS %>% 
  filter(Burn_Unburn == "Burn") %>% 
  filter(burn_sev_high == 0 | burn_sev_mod == 0 | burn_sev_low == 0)


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

# ---- 9. Explore variables ------------------------------------------------------------
names(merged)

# ---- 10. Export ------------------------------------------------------------

out_dir <- here("Output_for_analysis", "03_merge_geospatial")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(merged, file.path(out_dir, "03_master_merged.csv"))

message("Wrote: ", file.path(out_dir, "03_master_merged.csv"))

# BRIE LASSO csv with the 24 variables of interest:
# admin_vars <- c("rpuid", "vpuid", "enabled",
#                 "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
#                 "Study_ID", "Comparison_ID", "Pair", "Sampling_Date",
#                 "latitude", "longitude", "comid", "DOC_Interp_mg_C_L", "NO3_Interp_mg_N_L", 
#                 "Days_Between_Samples", "objectid", "fdate", "resolution", "gnis_id",
#                 "gnis_name", "reachcode", "flowdir", "wbareacomi", "ftype", "fcode", "shape_length",
#                 "streamleve", "streamcalc", "fromnode", "tonode", "hydroseq", "levelpathi",
#                 "pathlength", "terminalpa", "arbolatesu")

# watershed area
# Maximum watershed elevation smooth
# Watershed slope
# 30 year max temp 
# 30 year mean temp 
# 30 year min air temp 
# Forest cover
# Urban cover
# Grassland cover 
# Wetland cover 
# AG cover
# OM mean
# mean bedrock depth 
# Carbonate residual material
# Non-carbonate residual material 
# Silicic Residual material 
# Glacial till
# Saline lake sediment 
# Baseflow index
# soil permeability 
# mean runoff
# watershed burn percentage 
# Burn severity 
# Time since fire 

# 
merged_Brie_LASSO_01 <- merged %>%
  mutate(
    # --- Combined land cover classes ---
    forest_cover  = rowSums(across(c(pctconif2019ws, pctdecid2019ws, pctmxfst2019ws)), na.rm = TRUE),
    urban_cover   = rowSums(across(c(pcturbhi2019ws, pcturbmd2019ws, pcturblo2019ws, pcturbop2019ws)), na.rm = TRUE),
    wetland_cover = rowSums(across(c(pcthbwet2019ws, pctwdwet2019ws)), na.rm = TRUE),
    ag_cover      = rowSums(across(c(pctcrop2019ws, pcthay2019ws)), na.rm = TRUE),
    glacial_till  = rowSums(across(c(pctglactilloamws, pctglactilcrsws, pctglactilclayws)), na.rm = TRUE),
    grassland_cover = pctgrs2019ws
  ) %>%
  select(
    # --- Identifiers / response variables (keep these!) ---
    Study_ID, Comparison_ID, Pair, Site, Sampling_Date,
    latitude, longitude, comid, Burn_Unburn, Climate,
    DOC_Interp_mg_C_L, NO3_Interp_mg_N_L,
    
    # 1. Watershed area
    Area_watershed_km,
    # 2. Max watershed elevation (smooth)
    maxelevsmo,
    # 3. Watershed slope
    slope,
    # 4-6. 30-yr climate normals
    tmax8110ws, tmean8110ws, tmin8110ws,
    # 7-11. Land cover (combined)
    forest_cover, urban_cover, grassland_cover, wetland_cover, ag_cover,
    # 12. Soil organic matter
    omws,
    # 13. Bedrock depth
    rckdepws,
    # 14-16. Residual lithology
    pctcarbresidws, pctnoncarbresidws, pctsilicicws,
    # 17. Glacial till (combined)
    glacial_till,
    # 18. Saline lake sediment
    pctsallakews,
    # 19. Baseflow index
    bfiws,
    # 20. Soil permeability
    permws,
    # 21. Mean runoff
    runoffws,
    # 22. Watershed burn percentage
    burn_percent_fire_year,
    # 23. Burn severity
    burn_sev_high, burn_sev_mod, burn_sev_low,
    # 24. Time since fire
    Time_Since_Fire
  )
# remove pre post sites
merged_Brie_LASSO_02 <- merged_Brie_LASSO_01 %>% 
  filter(!Pair %in% c("Site_2_post", "Site_3_post", "Site_4_post"))

# deal with burn percent and severity for a few sites:
# Gluns & Toews; 1989 - East Fork Kootenai - remove (Canada)
# Gluns & Toews; 1989 - Middle Fork Kootenai - remove (Canada)
# Hickenbottom et al. 2023 - Middle_Fork_American - burn severity from text 
# Hickenbottom et al. 2023 - North_Fork_American - burn severity from text 
# Hickenbottom et al. 2023 - Trout_Creek - burn severity from text 
# Neary & Currier; 1982 - Crane Creek - remove (fire too old)
# Neary & Currier; 1982  - Wash Branch - remove (fire too old)
# Tiedemann; 1973 - Camas Creek - remove (fire too old)
# Tiedemann; 1973 - Grade Creek - remove (fire too old)

# test <- merged_Brie_LASSO_02 %>% 
#   filter(Study_ID == "Hickenbottom et al. 2023")

# Manually put in burn severity values for Hickenbottom based on the text and remove the papers that are too old or in canada 
merged_Brie_LASSO_03 <- merged_Brie_LASSO_02 %>% 
  filter(!Study_ID %in% c("Burd et al 2018", "Gluns & Toews; 1989", 
                          "Neary & Currier; 1982", "Tiedemann; 1973"))

# test <- merged_Brie_LASSO_03 %>% 
#   filter(Study_ID == "Hickenbottom et al. 2023")

merged_Brie_LASSO_04 <- merged_Brie_LASSO_03 %>%
  mutate(
    burn_sev_low = case_when(
      Study_ID == "Hickenbottom et al. 2023" & Site == "Middle_Fork_American" ~ 58,
      Study_ID == "Hickenbottom et al. 2023" & Site == "Trout_Creek" ~ 39,
      TRUE ~ burn_sev_low
    ),
    burn_sev_mod = case_when(
      Study_ID == "Hickenbottom et al. 2023" & Site == "Middle_Fork_American" ~ 25,
      Study_ID == "Hickenbottom et al. 2023" & Site == "Trout_Creek" ~ 45,
      TRUE ~ burn_sev_mod
    ),
    burn_sev_high = case_when(
      Study_ID == "Hickenbottom et al. 2023" & Site == "Middle_Fork_American" ~ 9,
      Study_ID == "Hickenbottom et al. 2023" & Site == "Trout_Creek" ~ 3,
      TRUE ~ burn_sev_high
    )
  )

merged_Brie_LASSO_final <- merged_Brie_LASSO_04 %>%
  mutate(
    burn_percent_fire_year = case_when(
      Study_ID == "Hickenbottom et al. 2023" & Site == "Middle_Fork_American" ~ 20.68,
      Study_ID == "Hickenbottom et al. 2023" & Site == "Trout_Creek" ~ 43.38,
      TRUE ~ burn_percent_fire_year
    )
  )

write_csv(merged_Brie_LASSO_final, file.path(out_dir, "03_master_merged_Brie_LASSO.csv"))

# # Read in Katie dNBR workflow:
# DNBR_Severity <- read_csv("Output_for_analysis/archive/14_Meta_calculate_burn_severity/DNBR_Severity.csv")
# 
# brie_site_list <- merged_Brie_LASSO_02 %>% 
#   select(Study_ID, Site, Burn_Unburn, burn_sev_low, burn_sev_mod, burn_sev_high) %>% 
#   distinct()
# 
# # Function to standardize site names
# standardize_sites <- function(df) {
#   df %>%
#     mutate(Site = Site %>%
#              str_replace_all("[_\\-\\.]", " ") %>%  # Replace _, -, . with space
#              str_squish() %>%                       # Remove extra whitespace
#              str_to_title())                        # Title Case (e.g., "Coal Creek")
# }
# 
# # Apply to both data frames
# DNBR_Severity <- standardize_sites(DNBR_Severity)
# brie_site_list <- standardize_sites(brie_site_list)
# 
# # Now merge
# merged_df <- DNBR_Severity %>%
#   full_join(brie_site_list, by = "Site") %>% 
#   select(Study_ID, Site, Burn_Unburn, mean_dnbr, burn_sev_low, burn_sev_mod, burn_sev_high) %>% 
#   filter(!is.na(Study_ID))
# 
#     #  * Burd et al 2018 - in Canada so no burn metrics. This will have to go. 
# # Coombs & Melack; 2013 - has both dNBR and low/mod/high
# # Crandall et al. 2021 - has both dNBR and low/mod/high
# # Gerla & Galloway; 1998 - has both dNBR and low/mod/high
#     # * Gluns & Toews; 1989 - *does not have dNBR or low/mod/high* - in Canada so no burn metrics. This will have to go.
# # Hauer & Spencer 1998 - has both dNBR and low/mod/high
#     # * Hickenbottom et al. 2023 - *does not have dNBR or low/mod/high*
#           # Middle Fork - 58 low, 25 moderate, 9 high 
#           # Trout Creek - 39 low, 45 moderate, 3 high 
#     # * Mast & Clow; 2008 - has both dNBR and low/mod/high but dNBR is negative...
# # Murphy et al. 2018 - has both dNBR and low/mod/high
#     # * Neary & Currier; 1982 - *does not have dNBR or low/mod/high*
#         # Fire from 1978 - remove 
#     # * Oliver et al. 2012 - has both dNBR and low/mod/high but dNBR is weird
#   # Rhea et al. 2021 - has low/mod/high but NO dNBR
#     # * Tiedemann; 1973 - *does not have dNBR or low/mod/high*
#   # * Uzun et al. 2020 - has low/mod/high but NO dNBR
# # Wagner et al. 2015 - has both dNBR and low/mod/high
# # Writer et al. 2014 - has both dNBR and low/mod/high
# 
# 
# 
# merged_Brie_LASSO_03 <- merged_Brie_LASSO_02 %>% 
#   filter(!Pair %in% c("Site_2_post", "Site_3_post", "Site_4_post"))
# 
# 
# 
# write_csv(merged_Brie_LASSO, file.path(out_dir, "03_master_merged_Brie_LASSO.csv"))
# 
# 
# 
# 
# 
# 
# 
# 
