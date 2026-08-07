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
# THIS IS WHERE THE LASSO DATA FRAME IS GENERATED FOR BRIE TO RUN
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
    tmax8110ws, tmean8110ws, tmin8110ws, precip8110ws,
    # 7-11. Land cover (combined)
    forest_cover, urban_cover, grassland_cover, wetland_cover, ag_cover,
    # 12. Soil organic matter
    omws,
    # 13. Bedrock depth
    rckdepws,
    # 14-16. Residual lithology
    pctcarbresidws, pctnoncarbresidws, pctsilicicws, clayws,
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

merged_Brie_LASSO_05 <- merged_Brie_LASSO_04 %>%
  mutate(
    burn_percent_fire_year = case_when(
      Study_ID == "Hickenbottom et al. 2023" & Site == "Middle_Fork_American" ~ 20.68,
      Study_ID == "Hickenbottom et al. 2023" & Site == "Trout_Creek" ~ 43.38,
      TRUE ~ burn_percent_fire_year
    )
  )


# ======================== Fetch watershed boundaries for each unique COMID ====================================
library(nhdplusTools)
library(sf)
library(dplyr)
library(purrr)

# ---------- User settings ----------
# Replace this with your actual data frame
# Assumes columns: Site, comid (and any other metadata)

OUT_DIR <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/shape_files"

# ---------- Function to fetch one watershed ----------
get_ws_from_comid <- function(comid, Site) {
  message(sprintf("Fetching site %s (COMID %s)...", Site, comid))
  
  ws <- tryCatch({
    get_nldi_basin(
      nldi_feature = list(featureSource = "comid",
                          featureID = as.character(comid))
    )
  }, error = function(e) {
    warning(sprintf("Failed for %s (COMID %s): %s", Site, comid, e$message))
    return(NULL)
  })
  
  if (is.null(ws) || nrow(ws) == 0) {
    warning(sprintf("Empty result for %s (COMID %s)", Site, comid))
    return(NULL)
  }
  
  ws$Site <- Site
  ws$comid   <- comid
  ws$area_km2 <- as.numeric(st_area(st_transform(ws, 5070))) / 1e6
  ws
}

# ---------- Loop over unique COMIDs ----------
unique_sites <- merged_Brie_LASSO_05 %>%  
  distinct(Site, comid)

ws_list <- pmap(
  list(unique_sites$comid, unique_sites$Site),
  get_ws_from_comid
)

# Drop failures
ws_list <- ws_list[!sapply(ws_list, is.null)]

# Combine into one sf object
ws_all <- do.call(rbind, ws_list)

# ---------- Save ----------
# One GeoPackage with all watersheds (recommended)
st_write(ws_all, file.path(OUT_DIR, "all_watersheds.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

# Also save one file per site (useful for the AI workflow)
for (i in seq_len(nrow(ws_all))) {
  sid <- ws_all$Site[i]
  st_write(ws_all[i, ],
           file.path(OUT_DIR, sprintf("%s.gpkg", sid)),
           delete_dsn = TRUE, quiet = TRUE)
}

# ---------- Quick QA ----------
cat("\n=== Summary ===\n")
print(ws_all %>%  st_drop_geometry() %>% 
        select(Site, comid, area_km2))

# Plot all watersheds
plot(st_geometry(ws_all), border = "steelblue", lwd = 1)
title(sprintf("Delineated watersheds (n = %d)", nrow(ws_all)))

# Compare delineated area vs. reported area from your meta-analysis
sites_with_area <- merged_Brie_LASSO_05 %>% 
  left_join(st_drop_geometry(ws_all), by = c("Site", "comid"))

# Look for large mismatches
sites_with_area <- sites_with_area %>% 
  mutate(pct_diff = 100 * (area_km2 - Area_watershed_km) / Area_watershed_km) %>% 
  arrange(desc(abs(pct_diff)))

# =============== Batch Aridity Index workflow for multiple watersheds ===============
# Inputs:  one GeoPackage per site in ./shapes_input/
# Outputs: annual + long-term AI tables, per-site PNG maps

# Install remotes if you don't have it
# install.packages("remotes")
# 
# # Install climateR from GitHub
# remotes::install_github("mikejohnson51/climateR")

library(sf)
library(terra)
library(climateR)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

merged_Brie_LASSO_final_TEST <- merged_Brie_LASSO_05 

# ---------- User settings ----------
SHAPE_DIR   <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/shape_files"
OUT_DIR     <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/gridmet_downloads"
MAP_DIR     <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/ai_maps"
START_DATE  <- as.Date("2020-01-01")
END_DATE    <- as.Date("2024-12-31")
BUFFER_M    <- 2000
MIN_DAYS    <- 300

# ---------- Helper: compute AI for one watershed ----------
weighted_daily_mean <- function(r_stack, frac_r) {
  if (!compareGeom(r_stack, frac_r, stopOnError = FALSE)) {
    frac_r <- resample(frac_r, r_stack[[1]], method = "near")
  }
  w  <- as.numeric(values(frac_r))     # length ncell
  vs <- values(r_stack)                # ncell x nlyr matrix
  
  num <- colSums(vs * w, na.rm = TRUE)
  den <- colSums((!is.na(vs)) * w)
  
  out <- num / den
  out[!is.finite(out)] <- NA
  out
}

compute_ai <- function(gpkg_path, Site,
                       start_date = START_DATE,
                       end_date   = END_DATE,
                       buffer_m   = BUFFER_M,
                       min_days   = MIN_DAYS,
                       out_dir    = OUT_DIR,
                       map_dir    = MAP_DIR) {
  
  message(sprintf("--- Processing %s ---", Site))
  
  # 1. Load and buffer watershed
  ws <- st_read(gpkg_path, quiet = TRUE) |> st_union() |> st_sf()
  ws_buff_ll <- ws |> st_transform(5070) |> st_buffer(buffer_m) |> st_transform(4326)
  ws_ll      <- st_transform(ws, 4326)
  
  # 2. Download / load gridMET
  pr_file  <- file.path(out_dir, sprintf("%s_pr_%s_%s.tif",  Site, start_date, end_date))
  pet_file <- file.path(out_dir, sprintf("%s_pet_%s_%s.tif", Site, start_date, end_date))
  
  if (!file.exists(pr_file)) {
    pr <- getGridMET(ws_buff_ll, "pr", start_date, end_date)[[1]]
    writeRaster(pr, pr_file, overwrite = TRUE)
  } else pr <- rast(pr_file)
  
  if (!file.exists(pet_file)) {
    pet <- getGridMET(ws_buff_ll, "pet", start_date, end_date)[[1]]
    writeRaster(pet, pet_file, overwrite = TRUE)
  } else pet <- rast(pet_file)
  
  # 3. Align pet to pr if needed; build fraction weights
  if (!compareGeom(pr, pet, stopOnError = FALSE)) pet <- resample(pet, pr, method = "near")
  frac <- rasterize(vect(ws_ll), pr[[1]], cover = TRUE, background = 0)
  mask <- frac > 0
  
  pr_dates  <- as.Date(gsub(".*_", "", names(pr)))
  pet_dates <- as.Date(gsub(".*_", "", names(pet)))
  
  # 4. Weighted daily means
  pr_daily  <- weighted_daily_mean(pr,  frac)
  pet_daily <- weighted_daily_mean(pet, frac)
  
  # 5. Align to common dates
  common <- as.Date(intersect(pr_dates, pet_dates), origin = "1970-01-01")
  pr_daily  <- pr_daily[match(common, pr_dates)]
  pet_daily <- pet_daily[match(common, pet_dates)]
  
  daily_df <- tibble(Site = Site, date = common,
                     year = year(common), P = pr_daily, PET = pet_daily)
  
  # 6. Annual summary
  annual_df <- daily_df |>
    group_by(Site, year) |>
    summarise(n_days     = sum(!is.na(P) & !is.na(PET)),
              P_annual   = sum(P,   na.rm = TRUE),
              PET_annual = sum(PET, na.rm = TRUE),
              AI         = P_annual / PET_annual,
              .groups = "drop") |>
    filter(n_days >= min_days)
  
  # 7. Long-term mean
  longterm_df <- annual_df |>
    summarise(AI_longterm = mean(AI, na.rm = TRUE),
              P_mean      = mean(P_annual, na.rm = TRUE),
              PET_mean    = mean(PET_annual, na.rm = TRUE),
              n_years     = n()) |>
    mutate(Site = Site, .before = 1)
  
  list(annual = annual_df, longterm = longterm_df)
}

# ---------- Loop over all site GeoPackages ----------
gpkg_files <- list.files(SHAPE_DIR, pattern = "\\.gpkg$", full.names = TRUE)
gpkg_files <- gpkg_files[!grepl("all_watersheds", gpkg_files)]  # exclude combined

Sites <- tools::file_path_sans_ext(basename(gpkg_files))

results <- map2(gpkg_files, Sites, compute_ai)
results <- results[!sapply(results, is.null)]

# results2 <- map2(gpkg_files, Sites, compute_ai)
# # results2 <- results[!sapply(results2, is.null)]

# ---------- Combine results ----------
annual_all   <- bind_rows(purrr::map(results, "annual"))
longterm_all <- bind_rows(purrr::map(results, "longterm"))

# Save tables
write.csv(annual_all,   "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/ai_maps/aridity_annual.csv",   row.names = FALSE)
write.csv(longterm_all, "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/ai_maps/aridity_longterm.csv", row.names = FALSE)

cat("\n=== Long-term AI summary ===\n")
print(longterm_all)

# ---------- Optional: comparison plot across sites ----------
ggplot(annual_all, aes(year, AI, color = Site)) +
  geom_line() + geom_point() +
  labs(title = "Annual aridity index across sites",
       y = "AI = P / PET", x = "Year") +
  theme_bw()

ggplot(longterm_all, aes(reorder(Site, AI_longterm), AI_longterm)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Long-term aridity index by site",
       x = NULL, y = "AI (mean P / mean PET)") +
  theme_bw()

# Merge Aridity with BRIE LASSO 
merged_Brie_LASSO_06 <- merged_Brie_LASSO_final_TEST %>% 
  left_join(longterm_all, by = "Site")

effect_sizes_daily <- read_csv("Output_for_analysis/04_calculate_effect_sizes/effect_sizes_daily.csv") |> 
  select(Study_ID:response_var, Pair_Burn, Site_Burn, lnRR, lnRR_area)

effect_sizes_wide <- effect_sizes_daily %>%
  pivot_wider(
    names_from = response_var,
    values_from = c(lnRR, lnRR_area),
    names_glue = "{.value}_{response_var}"
  )

merged_data <- merged_Brie_LASSO_06 %>%
  left_join(
    effect_sizes_wide,
    by = c(
      "Study_ID",
      "Comparison_ID",
      "Pair" = "Pair_Burn",
      "Sampling_Date"
    )
  )

merged_Brie_LASSO_final <- merged_data

# Write final merged_Brie_LASSO data frame with daily effect sizes calculated 

write_csv(merged_Brie_LASSO_final, file.path(out_dir, "03_master_merged_Brie_LASSO.csv"))










# ====================== LETS CONFIRM THAT THE SHAPE FILES MATCH THE LAT/LONGs FROM META ============
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)
library(tigris)

options(tigris_use_cache = TRUE)

# ----- Paths ----
gpkg_dir <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/shape_files"

csv_file <- "/Users/cava304/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/03_master_merged_Brie_LASSO.csv"

# ----- Read master data ----
master <- read_csv(csv_file)

# Change these if your column names differ
lat_col  <- "latitude"
lon_col  <- "longitude"
site_col <- "Site"


# ----- Convert lat/lon to sf points ----
pts <- st_as_sf(
  master,
  coords = c(lon_col, lat_col),
  crs = 4326,
  remove = FALSE
)

# ----- Read all geopackages ----
gpkg_files <- list.files(
  gpkg_dir,
  pattern = "\\.gpkg$",
  full.names = TRUE
) 

# Filter out "all_watersheds.gpkg"
gpkg_files <- gpkg_files[c(1, 3:16, 18:57)]

watersheds <- map_dfr(gpkg_files, function(f){
  
  x <- st_read(f, quiet = TRUE)
  
  x$Site <- tools::file_path_sans_ext(basename(f))
  
  x
})

# ----- US map ----
us <- states(cb = TRUE) |>
  filter(!STUSPS %in% c("AK","HI","PR", "AS", "MP", "GU", "VI"))

# ----- Plot ----
ggplot() +
  geom_sf(data = us,
          fill = "grey95",
          color = "grey60") +
  geom_sf(data = pts,
          color = "red",
          size = 1.2) +
  geom_sf(data = watersheds,
          fill = "dodgerblue",
          alpha = 0.35,
          color = "blue",
          linewidth = 0.2) +
  coord_sf() +
  theme_classic() +
  labs(
    title = "Comparison of Original Sampling Coordinates and NLDI Watersheds",
    subtitle = "Red = published coordinates, Blue = watershed polygons"
  )

# Calculate whether each point falls inside its watershed
watersheds <- watersheds %>%
  mutate(Site = str_trim(Site))

pts <- pts %>%
  mutate(Site = str_trim(.data[[site_col]]))

comparison <- pts %>%
  left_join(
    watersheds %>%
      st_drop_geometry() %>%
      distinct(Site),
    by = "Site"
  )

# LOOP 
results <- map_dfr(unique(pts$Site), function(s){
  
  pt <- pts %>% filter(Site == s)
  
  poly <- watersheds %>% filter(Site == s)
  
  if(nrow(poly)==0){
    
    return(data.frame(
      Site = s,
      inside = NA,
      distance_m = NA
    ))
    
  }
  
  inside <- lengths(st_within(pt, poly)) > 0
  
  dist <- as.numeric(
    st_distance(
      st_transform(pt, 5070),
      st_transform(poly, 5070)
    )
  )
  
  data.frame(
    Site = s,
    inside = inside,
    distance_m = dist
  )
  
})

results_test <- results %>% 
  distinct(Site, .keep_all = TRUE) 
  


# TAKE 2 #
library(sf)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(tools)

#-------------------------------------------------------
# Paths
#-------------------------------------------------------

gpkg_dir <- "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/shape_files"

csv_file <- "/Users/cava304/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/03_master_merged_Brie_LASSO.csv"

out_dir <- file.path(gpkg_dir, "QC_maps")

dir.create(out_dir, showWarnings = FALSE)

#-------------------------------------------------------
# Read master data
#-------------------------------------------------------

master <- read_csv(csv_file)

# CHANGE THESE TO MATCH YOUR COLUMN NAMES
site_col <- "Site"
lat_col  <- "latitude"
lon_col  <- "longitude"

pts <- st_as_sf(
  master,
  coords = c(lon_col, lat_col),
  crs = 4326,
  remove = FALSE
)

#-------------------------------------------------------
# List watershed files
#-------------------------------------------------------

gpkg_files <- list.files(
  gpkg_dir,
  pattern = "\\.gpkg$",
  full.names = TRUE
)

gpkg_files <- gpkg_files[c(1, 3:16, 18:57)]


#-------------------------------------------------------
# Loop over every watershed
#-------------------------------------------------------

for(f in gpkg_files){
  
  site <- file_path_sans_ext(basename(f))
  
  message(site)
  
  ws <- st_read(f, quiet = TRUE)
  
  pt <- pts %>%
    filter(.data[[site_col]] == site)
  
  if(nrow(pt)==0){
    
    message("No point found for ", site)
    next
    
  }
  
  # Make everything projected
  ws_proj <- st_transform(ws, 5070)
  pt_proj <- st_transform(pt, 5070)
  
  # Is point inside watershed?
  inside <- lengths(st_within(pt_proj, ws_proj)) > 0
  
  # Distance (meters)
  dist_m <- as.numeric(st_distance(pt_proj, ws_proj))
  
  # Build bounding box with 5 km padding
  bbox <- st_bbox(ws_proj)
  
  pad <- 5000
  
  bbox["xmin"] <- bbox["xmin"] - pad
  bbox["xmax"] <- bbox["xmax"] + pad
  bbox["ymin"] <- bbox["ymin"] - pad
  bbox["ymax"] <- bbox["ymax"] + pad
  
  bbox_sf <- st_as_sfc(bbox)
  
  # Plot
  p <- ggplot() +
    
    geom_sf(
      data = bbox_sf,
      fill = "grey98",
      color = NA
    ) +
    
    geom_sf(
      data = ws_proj,
      fill = "lightblue",
      color = "blue",
      linewidth = 0.6,
      alpha = 0.6
    ) +
    
    geom_sf(
      data = pt_proj,
      color = ifelse(inside, "forestgreen", "red"),
      size = 3
    ) +
    
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    
    theme_bw() +
    
    labs(
      title = site,
      subtitle = paste0(
        "Inside watershed: ",
        inside,
        "   Distance = ",
        round(dist_m,1),
        " m"
      )
    )
  
  ggsave(
    filename = file.path(out_dir,
                         paste0(site, "_QC.png")),
    plot = p,
    width = 6,
    height = 6,
    dpi = 300
  )
  
}


#









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
