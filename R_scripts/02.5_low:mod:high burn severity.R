# =================================== Objectives =================================
#
# Script : 02.5_low/mod/high burn severity for papers outside of 1984-2021.R
# Project: rc_sfa-rc-3-wenas-meta
# Purpose: Calculate burn severity for low/moderate/high for papers in the meta analysis that are beyond the 1984-2021 time period
#
# Inputs :
#   - 
#   - 
# Output :
#   - 
#
#
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 4 June 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")
rm(list=ls(all=T)) #this clears your Environment

library(pacman)
p_load(tidyverse,
       sf,
       terra,
       nhdplusTools,
       exactextractr,
       tidyterra,
       mapview)

# ---- HICKENBOTTOM --------------------------------------------------------
# MOSQUITO FIRE 
# ---- 1. Load MTBS Fire Perimeter ----
# Read in moquito fire 
mtbs_perims <- st_read("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/gis_data/Fire_Perimeters/mosquito/mtbs/2022/ca3900612074520220907/mosquito_burn_bndy.shp")

# ---- 2. Load MTBS Burn Severity Raster ----
# Download the specific fire bundle from MTBS, then load the *_dnbr6.tif
# Classes: 1=Unburned/Low, 2=Low, 3=Moderate, 4=High, 5=Increased Greenness, 6=NoData/Mask
severity <- rast("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/gis_data/Fire_Perimeters/mosquito/mtbs/2022/ca3900612074520220907/mosquito_dnbr.tif")

# ---- 3. Get the NHD Catchment by COMID ----
my_comid <- "14997923"

# Option A: Just the local catchment for that COMID
catchment <- get_nhdplus(comid = my_comid, realization = "catchment") %>%
  st_make_valid()

# Pull the full upstream contributing watershed
catchment <- get_nldi_basin(
  nldi_feature = list(featureSource = "comid", featureID = my_comid)
) %>%
  st_make_valid()

# Quick check
plot(st_geometry(catchment), col = "lightblue", main = "Contributing Watershed")

# ---- 4. Align CRS (use equal-area for accurate area calcs) ----
target_crs <- 5070  # CONUS Albers Equal Area
catchment <- st_transform(catchment, target_crs)
mtbs_perims <- st_transform(mtbs_perims, target_crs)
severity <- project(severity, paste0("EPSG:", target_crs), method = "near")

# ---- 5. PLOT ----
mapview(catchment, col.regions = "lightblue", alpha.regions = 0.3,
        layer.name = "Watershed") +
  mapview(mtbs_perims, col.regions = "orange", alpha.regions = 0.3,
          layer.name = "Fire Perimeter")
  # mapview(burned_in_catch, col.regions = "red", alpha.regions = 0.5,
  #         layer.name = "Burned in Watershed") +
  # mapview(sev_in_burn, layer.name = "Burn Severity")

# ---- 6. Percent of Catchment Burned ----
# Use an equal-area CRS for accurate area calculations (CONUS Albers)
target_crs <- 5070

mtbs_perims <- st_transform(mtbs_perims, target_crs)
catchment   <- st_transform(catchment,   target_crs)

# Now this will work
burned_in_catch <- st_intersection(mtbs_perims, catchment) %>%
  st_make_valid()

catch_area_m2  <- as.numeric(st_area(catchment))
burned_area_m2 <- as.numeric(sum(st_area(burned_in_catch)))

pct_catchment_burned <- (burned_area_m2 / catch_area_m2) * 100

cat("Catchment area (ha):     ", round(catch_area_m2 / 10000, 1), "\n")
cat("Burned area (ha):        ", round(burned_area_m2 / 10000, 1), "\n")
cat("Percent burned:          ", round(pct_catchment_burned, 2), "%\n")

# ---- 6. Percent Low / Moderate / High Severity within Burned Area ----
sev_in_burn <- severity %>%
  crop(vect(burned_in_catch)) %>%
  mask(vect(burned_in_catch))

sev_freq <- freq(sev_in_burn) %>%
  as_tibble() %>%
  filter(!is.na(value)) %>%
  mutate(class = case_when(
    value == 1 ~ "Unburned/Low",
    value == 2 ~ "Low",
    value == 3 ~ "Moderate",
    value == 4 ~ "High",
    value == 5 ~ "Increased Greenness",
    value == 6 ~ "Non-mapping/Mask",
    TRUE       ~ as.character(value)
  ))

severity_summary <- sev_freq %>%
  filter(class %in% c("Low", "Moderate", "High")) %>%
  mutate(pct_of_burned = count / sum(count) * 100,
         pct_of_catchment = count / sum(sev_freq$count) * 100)

print(severity_summary)


