# =================================== Objectives =================================
#
# Script: 06_exploratory_no3_area_unburned.R
# Purpose: Plot nitrate concentration against watershed area for unburned watersheds.
# Input:
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
# Output:
#   - Output_for_analysis/06_no3_area_unburned/figures/
#       - scatter_NO3_vs_watershed_area_unburned.png
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 8 May 2026

rm(list = ls(all = TRUE))

library(pacman)
p_load(
  tidyverse,
  here,
  scales
)

# ---- 1. Read merged master -------------------------------------------------
merged <- read_csv(
  here("Output_for_analysis", "03_merge_geospatial", "03_master_merged.csv"),
  na = c("", "NA", "-9999", "N/A"),
  show_col_types = FALSE
) %>%
  mutate(
    across(
      c(Area_watershed_km, NO3_Interp_mg_N_L),
      ~ suppressWarnings(as.numeric(.))
    )
  )

# ---- 2. Filter to unburned watersheds --------------------------------------
unburned_no3_area <- merged %>%
  filter(Burn_Unburn == "Unburn") %>%
  select(Study_ID, Site, Sampling_Date, Area_watershed_km, NO3_Interp_mg_N_L) %>%
  drop_na(Area_watershed_km, NO3_Interp_mg_N_L) %>%
  filter(Area_watershed_km > 0, NO3_Interp_mg_N_L > 0)

message(
  "Unburned NO3-area data: ", nrow(unburned_no3_area), " rows, ",
  n_distinct(unburned_no3_area$Site), " sites, ",
  n_distinct(unburned_no3_area$Study_ID), " studies."
)

# ---- 3. Output directory ----------------------------------------------------
out_dir <- here("Output_for_analysis", "06_no3_area_unburned")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 4. Plot nitrate by watershed area -------------------------------------
no3_area_plot <- ggplot(
  unburned_no3_area,
  aes(x = Area_watershed_km, y = NO3_Interp_mg_N_L)
) +
  geom_point(alpha = 0.35, size = 1.4, color = "#009E73") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_x_log10(
    labels = scales::label_number(accuracy = 0.01),
    breaks = scales::breaks_log()
  ) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 0.001),
    breaks = scales::breaks_log()
  ) +
  annotation_logticks(sides = "bl") +
  labs(
    title = "Nitrate vs Watershed Area in Unburned Watersheds",
    x = expression("Watershed area (km"^2*"; log10 scale)"),
    y = "NO3 (mg N/L; log10 scale)"
  ) +
  theme_bw(base_size = 11)

ggsave(
  file.path(fig_dir, "scatter_NO3_vs_watershed_area_unburned.png"),
  no3_area_plot,
  width = 6,
  height = 4.5,
  dpi = 200
)

if (interactive()) {
  print(no3_area_plot)
}
