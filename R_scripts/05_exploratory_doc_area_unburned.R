# =================================== Objectives =================================
#
# Script: 05_exploratory_doc_area_unburned.R
# Purpose: Plot DOC concentration against watershed area for unburned watersheds.
# Input:
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
# Output:
#   - Output_for_analysis/05_doc_area_unburned/figures/
#       - scatter_DOC_vs_watershed_area_unburned.png
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
      c(Area_watershed_km, DOC_Interp_mg_C_L),
      ~ suppressWarnings(as.numeric(.))
    )
  )

# ---- 2. Filter to unburned watersheds --------------------------------------
unburned_doc_area <- merged %>%
  filter(Burn_Unburn == "Unburn") %>%
  select(Study_ID, Site, Sampling_Date, Area_watershed_km, DOC_Interp_mg_C_L) %>%
  drop_na(Area_watershed_km, DOC_Interp_mg_C_L) %>%
  filter(Area_watershed_km > 0, DOC_Interp_mg_C_L > 0)

message(
  "Unburned DOC-area data: ", nrow(unburned_doc_area), " rows, ",
  n_distinct(unburned_doc_area$Site), " sites, ",
  n_distinct(unburned_doc_area$Study_ID), " studies."
)

# ---- 3. Output directory ----------------------------------------------------
out_dir <- here("Output_for_analysis", "05_doc_area_unburned")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 4. Plot DOC by watershed area -----------------------------------------
doc_area_plot <- ggplot(
  unburned_doc_area,
  aes(x = Area_watershed_km, y = DOC_Interp_mg_C_L)
) +
  geom_point(alpha = 0.35, size = 1.4, color = "#0072B2") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  scale_x_log10(
    labels = scales::label_number(accuracy = 0.01),
    breaks = scales::breaks_log()
  ) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 0.01),
    breaks = scales::breaks_log()
  ) +
  annotation_logticks(sides = "bl") +
  labs(
    title = "DOC vs Watershed Area in Unburned Watersheds",
    x = expression("Watershed area (km"^2*"; log10 scale)"),
    y = "DOC (mg C/L; log10 scale)"
  ) +
  theme_bw(base_size = 11)

ggsave(
  file.path(fig_dir, "scatter_DOC_vs_watershed_area_unburned.png"),
  doc_area_plot,
  width = 6,
  height = 4.5,
  dpi = 200
)

if (interactive()) {
  print(doc_area_plot)
}
