# =================================== Objectives =================================
#
# Script: 07_exploratory_concentration_area_ratios.R
# Purpose: Plot burned/unburned concentration ratios against burned/unburned
#          watershed area ratios for DOC and nitrate.
# Input:
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
# Output:
#   - Output_for_analysis/07_concentration_area_ratios/
#       - concentration_area_ratio_data.csv
#       - figures/scatter_DOC_concentration_ratio_vs_watershed_area_ratio.png
#       - figures/scatter_NO3_concentration_ratio_vs_watershed_area_ratio.png
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
      c(Area_watershed_km, DOC_Interp_mg_C_L, NO3_Interp_mg_N_L),
      ~ suppressWarnings(as.numeric(.))
    )
  )

# ---- 2. Output directories -------------------------------------------------
out_dir <- here("Output_for_analysis", "07_concentration_area_ratios")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. Build paired burn/unburn ratio data --------------------------------
finite_or_na <- function(x) {
  ifelse(is.finite(x), x, NA_real_)
}

make_ratio_data <- function(df, response_col, solute_label) {
  df %>%
    filter(!is.na(Comparison_ID)) %>%
    mutate(concentration = .data[[response_col]]) %>%
    group_by(Study_ID, Comparison_ID, Sampling_Date, Burn_Unburn) %>%
    summarise(
      concentration = mean(concentration, na.rm = TRUE),
      Area_watershed_km = median(Area_watershed_km, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      concentration = finite_or_na(concentration),
      Area_watershed_km = finite_or_na(Area_watershed_km)
    ) %>%
    pivot_wider(
      id_cols = c(Study_ID, Comparison_ID, Sampling_Date),
      names_from = Burn_Unburn,
      values_from = c(concentration, Area_watershed_km),
      names_glue = "{.value}_{Burn_Unburn}"
    ) %>%
    filter(
      concentration_Burn > 0,
      concentration_Unburn > 0,
      Area_watershed_km_Burn > 0,
      Area_watershed_km_Unburn > 0
    ) %>%
    mutate(
      solute = solute_label,
      concentration_ratio = concentration_Burn / concentration_Unburn,
      watershed_area_ratio = Area_watershed_km_Burn / Area_watershed_km_Unburn
    ) %>%
    filter(
      is.finite(concentration_ratio),
      is.finite(watershed_area_ratio)
    ) %>%
    select(
      solute, Study_ID, Comparison_ID, Sampling_Date,
      concentration_Burn, concentration_Unburn, concentration_ratio,
      Area_watershed_km_Burn, Area_watershed_km_Unburn, watershed_area_ratio
    )
}

ratio_data <- bind_rows(
  make_ratio_data(merged, "DOC_Interp_mg_C_L", "DOC"),
  make_ratio_data(merged, "NO3_Interp_mg_N_L", "NO3")
)

write_csv(ratio_data, file.path(out_dir, "concentration_area_ratio_data.csv"))

ratio_summary <- ratio_data %>%
  group_by(solute) %>%
  summarise(
    n_ratio_observations = n(),
    n_comparisons = n_distinct(Comparison_ID),
    n_studies = n_distinct(Study_ID),
    .groups = "drop"
  )

print(ratio_summary)

# ---- 4. Plot concentration ratio by watershed area ratio --------------------
make_ratio_plot <- function(df, solute_label, point_color) {
  sub <- df %>%
    filter(solute == solute_label)

  p <- ggplot(
    sub,
    aes(x = watershed_area_ratio, y = concentration_ratio)
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey45") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
    geom_point(alpha = 0.35, size = 1.4, color = point_color) +
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
      title = paste0(solute_label, " Concentration Ratio vs Watershed Area Ratio"),
      x = "Watershed area ratio (burned / unburned; log10 scale)",
      y = paste0(solute_label, " concentration ratio (burned / unburned; log10 scale)")
    ) +
    theme_bw(base_size = 11)

  ggsave(
    file.path(
      fig_dir,
      paste0(
        "scatter_", solute_label,
        "_concentration_ratio_vs_watershed_area_ratio.png"
      )
    ),
    p,
    width = 6,
    height = 4.5,
    dpi = 200
  )

  if (interactive()) {
    print(p)
  }

  p
}

doc_ratio_plot <- make_ratio_plot(ratio_data, "DOC", "#0072B2")
no3_ratio_plot <- make_ratio_plot(ratio_data, "NO3", "#009E73")
