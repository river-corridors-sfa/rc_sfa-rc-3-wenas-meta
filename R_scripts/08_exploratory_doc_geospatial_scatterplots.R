# =================================== Objectives =================================
#
# Script: 08_exploratory_doc_geospatial_scatterplots.R
# Purpose: Plot DOC concentration against numeric geospatial variables in the
#          merged master file.
# Input:
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
#   - Output_for_analysis/02_geospatial_comid_extraction/
#       geospatial_variables_bp_severity_pull.csv
# Output:
#   - Output_for_analysis/08_doc_geospatial_scatterplots/
#       - doc_geospatial_scatterplot_summary.csv
#       - figures/scatter_DOC_vs_<geospatial_variable>.png
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 11 May 2026

rm(list = ls(all = TRUE))

library(pacman)
p_load(
  tidyverse,
  here,
  scales
)

# ---- 1. Read merged master -------------------------------------------------
merged_path <- here(
  "Output_for_analysis", "03_merge_geospatial", "03_master_merged.csv"
)

geo_source_path <- here(
  "Output_for_analysis", "02_geospatial_comid_extraction",
  "geospatial_variables_bp_severity_pull.csv"
)

merged <- read_csv(
  merged_path,
  na = c("", "NA", "-9999", "-9998", "N/A"),
  show_col_types = FALSE
)

geo_source_cols <- read_csv(
  geo_source_path,
  n_max = 0,
  show_col_types = FALSE
) %>%
  names()

# The source geospatial file uses "site", while 03_master_merged uses "Site".
geo_source_cols <- str_replace(geo_source_cols, "^site$", "Site")

# Include geospatial variables from the source pull plus geospatial fields that
# came from the time-series metadata.
candidate_geo_vars <- c(
  "latitude",
  "longitude",
  "Area_watershed_km",
  setdiff(geo_source_cols, c("Site", "latitude", "longitude"))
) %>%
  unique() %>%
  intersect(names(merged))

doc_var <- "DOC_Interp_mg_C_L"

merged <- merged %>%
  mutate(across(all_of(c(doc_var, candidate_geo_vars)), ~ suppressWarnings(as.numeric(.))))

# ---- 2. Output directories -------------------------------------------------
out_dir <- here("Output_for_analysis", "08_doc_geospatial_scatterplots")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. Helpers ------------------------------------------------------------
safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

axis_label <- function(variable, log_scale = FALSE) {
  label <- case_when(
    variable == "latitude" ~ "Latitude",
    variable == "longitude" ~ "Longitude",
    variable == "Area_watershed_km" ~ "Watershed area (km2)",
    variable == "burn_percent_fire_year" ~ "Watershed burned in fire year (%)",
    variable == "burn_sev_high" ~ "High burn severity (%)",
    variable == "burn_sev_mod" ~ "Moderate burn severity (%)",
    variable == "burn_sev_low" ~ "Low burn severity (%)",
    variable == "burn_sev_NA" ~ "Unclassified burn severity (%)",
    variable == "totdasqkm" ~ "Total drainage area (km2)",
    variable == "areasqkm" ~ "Flowline catchment area (km2)",
    variable == "slope" ~ "Flowline slope (m/m)",
    variable == "lengthkm" ~ "Flowline length (km)",
    variable == "shape_length" ~ "Flowline shape length",
    variable == "maxelevraw" ~ "Maximum elevation, raw",
    variable == "minelevraw" ~ "Minimum elevation, raw",
    variable == "maxelevsmo" ~ "Maximum elevation, smoothed",
    variable == "minelevsmo" ~ "Minimum elevation, smoothed",
    TRUE ~ variable
  )

  if (log_scale) {
    label <- paste0(label, " (log10 scale)")
  }

  label
}

safe_min <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  median(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

summarise_plot_input <- function(df, variable) {
  sub <- df %>%
    select(Study_ID, Site, Burn_Unburn, all_of(c(doc_var, variable))) %>%
    drop_na(all_of(c(doc_var, variable))) %>%
    filter(.data[[doc_var]] > 0)

  if (!is.numeric(sub[[variable]])) {
    return(tibble(
      variable = variable,
      plotted = FALSE,
      skip_reason = "not numeric",
      n_rows = nrow(sub),
      n_sites = n_distinct(sub$Site),
      n_studies = n_distinct(sub$Study_ID),
      x_distinct = n_distinct(sub[[variable]]),
      x_min = NA_real_,
      x_median = NA_real_,
      x_max = NA_real_,
      x_scale = NA_character_,
      spearman_r = NA_real_,
      spearman_p = NA_real_,
      plot_file = NA_character_
    ))
  }

  x <- sub[[variable]]
  n_rows <- nrow(sub)
  x_distinct <- n_distinct(x)
  x_min <- safe_min(x)
  x_median <- safe_median(x)
  x_max <- safe_max(x)

  if (n_rows < 3) {
    skip_reason <- "fewer than 3 complete positive-DOC rows"
  } else if (x_distinct < 2) {
    skip_reason <- "fewer than 2 distinct x values"
  } else {
    skip_reason <- NA_character_
  }

  can_log_x <- n_rows > 0 &&
    !is.na(x_min) &&
    !is.na(x_max) &&
    x_min > 0 &&
    (x_max / x_min >= 100)

  cor_test <- if (is.na(skip_reason)) {
    suppressWarnings(cor.test(x, sub[[doc_var]], method = "spearman"))
  } else {
    NULL
  }

  tibble(
    variable = variable,
    plotted = is.na(skip_reason),
    skip_reason = skip_reason,
    n_rows = n_rows,
    n_sites = n_distinct(sub$Site),
    n_studies = n_distinct(sub$Study_ID),
    x_distinct = x_distinct,
    x_min = x_min,
    x_median = x_median,
    x_max = x_max,
    x_scale = ifelse(can_log_x, "log10", "linear"),
    spearman_r = ifelse(is.null(cor_test), NA_real_, unname(cor_test$estimate)),
    spearman_p = ifelse(is.null(cor_test), NA_real_, cor_test$p.value),
    plot_file = ifelse(
      is.na(skip_reason),
      file.path("figures", paste0("scatter_DOC_vs_", safe_filename(variable), ".png")),
      NA_character_
    )
  )
}

make_doc_scatter <- function(df, variable, x_scale) {
  sub <- df %>%
    select(Study_ID, Site, Burn_Unburn, all_of(c(doc_var, variable))) %>%
    drop_na(all_of(c(doc_var, variable))) %>%
    filter(.data[[doc_var]] > 0)

  p <- ggplot(
    sub,
    aes(x = .data[[variable]], y = .data[[doc_var]])
  ) +
    geom_point(aes(color = Burn_Unburn), alpha = 0.35, size = 1.4) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black",
      linewidth = 0.8
    ) +
    scale_y_log10(
      labels = scales::label_number(accuracy = 0.01),
      breaks = scales::breaks_log()
    ) +
    annotation_logticks(sides = "l") +
    scale_color_manual(
      values = c("Burn" = "#D55E00", "Unburn" = "#0072B2"),
      na.value = "grey55"
    ) +
    labs(
      title = paste0("DOC vs ", axis_label(variable)),
      subtitle = paste0(
        "n = ", nrow(sub), " observations, ",
        n_distinct(sub$Site), " sites, ",
        n_distinct(sub$Study_ID), " studies"
      ),
      x = axis_label(variable, log_scale = x_scale == "log10"),
      y = "DOC (mg C/L; log10 scale)",
      color = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  if (x_scale == "log10") {
    p <- p +
      scale_x_log10(
        labels = scales::label_number(accuracy = 0.01),
        breaks = scales::breaks_log()
      ) +
      annotation_logticks(sides = "b")
  }

  ggsave(
    file.path(fig_dir, paste0("scatter_DOC_vs_", safe_filename(variable), ".png")),
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

# ---- 4. Build one DOC scatter plot per numeric geospatial variable ----------
plot_summary <- map_dfr(candidate_geo_vars, ~ summarise_plot_input(merged, .x))

walk2(
  plot_summary$variable[plot_summary$plotted],
  plot_summary$x_scale[plot_summary$plotted],
  ~ make_doc_scatter(merged, .x, .y)
)

write_csv(
  plot_summary,
  file.path(out_dir, "doc_geospatial_scatterplot_summary.csv")
)

message(
  "Plotted ", sum(plot_summary$plotted), " DOC-geospatial scatterplots. ",
  "Skipped ", sum(!plot_summary$plotted), " variables. ",
  "Summary written to: ",
  file.path(out_dir, "doc_geospatial_scatterplot_summary.csv")
)
