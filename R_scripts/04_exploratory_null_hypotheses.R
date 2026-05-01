# =================================== Objectives =================================
# 
# Script: 04_exploratory_null_hypotheses.R
# Purpose: Explore distributions and relationships between DOC/nitrate and key
#          catchment predictors (watershed area, burn %, climate, TSF, burn
#          severity). Produces visualizations and formal statistical tests for
#          the null hypothesis that DOC/NO3 do NOT vary with these predictors.
# Input :
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
# Output :
#   - Output_for_analysis/04_null_hypothesis_exploration/
#       - figures/  (one PNG per predictor × response combination)
#       - 04_null_hypothesis_test_results.csv
#       - 04_summary_statistics.csv
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 1 May 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")
rm(list=ls(all=T)) #this clears your Environment

library(pacman)
p_load(tidyverse,
       here,
       broom,
       ggpubr,
       scales)

# ---- 1. Read merged master -------------------------------------------------
merged <- read_csv(
  here("Output_for_analysis", "03_merge_geospatial", "03_master_merged.csv"),
  na = c("", "NA", "-9999", "N/A"),
  show_col_types = FALSE
) %>%
  # Force numeric types on response and key predictors (in case of chr "NA" leftovers)
  mutate(across(c(DOC_Interp_mg_C_L, NO3_Interp_mg_N_L,
                  Area_watershed_km, burn_percent_fire_year,
                  burn_sev_high, burn_sev_mod, burn_sev_low,
                  Time_Since_Fire),
                ~ suppressWarnings(as.numeric(.))))

message("Merged data: ", nrow(merged), " rows, ",
        n_distinct(merged$Site), " sites, ",
        n_distinct(merged$Study_ID), " studies.")

# ---- 2. Define variables ---------------------------------------------------
response_vars <- c("DOC_Interp_mg_C_L", "NO3_Interp_mg_N_L")

# Predictors that benefit from log-x scaling (span orders of magnitude)
log_predictors <- c("Area_watershed_km", "burn_percent_fire_year",
                    "burn_sev_high", "burn_sev_mod", "burn_sev_low")

# Predictors kept on linear x-scale
linear_predictors <- c("Time_Since_Fire")

continuous_predictors <- c(log_predictors, linear_predictors)
categorical_predictors <- c("Climate", "Burn_Unburn")

# ---- 3. Output directories -------------------------------------------------
out_dir <- here("Output_for_analysis", "04_null_hypothesis_exploration")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# SECTION A: Summary statistics (calculated on raw, reported in both scales)
# ==============================================================================

summary_stats <- merged %>%
  group_by(Study_ID, Site, Burn_Unburn) %>%
  summarise(
    n_obs      = sum(!is.na(DOC_Interp_mg_C_L) | !is.na(NO3_Interp_mg_N_L)),
    DOC_median = median(DOC_Interp_mg_C_L, na.rm = TRUE),
    DOC_mean   = mean(DOC_Interp_mg_C_L, na.rm = TRUE),
    DOC_sd     = sd(DOC_Interp_mg_C_L, na.rm = TRUE),
    NO3_median = median(NO3_Interp_mg_N_L, na.rm = TRUE),
    NO3_mean   = mean(NO3_Interp_mg_N_L, na.rm = TRUE),
    NO3_sd     = sd(NO3_Interp_mg_N_L, na.rm = TRUE),
    .groups = "drop"
  )

# write_csv(summary_stats, file.path(out_dir, "04_summary_statistics.csv"))

# ==============================================================================
# SECTION B: Continuous predictors (scatter plots + Spearman correlations)
# ==============================================================================
# Spearman is rank-based so it's scale-invariant — running on raw values is fine.
# We log-transform axes only for visualization.

test_continuous <- function(df, response, predictor){
  sub <- df %>% dplyr::select(all_of(c(response, predictor))) %>% drop_na()
  if(nrow(sub) < 3) return(NULL)
  
  sp <- suppressWarnings(cor.test(sub[[predictor]], sub[[response]],
                                  method = "spearman"))
  
  # Also fit on log-log (where applicable) for an R² that matches the plot
  sub_log <- sub %>%
    filter(.data[[response]] > 0)
  if(predictor %in% log_predictors) sub_log <- sub_log %>% filter(.data[[predictor]] > 0)
  
  lm_r2 <- NA_real_
  lm_p  <- NA_real_
  if(nrow(sub_log) >= 3){
    xform <- if(predictor %in% log_predictors) paste0("log10(", predictor, ")") else predictor
    f <- as.formula(paste0("log10(", response, ") ~ ", xform))
    fit <- try(lm(f, data = sub_log), silent = TRUE)
    if(!inherits(fit, "try-error")){
      g <- broom::glance(fit)
      lm_r2 <- g$r.squared
      lm_p  <- g$p.value
    }
  }
  
  tibble(
    response    = response,
    predictor   = predictor,
    n           = nrow(sub),
    spearman_r  = unname(sp$estimate),
    spearman_p  = sp$p.value,
    log_lm_r2   = lm_r2,
    log_lm_p    = lm_p,
    null_reject = spearman_p < 0.05
  )
}

continuous_results <- expand_grid(
  response  = response_vars,
  predictor = continuous_predictors
) %>%
  pmap_dfr(~ test_continuous(merged, ..1, ..2))

print(continuous_results)

# ---- Scatter plots with log-scaled axes ------------------------------------

make_scatter <- function(df, response, predictor){
  sub <- df %>%
    dplyr::select(all_of(c(response, predictor, "Study_ID", "Burn_Unburn"))) %>%
    drop_na(all_of(c(response, predictor))) %>%
    filter(.data[[response]] > 0)
  
  if(predictor %in% log_predictors){
    sub <- sub %>% filter(.data[[predictor]] > 0)
  }
  
  if(nrow(sub) < 3) return(NULL)
  
  p <- ggplot(sub, aes(x = .data[[predictor]], y = .data[[response]])) +
    geom_point(aes(color = Burn_Unburn), alpha = 0.3, size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    ggpubr::stat_cor(method = "spearman", label.x.npc = 0.05,
                     label.y.npc = 0.95, size = 3.5) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.01),
                  breaks = scales::breaks_log()) +                   # <-- UPDATED
    annotation_logticks(sides = "l") +
    scale_color_manual(values = c("Burn" = "#D55E00", "Unburn" = "#0072B2")) +
    labs(
      title    = paste0(response, " vs ", predictor),
      subtitle = "Null: no monotonic relationship | Spearman shown",
      x = predictor, y = paste0(response, " (log10)"), color = NULL
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  if(predictor %in% log_predictors){
    p <- p +
      scale_x_log10(labels = scales::label_number(accuracy = 0.01),
                    breaks = scales::breaks_log()) +                  # <-- UPDATED
      annotation_logticks(sides = "b") +
      labs(x = paste0(predictor, " (log10)"))
  }
  
  ggsave(file.path(fig_dir, paste0("scatter_", response, "_vs_", predictor, ".png")),
         p, width = 6, height = 4.5, dpi = 200)
  p
}

walk2(
  rep(response_vars, each = length(continuous_predictors)),
  rep(continuous_predictors, times = length(response_vars)),
  ~ make_scatter(merged, .x, .y)
)

# ==============================================================================
# SECTION C: Categorical predictors (boxplots with log-y)
# ==============================================================================

test_categorical <- function(df, response, predictor){
  sub <- df %>% dplyr::select(all_of(c(response, predictor))) %>% drop_na()
  if(n_distinct(sub[[predictor]]) < 2 || nrow(sub) < 3) return(NULL)
  
  kw <- kruskal.test(as.formula(paste(response, "~", predictor)), data = sub)
  
  tibble(
    response    = response,
    predictor   = predictor,
    n           = nrow(sub),
    n_groups    = n_distinct(sub[[predictor]]),
    kw_chisq    = unname(kw$statistic),
    kw_p        = kw$p.value,
    null_reject = kw_p < 0.05
  )
}

categorical_results <- expand_grid(
  response  = response_vars,
  predictor = categorical_predictors
) %>%
  pmap_dfr(~ test_categorical(merged, ..1, ..2))

print(categorical_results)

make_box <- function(df, response, predictor){
  sub <- df %>%
    dplyr::select(all_of(c(response, predictor))) %>%
    drop_na() %>%
    filter(.data[[response]] > 0)
  if(nrow(sub) < 3) return(NULL)
  
  p <- ggplot(sub, aes(x = .data[[predictor]], y = .data[[response]],
                       fill = .data[[predictor]])) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
    ggpubr::stat_compare_means(method = "kruskal.test", size = 3.5,
                               label.y.npc = 0.95) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.01),
                  breaks = scales::breaks_log()) +                   # <-- UPDATED
    annotation_logticks(sides = "l") +
    labs(
      title    = paste0(response, " by ", predictor),
      subtitle = "Null: equal medians across groups (Kruskal-Wallis)",
      x = predictor, y = paste0(response, " (log10)")
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggsave(file.path(fig_dir, paste0("box_", response, "_by_", predictor, ".png")),
         p, width = 6, height = 4.5, dpi = 200)
  p
}

walk2(
  rep(response_vars, each = length(categorical_predictors)),
  rep(categorical_predictors, times = length(response_vars)),
  ~ make_box(merged, .x, .y)
)

# ==============================================================================
# SECTION D: Time-Since-Fire binned (log-y concentrations)
# ==============================================================================

# merged_tsf <- merged %>%
#   mutate(TSF_bin = cut(Time_Since_Fire,
#                        breaks = c(-Inf, 0, 1, 3, 5, 10, Inf