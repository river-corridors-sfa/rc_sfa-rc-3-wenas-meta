# =================================== Objectives =================================
# 
# Script: 04_calculate_effect_sizes.R
# Purpose: Calculate log-response-ratio (lnR) effect sizes comparing burned
#          to unburned (control) watersheds for each unique burned–unburned
#          pair defined upstream in 01_read_and_gapfill.R.
#
#          lnR = log( C_burned / C_unburned )
#
#          Two flavors are computed:
#            (a) non-normalized : raw daily concentrations
#            (b) area-normalized: daily concentrations divided by the
#                                 respective watershed area (km^2) before
#                                 taking the ratio
#
#          Effect sizes are saved as their own long-format data frame and
#          also joined back onto the master merged file for convenience.
# Input :
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
# Output :
# 
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 11 May 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")
rm(list=ls(all=T)) #this clears your Environment

library(pacman)
p_load(tidyverse,
       here,
       lubridate,
       ggridges,
       patchwork)


# ---- 1. Load master merged data -------------------------------------------
master_merged <- read_csv(
  "Output_for_analysis/03_merge_geospatial/03_master_merged.csv"
)


# 2. Put chemistry in long format so each row is one solute observation -----
#    (DOC and NO3 live in separate columns in the master file).
chem_long <- master_merged %>%
  select(Study_ID, Comparison_ID, Pair, Site,
         Sampling_Date, Burn_Unburn,
         Area_watershed_km,
         DOC_Interp_mg_C_L, NO3_Interp_mg_N_L) %>%
  pivot_longer(
    cols = c(DOC_Interp_mg_C_L, NO3_Interp_mg_N_L),
    names_to = "response_var",
    values_to = "concentration"
  ) %>%
  mutate(
    response_var = recode(response_var,
                          DOC_Interp_mg_C_L = "DOC",
                          NO3_Interp_mg_N_L = "NO3")
  ) %>%
  filter(!is.na(concentration))


# 3. Build burn–unburn pairings within each Comparison_ID -------------------
#    `Pair` identifies a specific site within a comparison (e.g., Control_1,
#    Site_1, Site_2). Within one Comparison_ID there can be >1 burned site
#    and/or >1 unburned site, so we do every Burn × Unburn combination.

# average any accidental duplicates for a given site/day/solute
chem_long_clean <- chem_long %>%
  group_by(Study_ID, Comparison_ID, Pair, Site,
           Sampling_Date, response_var, Burn_Unburn) %>%
  summarise(
    concentration = mean(concentration, na.rm = TRUE),
    Area_watershed_km = mean(Area_watershed_km, na.rm = TRUE),
    .groups = "drop"
  )

burn_side <- chem_long_clean %>%
  filter(Burn_Unburn == "Burn") %>%
  select(Study_ID, Comparison_ID, Sampling_Date, response_var,
         Pair_Burn = Pair,
         Site_Burn = Site,
         concentration_Burn = concentration,
         Area_watershed_km_Burn = Area_watershed_km)

unburn_side <- chem_long_clean %>%
  filter(Burn_Unburn == "Unburn") %>%
  select(Study_ID, Comparison_ID, Sampling_Date, response_var,
         Pair_Unburn  = Pair,
         Site_Unburn = Site,
         concentration_Unburn = concentration,
         Area_watershed_km_Unburn = Area_watershed_km)

paired <- burn_side %>%
  inner_join(
    unburn_side,
    by = c("Study_ID", "Comparison_ID", "Sampling_Date", "response_var"),
    relationship = "many-to-many"
  ) %>%
  # stable identifier for this specific burn–unburn pairing
  mutate(
    pair_key = paste(Study_ID, Comparison_ID,
                     Pair_Burn, Pair_Unburn, sep = " | ")
  )


# 4. Compute daily effect sizes ---------------------------------------------
effect_daily <- paired %>%
  mutate(
    # Guard against zero / negative values before taking logs
    valid_raw = !is.na(concentration_Burn) &
      !is.na(concentration_Unburn) &
      concentration_Burn   > 0 &
      concentration_Unburn > 0,
    
    valid_area = valid_raw &
      !is.na(Area_watershed_km_Burn) &
      !is.na(Area_watershed_km_Unburn) &
      Area_watershed_km_Burn > 0 &
      Area_watershed_km_Unburn > 0,
    
    # (a) non-normalized lnRR
    lnRR = if_else(
      valid_raw,
      log(concentration_Burn / concentration_Unburn),
      NA_real_
    ),
    
    # area-normalized concentrations (concentration per km^2 of watershed)
    conc_norm_Burn   = concentration_Burn   / Area_watershed_km_Burn,
    conc_norm_Unburn = concentration_Unburn / Area_watershed_km_Unburn,
    
    # (b) area-normalized lnRR
    lnRR_area = if_else(
      valid_area,
      log(conc_norm_Burn / conc_norm_Unburn),
      NA_real_
    )
  ) %>%
  select(-valid_raw, -valid_area)

# ---- 5. Aggregations -------------------------------------------------------
summarize_es <- function(df, ...) {
  df %>%
    group_by(Study_ID, Comparison_ID,
             Pair_Burn, Pair_Unburn, pair_key,
             response_var, ...) %>%
    summarise(
      lnRR_mean      = mean(lnRR,      na.rm = TRUE),
      lnRR_sd        = sd(lnRR,        na.rm = TRUE),
      lnRR_n         = sum(!is.na(lnRR)),
      lnRR_area_mean = mean(lnRR_area, na.rm = TRUE),
      lnRR_area_sd   = sd(lnRR_area,   na.rm = TRUE),
      lnRR_area_n    = sum(!is.na(lnRR_area)),
      .groups = "drop"
    ) %>%
    mutate(
      lnRR_var      = (lnRR_sd^2)      / lnRR_n,
      lnRR_area_var = (lnRR_area_sd^2) / lnRR_area_n
    )
}

effect_monthly <- effect_daily %>%
  mutate(year = year(Sampling_Date), month = month(Sampling_Date)) %>%
  summarize_es(year, month)

effect_yearly <- effect_daily %>%
  mutate(year = year(Sampling_Date)) %>%
  summarize_es(year)

effect_overall <- summarize_es(effect_daily)

# 6. Join daily effect sizes back onto the master merged file ---------------
#    master_merged has one row per SITE (keyed by Pair), while effect sizes
#    live at the PAIRING level (Pair_Burn × Pair_Unburn). A single site may
#    belong to multiple pairings, so we first average lnRR across pairings
#    for each site-day-solute, then join.
#
#    The site on the Burn side is matched via Pair_Burn; the site on the
#    Unburn side is matched via Pair_Unburn. Both joins are done, then
#    coalesced into single lnRR_* columns.

# --- effect sizes from the perspective of the BURN site -------------------
es_burn_side <- effect_daily %>%
  group_by(Study_ID, Comparison_ID, Pair_Burn,
           Sampling_Date, response_var) %>%
  summarise(
    lnRR           = mean(lnRR,           na.rm = TRUE),
    lnRR_area      = mean(lnRR_area,      na.rm = TRUE),
    conc_norm_self = mean(conc_norm_Burn, na.rm = TRUE),
    n_pairings     = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = response_var,
    values_from = c(lnRR, lnRR_area, conc_norm_self, n_pairings),
    names_glue  = "{.value}_{response_var}_burnSide"
  ) %>%
  rename(Pair = Pair_Burn)

# --- effect sizes from the perspective of the UNBURN site -----------------
es_unburn_side <- effect_daily %>%
  group_by(Study_ID, Comparison_ID, Pair_Unburn,
           Sampling_Date, response_var) %>%
  summarise(
    lnRR           = mean(lnRR,             na.rm = TRUE),
    lnRR_area      = mean(lnRR_area,        na.rm = TRUE),
    conc_norm_self = mean(conc_norm_Unburn, na.rm = TRUE),
    n_pairings     = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = response_var,
    values_from = c(lnRR, lnRR_area, conc_norm_self, n_pairings),
    names_glue  = "{.value}_{response_var}_unburnSide"
  ) %>%
  rename(Pair = Pair_Unburn)

# --- join both sides onto master_merged and coalesce ----------------------
master_merged_with_ES <- master_merged %>%
  left_join(es_burn_side,
            by = c("Study_ID", "Comparison_ID", "Pair", "Sampling_Date")) %>%
  left_join(es_unburn_side,
            by = c("Study_ID", "Comparison_ID", "Pair", "Sampling_Date")) %>%
  mutate(
    # DOC
    lnRR_DOC           = coalesce(lnRR_DOC_burnSide,           lnRR_DOC_unburnSide),
    lnRR_area_DOC      = coalesce(lnRR_area_DOC_burnSide,      lnRR_area_DOC_unburnSide),
    conc_norm_DOC      = coalesce(conc_norm_self_DOC_burnSide, conc_norm_self_DOC_unburnSide),
    n_pairings_DOC     = coalesce(n_pairings_DOC_burnSide,     n_pairings_DOC_unburnSide),
    # NO3
    lnRR_NO3           = coalesce(lnRR_NO3_burnSide,           lnRR_NO3_unburnSide),
    lnRR_area_NO3      = coalesce(lnRR_area_NO3_burnSide,      lnRR_area_NO3_unburnSide),
    conc_norm_NO3      = coalesce(conc_norm_self_NO3_burnSide, conc_norm_self_NO3_unburnSide),
    n_pairings_NO3     = coalesce(n_pairings_NO3_burnSide,     n_pairings_NO3_unburnSide)
  ) %>%
  # drop the intermediate side-specific columns
  select(-ends_with("_burnSide"), -ends_with("_unburnSide"))

# 7. Save outputs -----------------------------------------------------------
out_dir <- "Output_for_analysis/04_calculate_effect_sizes"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(effect_daily,              file.path(out_dir, "effect_sizes_daily.csv"))
write_csv(effect_monthly,            file.path(out_dir, "effect_sizes_monthly.csv"))
write_csv(effect_yearly,             file.path(out_dir, "effect_sizes_yearly.csv"))
write_csv(effect_overall,            file.path(out_dir, "effect_sizes_overall.csv"))
write_csv(master_merged_with_ES, file.path(out_dir, "04_master_merged_with_ES.csv"))

saveRDS(effect_daily,                file.path(out_dir, "effect_sizes_daily.rds"))
saveRDS(effect_monthly,              file.path(out_dir, "effect_sizes_monthly.rds"))
saveRDS(effect_yearly,               file.path(out_dir, "effect_sizes_yearly.rds"))
saveRDS(effect_overall,              file.path(out_dir, "effect_sizes_overall.rds"))
saveRDS(master_merged_with_ES,   file.path(out_dir, "04_master_merged_with_ES.rds"))

message("Effect sizes written to ", out_dir)

# 8. Data visualization  -----------------------------------------------------------
out_dir <- "Output_for_analysis/04_calculate_effect_sizes/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# Long version: one row per observation per metric (lnRR or lnRR_area)
effect_long <- effect_daily %>%
  select(Study_ID, Comparison_ID, Pair_Burn, Pair_Unburn,
         Sampling_Date, response_var, lnRR, lnRR_area) %>%
  pivot_longer(
    cols      = c(lnRR, lnRR_area),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric,
                    lnRR      = "Non-normalized",
                    lnRR_area = "Area-normalized")
  ) %>%
  filter(is.finite(value))

# A consistent theme
theme_set(theme_bw(base_size = 12))


# Overall density: non-normalized vs area-normalized ---------------------
p1 <- ggplot(effect_long, aes(x = value, fill = metric, color = metric)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ response_var, scales = "free_y") +
  labs(
    title = "Distribution of daily lnRR effect sizes",
    x = "lnRR  (log[Burn / Unburn])",
    y = "Density",
    fill = NULL, color = NULL
  ) +
  theme(legend.position = "bottom")
p1
# ggsave(file.path(out_dir, "01_density_metric_by_solute.png"),
#        p1, width = 9, height = 4.5, dpi = 300)

# Density by study (one panel per study) ---------------------------------
p3 <- ggplot(effect_long, aes(x = value, fill = metric, color = metric)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(response_var ~ Study_ID, scales = "free") +
  labs(
    title = "Daily lnRR density by study",
    x = "lnRR", y = "Density", fill = NULL, color = NULL
  ) +
  theme(strip.text.x = element_text(size = 8),
        legend.position = "bottom")
p3
ggsave(file.path(out_dir, "03_density_by_study.png"),
       p3, width = 16, height = 6, dpi = 300)

# Ridge plot: studies stacked, one metric at a time ----------------------
p4_raw <- effect_long %>%
  filter(metric == "Non-normalized") %>%
  ggplot(aes(x = value, y = fct_reorder(Study_ID, value, median, na.rm = TRUE),
             fill = response_var)) +
  geom_density_ridges(alpha = 0.6, scale = 1.1, rel_min_height = 0.01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Non-normalized lnRR by study (ridge plot)",
       x = "lnRR", y = NULL, fill = "Solute")

p4_raw

p4_area <- effect_long %>%
  filter(metric == "Area-normalized") %>%
  ggplot(aes(x = value, y = fct_reorder(Study_ID, value, median, na.rm = TRUE),
             fill = response_var)) +
  geom_density_ridges(alpha = 0.6, scale = 1.1, rel_min_height = 0.01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Area-normalized lnRR by study (ridge plot)",
       x = "lnRR (area-normalized)", y = NULL, fill = "Solute")

p4_area

ggsave(file.path(out_dir, "04a_ridges_raw.png"),
       p4_raw,  width = 9, height = 7, dpi = 300)
ggsave(file.path(out_dir, "04b_ridges_area.png"),
       p4_area, width = 9, height = 7, dpi = 300)

# Side-by-side combined
p4_combined <- p4_raw + p4_area
ggsave(file.path(out_dir, "04c_ridges_combined.png"),
       p4_combined, width = 16, height = 7, dpi = 300)

# Boxplot + jitter by study (compact summary with outliers visible) ------
p5 <- ggplot(effect_long,
             aes(x = fct_reorder(Study_ID, value, median, na.rm = TRUE),
                 y = value, fill = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.15, alpha = 0.6) +
  facet_wrap(~ response_var, ncol = 1, scales = "free") +
  coord_flip() +
  labs(title = "Boxplot of daily lnRR by study",
       x = NULL, y = "lnRR", fill = NULL)
p5
ggsave(file.path(out_dir, "05_box_by_study.png"),
       p5, width = 10, height = 9, dpi = 300)

# Violin: metric on x, value on y, faceted by study ----------------------
p6 <- ggplot(effect_long, aes(x = metric, y = value, fill = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  facet_grid(response_var ~ Study_ID, scales = "free_y") +
  labs(title = "Violin of daily lnRR by study & metric",
       x = NULL, y = "lnRR") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1),
        strip.text.x = element_text(size = 8))
p6
ggsave(file.path(out_dir, "06_violin_by_study.png"),
       p6, width = 16, height = 7, dpi = 300)

# ECDF: full distribution shape, no binning ------------------------------
p8 <- ggplot(effect_long, aes(x = value, color = metric)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ response_var, scales = "free_x") +
  labs(title = "Empirical CDF of daily lnRR",
       x = "lnRR", y = "Cumulative proportion", color = NULL)
p8
# ggsave(file.path(out_dir, "08_ecdf_metric_by_solute.png"),
#        p8, width = 9, height = 4.5, dpi = 300)



