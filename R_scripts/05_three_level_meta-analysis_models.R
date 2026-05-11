# =================================== Objectives =================================
# 
# Script: 05_three_level_meta-analysis_models.R
# Purpose: Fit three-level meta-analytic models to burn vs. unburn lnRR
#          effect sizes, using the tables built in 04_calculate_effect_sizes.R.
#          Workflow follows Harrer et al. "Doing Meta-Analysis in R" Ch. on
#          Multilevel Meta-Analysis (https://doing-meta.guide/multilevel-ma).
# Nesting (3-level hierarchy):
#   Level 1: sampling variance of each lnRR  (vi, fixed)
#   Level 2: pairings within a comparison    (pair_key)
#   Level 3: comparisons within a study      (Comparison_ID)
#   Level 4 (optional 4-level): studies      (Study_ID)
#
#   -> We fit as: random = ~ 1 | Study_ID / Comparison_ID / pair_key
#      which gives 3 between-cluster variance components plus level-1.
#
# Two versions of lnRR are fit in parallel:
#   - non-normalized  (lnRR_mean,      vi = lnRR_var)
#   - area-normalized (lnRR_area_mean, vi = lnRR_area_var)
#
# Separate models are fit for DOC and NO3.
# Input :
#   - Output_for_analysis/04_calculate_effect_sizes/04_master_merged_with_ES
# Output :
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
       metafor,
       clubSandwich)

# ---- 1. Load effect sizes + master (for moderators) -----------------------
es_dir <- "Output_for_analysis/04_calculate_effect_sizes"

effect_overall <- read_csv(file.path(es_dir, "effect_sizes_overall.csv"))
effect_yearly <- read_csv(file.path(es_dir, "effect_sizes_yearly.csv"))
master_ES <- read_csv(file.path(es_dir, "04_master_merged_with_ES.csv"))

# 2. Build the analysis table -----------------------------------------------
#    One row per pairing × solute. The moderators describe the BURNED
#    watershed (that's where the disturbance acts), pulled from the first
#    Burn row of each Pair_Burn site.

# Moderators to consider
moderators <- c(
  "Climate", "Time_Since_Fire", "Area_watershed_km",
  "burn_percent_fire_year",
  "burn_sev_high", "burn_sev_mod", "burn_sev_low",
  "slope", "elevfixed",
  "qa_ma", "va_ma"
)
# Keep only the ones actually present
moderators <- intersect(moderators, names(master_ES))

# Metadata per Burn site (one row per Study_ID + Comparison_ID + Pair)
burn_site_meta <- master_ES %>%
  filter(Burn_Unburn == "Burn") %>%
  group_by(Study_ID, Comparison_ID, Pair) %>%
  summarise(across(all_of(moderators),
                   ~ {
                     if (is.numeric(.)) mean(., na.rm = TRUE)
                     else               first(na.omit(.))
                   }),
            .groups = "drop") %>%
  rename(Pair_Burn = Pair)

# --- Assemble the meta-analysis data (overall: one lnRR per pair) ---------
meta_dat <- effect_overall %>%
  mutate(pair_key = paste(Study_ID, Comparison_ID,
                          Pair_Burn, Pair_Unburn, sep = " | ")) %>%
  left_join(burn_site_meta,
            by = c("Study_ID", "Comparison_ID", "Pair_Burn")) %>%
  # require at least 2 daily obs so we have a real variance
  filter(lnRR_n >= 2,
         is.finite(lnRR_mean), is.finite(lnRR_var), lnRR_var > 0) %>%
  mutate(
    Study_ID      = factor(Study_ID),
    Comparison_ID = factor(Comparison_ID),
    pair_key      = factor(pair_key),
    es_id         = factor(row_number())
  )

# Split by solute
dat_DOC <- meta_dat %>% filter(response_var == "DOC")
dat_NO3 <- meta_dat %>% filter(response_var == "NO3")

# 3. Fit three-level random-effects models ----------------------------------
#    sigma^2.1 = between-study
#    sigma^2.2 = between-comparison within study
#    sigma^2.3 = between-pairing within comparison
fit_ml <- function(d, yi_col, vi_col) {
  rma.mv(
    yi     = d[[yi_col]],
    V      = d[[vi_col]],
    random = ~ 1 | Study_ID / Comparison_ID / pair_key,
    data   = d,
    method = "REML",
    test   = "t",       # Knapp–Hartung adjustment
    sparse = TRUE
  )
}

# --- DOC ---
m_DOC_raw  <- fit_ml(dat_DOC, "lnRR_mean",      "lnRR_var")
m_DOC_area <- fit_ml(dat_DOC, "lnRR_area_mean", "lnRR_area_var")

# --- NO3 ---
m_NO3_raw  <- fit_ml(dat_NO3, "lnRR_mean",      "lnRR_var")
m_NO3_area <- fit_ml(dat_NO3, "lnRR_area_mean", "lnRR_area_var")

summary(m_DOC_raw)
summary(m_DOC_area)
summary(m_NO3_raw)
summary(m_NO3_area)

# 4. Distribution of variance: multilevel I^2 -------------------------------
#    Cheung (2014); Harrer et al. §10.3

i2_multilevel <- function(mod) {
  W  <- diag(1 / mod$vi)
  X  <- model.matrix(mod)
  P  <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  typical_v <- (mod$k - mod$p) / sum(diag(P))
  total     <- sum(mod$sigma2) + typical_v
  tibble(
    I2_study      = 100 * mod$sigma2[1] / total,
    I2_comparison = 100 * mod$sigma2[2] / total,
    I2_pair       = 100 * mod$sigma2[3] / total,
    I2_total      = 100 * sum(mod$sigma2) / total
  )
}

i2_tbl <- bind_rows(
  i2_multilevel(m_DOC_raw)  %>% mutate(solute = "DOC", metric = "non-norm"),
  i2_multilevel(m_DOC_area) %>% mutate(solute = "DOC", metric = "area-norm"),
  i2_multilevel(m_NO3_raw)  %>% mutate(solute = "NO3", metric = "non-norm"),
  i2_multilevel(m_NO3_area) %>% mutate(solute = "NO3", metric = "area-norm")
) %>% relocate(solute, metric)

print(i2_tbl)

# 5. Is each level justified? LR tests vs. reduced models -------------------
test_level <- function(d, yi_col, vi_col, drop = c("study","comparison","pair")) {
  drop <- match.arg(drop)
  # sigma2 order: c(Study, Comparison, pair_key)
  s2 <- c(NA, NA, NA)
  s2[switch(drop, study = 1, comparison = 2, pair = 3)] <- 0
  reduced <- rma.mv(
    yi = d[[yi_col]], V = d[[vi_col]],
    random = ~ 1 | Study_ID / Comparison_ID / pair_key,
    sigma2 = s2,
    data = d, method = "REML", test = "t", sparse = TRUE
  )
  full <- fit_ml(d, yi_col, vi_col)
  anova(full, reduced)
}

# Example: DOC, raw
test_level(dat_DOC, "lnRR_mean", "lnRR_var", "study")
test_level(dat_DOC, "lnRR_mean", "lnRR_var", "comparison")
test_level(dat_DOC, "lnRR_mean", "lnRR_var", "pair")

# 6. Moderator (meta-regression) analyses -----------------------------------
#    Scale continuous moderators so the intercept is interpretable and
#    variance components are stable.
num_mods <- moderators[vapply(master_ES[moderators], is.numeric, logical(1))]
cat_mods <- setdiff(moderators, num_mods)

scale_mods <- function(d) {
  d %>% mutate(across(all_of(num_mods), ~ as.numeric(scale(.))))
}
dat_DOC_s <- scale_mods(dat_DOC)
dat_NO3_s <- scale_mods(dat_NO3)

fit_mod <- function(d, yi_col, vi_col, rhs) {
  rma.mv(
    yi = d[[yi_col]], V = d[[vi_col]],
    mods   = as.formula(paste("~", rhs)),
    random = ~ 1 | Study_ID / Comparison_ID / pair_key,
    data   = d, method = "REML", test = "t", sparse = TRUE
  )
}

# --- (a) Single-moderator sweep (one at a time) ---------------------------
single_mod_sweep <- function(d, yi_col, vi_col, solute, metric) {
  map_dfr(moderators, function(v) {
    out <- tryCatch(
      fit_mod(d, yi_col, vi_col, v),
      error = function(e) NULL
    )
    if (is.null(out)) {
      return(tibble(solute = solute, metric = metric, moderator = v,
                    note = "model failed"))
    }
    # For categorical moderators, beta has multiple rows; summarise omnibus QM
    tibble(
      solute    = solute,
      metric    = metric,
      moderator = v,
      k         = out$k,
      QM        = out$QM,
      QMdf1     = out$QMdf[1],
      QM_p      = out$QMp,
      R2        = ifelse(is.null(out$R2), NA_real_, out$R2),
      # first non-intercept coefficient (useful for numeric mods)
      beta1     = if (length(out$beta) >= 2) out$beta[2] else NA_real_,
      se1       = if (length(out$se)   >= 2) out$se[2]   else NA_real_,
      p1        = if (length(out$pval) >= 2) out$pval[2] else NA_real_
    )
  })
}

single_mod_tbl <- bind_rows(
  single_mod_sweep(dat_DOC_s, "lnRR_mean",      "lnRR_var",      "DOC", "non-norm"),
  single_mod_sweep(dat_DOC_s, "lnRR_area_mean", "lnRR_area_var", "DOC", "area-norm"),
  single_mod_sweep(dat_NO3_s, "lnRR_mean",      "lnRR_var",      "NO3", "non-norm"),
  single_mod_sweep(dat_NO3_s, "lnRR_area_mean", "lnRR_area_var", "NO3", "area-norm")
) %>%
  arrange(solute, metric, QM_p)

print(single_mod_tbl, n = Inf)

# --- (b) Multiple-moderator models ---------------------------------------
#    Drop any moderator with too many NAs to keep sample size reasonable.
drop_high_na <- function(d, vars, thresh = 0.5) {
  keep <- vars[vapply(vars,
                      function(v) mean(is.na(d[[v]])) < thresh,
                      logical(1))]
  keep
}

mods_keep_DOC <- drop_high_na(dat_DOC_s, moderators)
mods_keep_NO3 <- drop_high_na(dat_NO3_s, moderators)

rhs_DOC <- paste(mods_keep_DOC, collapse = " + ")
rhs_NO3 <- paste(mods_keep_NO3, collapse = " + ")

# Only rows with complete cases across the kept moderators
dat_DOC_cc <- dat_DOC_s %>% drop_na(all_of(mods_keep_DOC))
dat_NO3_cc <- dat_NO3_s %>% drop_na(all_of(mods_keep_NO3))

m_DOC_full_raw  <- fit_mod(dat_DOC_cc, "lnRR_mean",      "lnRR_var",      rhs_DOC)
m_DOC_full_area <- fit_mod(dat_DOC_cc, "lnRR_area_mean", "lnRR_area_var", rhs_DOC)
m_NO3_full_raw  <- fit_mod(dat_NO3_cc, "lnRR_mean",      "lnRR_var",      rhs_NO3)
m_NO3_full_area <- fit_mod(dat_NO3_cc, "lnRR_area_mean", "lnRR_area_var", rhs_NO3)

summary(m_DOC_full_raw)
summary(m_NO3_full_raw)

# 7. Robust variance estimation (cluster-robust SEs at study level) ---------
#    Harrer et al. §10.7; Pustejovsky & Tipton (2022).
m_DOC_full_raw_rve  <- robust(m_DOC_full_raw,  cluster = dat_DOC_cc$Study_ID,
                              clubSandwich = TRUE)
m_DOC_full_area_rve <- robust(m_DOC_full_area, cluster = dat_DOC_cc$Study_ID,
                              clubSandwich = TRUE)
m_NO3_full_raw_rve  <- robust(m_NO3_full_raw,  cluster = dat_NO3_cc$Study_ID,
                              clubSandwich = TRUE)
m_NO3_full_area_rve <- robust(m_NO3_full_area, cluster = dat_NO3_cc$Study_ID,
                              clubSandwich = TRUE)

m_DOC_full_raw_rve
m_NO3_full_raw_rve

# 8. Model diagnostics ------------------------------------------------------
diagnose <- function(mod, tag) {
  message("---- Diagnostics: ", tag, " ----")
  # Profile likelihood for each variance component
  try(profile(mod, sigma2 = 1, main = paste(tag, "- sigma^2 (study)")),
      silent = TRUE)
  try(profile(mod, sigma2 = 2, main = paste(tag, "- sigma^2 (comparison)")),
      silent = TRUE)
  try(profile(mod, sigma2 = 3, main = paste(tag, "- sigma^2 (pair)")),
      silent = TRUE)
  
  # Standardized residuals
  rs <- rstandard(mod)$z
  par(mfrow = c(1, 2))
  plot(rs, main = paste(tag, "std. residuals"),
       ylab = "z", xlab = "index")
  abline(h = c(-2, 2), lty = 2)
  qqnorm(rs, main = paste(tag, "Q-Q")); qqline(rs)
  par(mfrow = c(1, 1))
}

diagnose(m_DOC_raw,  "DOC non-norm (null)")
diagnose(m_DOC_area, "DOC area-norm (null)")
diagnose(m_NO3_raw,  "NO3 non-norm (null)")
diagnose(m_NO3_area, "NO3 area-norm (null)")

# Cook's distance at the pair level (influence on fixed effects)
cd_DOC_raw <- tryCatch(cooks.distance(m_DOC_raw,  cluster = dat_DOC$pair_key),
                       error = function(e) NULL)
if (!is.null(cd_DOC_raw)) {
  plot(cd_DOC_raw, type = "h",
       main = "Cook's distance - DOC non-norm (pair level)",
       ylab = "Cook's D")
}

# 9. Publication bias / small-study effects --------------------------------
#    Multilevel-adjusted Egger-type test: regress yi on sqrt(vi) in the
#    multilevel model.
egger_ml <- function(d, yi_col, vi_col) {
  rma.mv(
    yi = d[[yi_col]], V = d[[vi_col]],
    mods   = ~ I(sqrt(d[[vi_col]])),
    random = ~ 1 | Study_ID / Comparison_ID / pair_key,
    data   = d, method = "REML", test = "t", sparse = TRUE
  )
}

egger_DOC_raw  <- egger_ml(dat_DOC, "lnRR_mean",      "lnRR_var")
egger_DOC_area <- egger_ml(dat_DOC, "lnRR_area_mean", "lnRR_area_var")
egger_NO3_raw  <- egger_ml(dat_NO3, "lnRR_mean",      "lnRR_var")
egger_NO3_area <- egger_ml(dat_NO3, "lnRR_area_mean", "lnRR_area_var")

summary(egger_DOC_raw)
summary(egger_NO3_raw)

# Funnel plots (from null models)
par(mfrow = c(2, 2))
funnel(m_DOC_raw,  main = "DOC - non-normalized")
funnel(m_DOC_area, main = "DOC - area-normalized")
funnel(m_NO3_raw,  main = "NO3 - non-normalized")
funnel(m_NO3_area, main = "NO3 - area-normalized")
par(mfrow = c(1, 1))

# 10. Forest plots (aggregated to study level for readability) -------------
make_forest <- function(mod, d, yi_col, vi_col, label) {
  agg <- aggregate(
    d %>% mutate(yi = .data[[yi_col]], vi = .data[[vi_col]]),
    cluster = Study_ID,
    V = vcov(mod, type = "obs"),
    addk = TRUE
  )
  forest(
    agg$yi, agg$vi,
    slab     = agg$Study_ID,
    header   = "Study",
    xlab     = paste("lnRR -", label),
    ilab     = agg$ki, ilab.xpos = min(agg$yi, na.rm = TRUE) - 0.5,
    ilab.lab = "k"
  )
  addpoly(mod, row = -1, mlab = "Pooled (3-level RE)")
}

# Write forest plots to PNG
fig_dir <- "Output_for_analysis/05_three_level_meta-analysis_models/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

png(file.path(fig_dir, "forest_DOC_nonnorm.png"),
    width = 1600, height = 1200, res = 150)
make_forest(m_DOC_raw,  dat_DOC, "lnRR_mean",      "lnRR_var",
            "DOC non-normalized")
dev.off()

png(file.path(fig_dir, "forest_DOC_areanorm.png"),
    width = 1600, height = 1200, res = 150)
make_forest(m_DOC_area, dat_DOC, "lnRR_area_mean", "lnRR_area_var",
            "DOC area-normalized")
dev.off()

png(file.path(fig_dir, "forest_NO3_nonnorm.png"),
    width = 1600, height = 1200, res = 150)
make_forest(m_NO3_raw,  dat_NO3, "lnRR_mean",      "lnRR_var",
            "NO3 non-normalized")
dev.off()

png(file.path(fig

              