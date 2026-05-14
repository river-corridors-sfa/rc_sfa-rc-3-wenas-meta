# =================================== Objectives =================================
# 
# Script: 03b_exploratory_null_hypotheses.R
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
out_dir <- here("Output_for_analysis", "03b_exploratory_null_hypothesis")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)


# SECTION A: Summary statistics (calculated on raw, reported in both scales)

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

# burn_unburn <- summary_stats %>% 
#   select(Burn_Unburn, NO3_median) %>% 
#   filter(NO3_median > 0) %>% 
#   group_by(Burn_Unburn) %>% 
#   summarize(n = n())

# write_csv(summary_stats, file.path(out_dir, "04_summary_statistics.csv"))

# SECTION B: Continuous predictors (scatter plots + Spearman correlations)
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

# SECTION C: Categorical predictors (boxplots with log-y)

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

# Down selection process ####

# COVARIATE REDUCTION PIPELINE FOR DOC AND NO3 SPARSE MODELING
# Workflow:
#   Step 1: Drop obvious non-predictive variables
#   Step 2: Compute aggregated variables
#   Step 3: Remove near-zero variance predictors
#   Step 4: Correlation analysis (drop |r| > 0.85)
#   Step 5: PCA to understand multivariate structure
#   Step 6: Hypothesis-based final selection
#   Step 7: Prepare reduced datasets for LASSO/SuSiE
# --- Load required packages ---
p_load(tidyverse,
       corrplot,
       ggplot2,
       patchwork,
       factoextra, 
       FactoMineR, 
       caret)

# Set seed for reproducibility
set.seed(42)

# Create output directory for plots
out_dir <- here("Output_for_analysis", "03b_exploratory_null_hypothesis", "figures")

# STEP 0: LOAD YOUR DATA ####
# Assumes a data frame `full_data` with:
#   - Site-level rows (one row per site, 71 sites)
#   - Identifier columns: Study_ID, Site, Burn_Unburn
#   - Response columns: DOC_median, DOC_mean, DOC_sd,
#                       NO3_median, NO3_mean, NO3_sd, n_obs
#   - ~200 geospatial covariates

full_data <- merged %>% 
  group_by(Site) %>% 
  summarise(n_obs      = sum(!is.na(DOC_Interp_mg_C_L) | !is.na(NO3_Interp_mg_N_L)),
            DOC_median = median(DOC_Interp_mg_C_L, na.rm = TRUE),
            DOC_mean   = mean(DOC_Interp_mg_C_L, na.rm = TRUE),
            DOC_sd     = sd(DOC_Interp_mg_C_L, na.rm = TRUE),
            NO3_median = median(NO3_Interp_mg_N_L, na.rm = TRUE),
            NO3_mean   = mean(NO3_Interp_mg_N_L, na.rm = TRUE),
            NO3_sd     = sd(NO3_Interp_mg_N_L, na.rm = TRUE),
            across(everything(), first))

# STEP 1: DROP OBVIOUS NON-PREDICTIVE VARIABLES ####
# Administrative / HUC codes (spatial IDs, not mechanisms)
admin_vars <- c("rpuid", "vpuid", "enabled",
                "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
                "Study_ID", "Comparison_ID", "Pair", "Sampling_Date",
                "latitude", "longitude", "comid", "DOC_Interp_mg_C_L", "NO3_Interp_mg_N_L", 
                "Days_Between_Samples", "objectid", "fdate", "resolution", "gnis_id",
                "gnis_name", "reachcode", "flowdir", "wbareacomi", "ftype", "fcode", "shape_length",
                "streamleve", "streamcalc", "fromnode", "tonode", "hydroseq", "levelpathi",
                "pathlength", "terminalpa", "arbolatesu")

# Topology variables (describe flow network, not watershed properties) [2]
topology_vars <- c("divergence", "startflag", "terminalfl", "dnlevel",
                   "uplevelpat", "uphydroseq", "dnlevelpat", "dnminorhyd",
                   "dndraincou", "dnhydroseq", "frommeas", "tomeas",
                   "rtndiv", "vpuin", "vpuout", "hwtype", "elevfixed",
                   "slopelenkm", "tidal", "wbareatype", "pathtimema")

# Raw elevation artifacts (smoothed slope is better) [2]
elev_raw_vars <- c("maxelevraw", "minelevraw", "maxelevsmo", "minelevsmo")

# Fire metadata / artifacts (dictionary flags burn_sev_NA as droppable) [2]
fire_meta_vars <- c("fire_years_used", "burn_sev_NA")

# Monthly flow variables (72 total: 6 prefixes × 12 months) [2]
months <- sprintf("%02d", 1:12)
flow_prefixes <- c("qa", "qc", "qe", "va", "vc", "ve")
monthly_flow_vars <- as.vector(outer(flow_prefixes, months, paste, sep = "_"))

# Combine all drop lists
vars_to_drop <- c(admin_vars, topology_vars, elev_raw_vars,
                  fire_meta_vars, monthly_flow_vars)

cat("=== STEP 1: Dropping obvious non-predictive variables ===\n")
cat("Variables to drop:", length(vars_to_drop), "\n")

data_step1 <- full_data %>%
  select(-any_of(vars_to_drop))

cat("Variables remaining after Step 1:", ncol(data_step1), "\n\n")

# STEP 2: COMPUTE AGGREGATED VARIABLES ####

cat("=== STEP 2: Computing aggregated variables ===\n")

data_step2 <- data_step1 %>%
  mutate(
    # Aggregate land cover into conceptual groups [2]
    pct_forest  = pctconif2019ws + pctdecid2019ws + pctmxfst2019ws,
    pct_urban   = pcturbhi2019ws + pcturblo2019ws + pcturbmd2019ws + pcturbop2019ws,
    pct_ag      = pctcrop2019ws + pcthay2019ws,
    pct_wetland = pcthbwet2019ws + pctwdwet2019ws,
    
    # Weighted burn severity index (high weighted more than low) [2]
    burn_severity_index = 3 * burn_sev_high + 2 * burn_sev_mod + 1 * burn_sev_low,
    
    # Binary burn status as numeric (for LASSO/SuSiE)
    burn_binary = ifelse(Burn_Unburn == "Burn", 1, 0)
  ) %>%
  # Drop original NLCD variables now that we have aggregates
  select(-any_of(c("pctconif2019ws", "pctdecid2019ws", "pctmxfst2019ws",
                   "pcturbhi2019ws", "pcturblo2019ws", "pcturbmd2019ws",
                   "pcturbop2019ws", "pctcrop2019ws", "pcthay2019ws",
                   "pcthbwet2019ws", "pctwdwet2019ws"))) %>%
  # Drop individual burn severity components (keep the index)
  # Optional - comment out if you want to keep them separate
  select(-any_of(c("burn_sev_high", "burn_sev_mod", "burn_sev_low"))) %>%
  # Keep only qa_ma (dictionary flags it as KEY VARIABLE) [2], drop other annual flows
  select(-any_of(c("qc_ma", "qe_ma", "va_ma", "vc_ma", "ve_ma")))

cat("Variables remaining after Step 2:", ncol(data_step2), "\n\n")

# STEP 3: REMOVE NEAR-ZERO VARIANCE PREDICTORS ####

cat("=== STEP 3: Removing near-zero variance predictors ===\n")

# Define identifiers and response variables to exclude from reduction
response_vars <- c("DOC_median", "DOC_mean", "DOC_sd",
                   "NO3_median", "NO3_mean", "NO3_sd", "n_obs")
id_vars <- c("Site", "Burn_Unburn")

# Extract covariate columns only
covariate_cols <- setdiff(names(data_step2), c(response_vars, id_vars))
numeric_covs <- data_step2 %>%
  select(all_of(covariate_cols)) %>%
  select(where(is.numeric))

# Identify near-zero variance variables
nzv <- nearZeroVar(numeric_covs, saveMetrics = TRUE, freqCut = 95/5)
nzv_to_drop <- rownames(nzv)[nzv$nzv == TRUE]

cat("Near-zero variance variables to drop:", length(nzv_to_drop), "\n")
if (length(nzv_to_drop) > 0) print(nzv_to_drop)

data_step3 <- data_step2 %>%
  select(-any_of(nzv_to_drop))

cat("Variables remaining after Step 3:", ncol(data_step3), "\n\n")

# STEP 4: CORRELATION ANALYSIS (DROP |r| > 0.85) ####

cat("=== STEP 4: Correlation analysis ===\n")

# Update covariate list
covariate_cols <- setdiff(names(data_step3), c(response_vars, id_vars))
cov_matrix <- data_step3 %>%
  select(all_of(covariate_cols)) %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_mat <- cor(cov_matrix, use = "pairwise.complete.obs")

# DIAGNOSTIC: Identify problematic columns before correlation ####

cat("=== Diagnosing problematic columns ===\n\n")

# Check for columns that are entirely NA
all_na_cols <- names(cov_matrix)[sapply(cov_matrix, function(x) all(is.na(x)))]
cat("Columns that are entirely NA:\n")
print(all_na_cols)

# Check for columns with zero variance (all same value, ignoring NA)
zero_var_cols <- names(cov_matrix)[sapply(cov_matrix, function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 2) return(TRUE)  # Not enough data
  return(sd(x_clean) == 0)
})]
cat("\nColumns with zero variance (or insufficient data):\n")
print(zero_var_cols)

# Check proportion of NAs in each column
na_prop <- sapply(cov_matrix, function(x) mean(is.na(x)))
high_na_cols <- names(na_prop)[na_prop > 0.5]
cat("\nColumns with >50% missing values:\n")
print(data.frame(variable = high_na_cols, prop_NA = round(na_prop[high_na_cols], 2)))

# REMOVE PROBLEMATIC COLUMNS ####

# Combine problem columns into one drop list
problem_cols <- unique(c(all_na_cols, zero_var_cols, high_na_cols))

cat("\nTotal columns being dropped due to data quality issues:",
    length(problem_cols), "\n")
print(problem_cols)

# Remove them from cov_matrix
cov_matrix <- cov_matrix %>% select(-any_of(problem_cols))

cat("\nColumns remaining after cleanup:", ncol(cov_matrix), "\n\n")

# ALSO update data_step3 so these don't carry forward
data_step3 <- data_step3 %>% select(-any_of(problem_cols))

# NOW COMPUTE CORRELATION (should work without warnings) ####

cor_mat <- cor(cov_matrix, use = "pairwise.complete.obs")

# Sanity check: are there still any NAs in the correlation matrix?
n_na_in_cor <- sum(is.na(cor_mat))
cat("Number of NA values in correlation matrix:", n_na_in_cor, "\n")

if (n_na_in_cor > 0) {
  # Find which variables are causing remaining NAs
  na_rows <- rowSums(is.na(cor_mat))
  problem_vars <- names(na_rows)[na_rows > 0]
  cat("Variables still causing NAs in correlation matrix:\n")
  print(problem_vars)
  
  # Drop them
  cov_matrix <- cov_matrix %>% select(-any_of(problem_vars))
  data_step3 <- data_step3 %>% select(-any_of(problem_vars))
  cor_mat <- cor(cov_matrix, use = "pairwise.complete.obs")
  cat("Recomputed correlation matrix after dropping. Dimensions:",
      dim(cor_mat), "\n")
}

# --- PLOT 1: Full correlation heatmap ---
png("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/01_correlation_heatmap.png",
    width = 1400, height = 1400, res = 120)
corrplot(cor_mat,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.6,
         tl.col = "black",
         addCoef.col = NULL,
         title = "Correlation Matrix of Remaining Covariates",
         mar = c(0, 0, 2, 0))
dev.off()

# --- Identify highly correlated pairs ---
high_cor_pairs <- which(abs(cor_mat) > 0.85 & upper.tri(cor_mat), arr.ind = TRUE)

if (nrow(high_cor_pairs) > 0) {
  high_cor_df <- data.frame(
    var1 = rownames(cor_mat)[high_cor_pairs[, 1]],
    var2 = colnames(cor_mat)[high_cor_pairs[, 2]],
    correlation = cor_mat[high_cor_pairs]
  ) %>%
    arrange(desc(abs(correlation)))
  
  cat("\nHighly correlated pairs (|r| > 0.85):\n")
  print(high_cor_df)
  write_csv(high_cor_df, "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/highly_correlated_pairs.csv")
}

# --- Use findCorrelation to suggest drops ---
high_cor_drop <- findCorrelation(cor_mat, cutoff = 0.85, names = TRUE)
cat("\nSuggested variables to drop based on correlation (|r| > 0.85):\n")
print(high_cor_drop)

# --- PLOT 2: Reduced correlation heatmap (after dropping correlated) ---
data_step4 <- data_step3 %>%
  select(-any_of(high_cor_drop))

covariate_cols_reduced <- setdiff(names(data_step4), c(response_vars, id_vars))
cov_matrix_reduced <- data_step4 %>%
  select(all_of(covariate_cols_reduced)) %>%
  select(where(is.numeric))

cor_mat_reduced <- cor(cov_matrix_reduced, use = "pairwise.complete.obs")

png("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/02_correlation_heatmap_reduced.png",
    width = 1000, height = 1000, res = 120)
corrplot(cor_mat_reduced,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.6,
         title = "Correlation Matrix After Dropping Highly Correlated Variables",
         mar = c(0, 0, 2, 0))
dev.off()

cat("Variables remaining after Step 4:", ncol(data_step4), "\n\n")

# STEP 5: PCA TO UNDERSTAND MULTIVARIATE STRUCTURE (continued) ####

cat("=== STEP 5: PCA analysis ===\n")

# Prepare data for PCA (complete cases only)
pca_input <- cov_matrix_reduced %>% drop_na()
pca_site_info <- data_step4 %>%
  drop_na(all_of(names(cov_matrix_reduced))) %>%
  select(all_of(id_vars))

cat("Sites with complete data for PCA:", nrow(pca_input), "\n")
cat("Variables entering PCA:", ncol(pca_input), "\n\n")

# Run PCA with scaling (standardize variables)
pca_result <- PCA(pca_input, scale.unit = TRUE, graph = FALSE, ncp = 10)

# Extract variance explained
eig_values <- get_eigenvalue(pca_result)
cat("Variance explained by first 10 PCs:\n")
print(round(eig_values, 2))

# --- PLOT 3: Scree plot (variance explained by each PC) ---
p_scree <- fviz_eig(pca_result,
                    addlabels = TRUE,
                    ylim = c(0, 50),
                    ncp = 10,
                    main = "Scree Plot: Variance Explained by Principal Components",
                    barfill = "#3498DB",
                    barcolor = "#2C3E50") +
  theme_minimal() +
  labs(x = "Principal Component", y = "% of Variance Explained")

ggsave("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/03_scree_plot.png",
       p_scree, width = 8, height = 6, dpi = 300)

# Determine how many PCs to retain (Kaiser criterion: eigenvalue > 1)
n_pc_retain <- sum(eig_values[, 1] > 1)
cat("\nNumber of PCs with eigenvalue > 1 (Kaiser criterion):", n_pc_retain, "\n")

# Alternatively: number of PCs needed to explain 80% of variance
n_pc_80 <- which(eig_values[, 3] >= 80)[1]
cat("Number of PCs needed to explain 80% of variance:", n_pc_80, "\n\n")

# --- PLOT 4: PCA biplot colored by burn status ---
p_biplot <- fviz_pca_biplot(pca_result,
                            col.ind = pca_site_info$Burn_Unburn,
                            palette = c("#E74C3C", "#3498DB"),
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            legend.title = "Burn Status",
                            label = "var",
                            col.var = "black",
                            alpha.var = 0.6,
                            repel = TRUE,
                            title = "PCA Biplot: Sites and Covariates Colored by Burn Status") +
  theme_minimal() +
  theme(legend.position = "right")

ggsave("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/04_pca_biplot.png",
       p_biplot, width = 12, height = 10, dpi = 300)

# --- PLOT 5: Variable contributions to top PCs ---
p_contrib1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 20,
                           fill = "#3498DB", color = "#2C3E50",
                           title = "Top 20 Variable Contributions to PC1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

p_contrib2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 20,
                           fill = "#E74C3C", color = "#2C3E50",
                           title = "Top 20 Variable Contributions to PC2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

p_contrib3 <- fviz_contrib(pca_result, choice = "var", axes = 3, top = 20,
                           fill = "#2ECC71", color = "#2C3E50",
                           title = "Top 20 Variable Contributions to PC3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

p_contrib_combined <- p_contrib1 / p_contrib2 / p_contrib3
ggsave("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/05_pca_contributions.png",
       p_contrib_combined, width = 12, height = 14, dpi = 300)

# --- PLOT 6: Variable map (correlation circle) ---
p_var_map <- fviz_pca_var(pca_result,
                          col.var = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE,
                          title = "Variables in PC1-PC2 Space (colored by contribution)") +
  theme_minimal()

ggsave("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/06_variable_map.png",
       p_var_map, width = 10, height = 10, dpi = 300)

# --- PLOT 7: Individual sites in PC space (colored by burn status) ---
p_ind <- fviz_pca_ind(pca_result,
                      col.ind = pca_site_info$Burn_Unburn,
                      palette = c("#E74C3C", "#3498DB"),
                      addEllipses = TRUE,
                      ellipse.type = "confidence",
                      legend.title = "Burn Status",
                      repel = TRUE,
                      title = "Sites in PC1-PC2 Space") +
  theme_minimal()

ggsave("~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/07_pca_individuals.png",
       p_ind, width = 10, height = 8, dpi = 300)

# --- Export top variables per PC as a table ---
var_contrib <- get_pca_var(pca_result)$contrib
top_contrib_df <- as.data.frame(var_contrib) %>%
  rownames_to_column("variable") %>%
  pivot_longer(-variable, names_to = "PC", values_to = "contribution") %>%
  group_by(PC) %>%
  slice_max(contribution, n = 10) %>%
  ungroup() %>%
  arrange(PC, desc(contribution))

write_csv(top_contrib_df, "~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03b_exploratory_null_hypothesis/figures/top_pca_contributors.csv")
cat("Top PCA contributors saved to top_pca_contributors.csv\n\n")

# STEP 6: HYPOTHESIS-BASED FINAL SELECTION ####

cat("=== STEP 6: Hypothesis-based final variable selection ===\n")

# Based on biogeochemical reasoning, define your final variable sets
# These should be informed by the correlation and PCA results above

# Final DOC covariates (n = 43; aim for ~15)
doc_final_vars <- c(
  # Fire (primary hypothesis)
  "burn_binary", "burn_percent_fire_year", "burn_severity_index",
  # Land cover
  "pct_forest", "pct_urban", "pct_ag", "pct_wetland",
  # Climate
  "precip8110ws", "tmean8110ws",
  # Soils (DOC-relevant: organic matter, permeability, depth)
  "omws", "permws", "rckdepws",
  # Hydrology
  "bfiws", "runoffws", "totdasqkm", "slope"
)

# Final NO3 covariates (n = 61; aim for ~18)
no3_final_vars <- c(
  # Fire
  "burn_binary", "burn_percent_fire_year", "burn_severity_index",
  # Land cover
  "pct_forest", "pct_urban", "pct_ag", "pct_wetland",
  # Climate
  "precip8110ws", "tmean8110ws",
  # Soils (NO3-relevant: clay/sand, organic matter, conductivity)
  "clayws", "omws", "permws", "hydrlcondws",
  # Hydrology
  "bfiws", "runoffws", "totdasqkm", "slope", "qa_ma"
)

# Filter to variables that actually exist in data_step4
doc_final_vars <- intersect(doc_final_vars, names(data_step4))
no3_final_vars <- intersect(no3_final_vars, names(data_step4))

cat("Final DOC covariates (", length(doc_final_vars), "):\n")
print(doc_final_vars)
cat("\nFinal NO3 covariates (", length(no3_final_vars), "):\n")
print(no3_final_vars)

# STEP 7: PREPARE REDUCED DATASETS FOR LASSO/SuSiE ####

cat("\n=== STEP 7: Preparing final datasets for sparse modeling ===\n")

# --- DOC dataset ---
doc_dataset <- data_step4 %>%
  filter(!is.na(DOC_median)) %>%
  select(all_of(c(id_vars, "DOC_median", "DOC_mean", "n_obs", doc_final_vars))) %>%
  # Remove any rows with NA in covariates
  drop_na(all_of(doc_final_vars))

cat("DOC dataset dimensions:", nrow(doc_dataset), "sites x",
    length(doc_final_vars), "covariates\n")
cat("  Burned:", sum(doc_dataset$Burn_Unburn == "Burn"), "\n")
cat("  Unburned:", sum(doc_dataset$Burn_Unburn == "Unburn"), "\n")

# --- NO3 dataset ---
no3_dataset <- data_step4 %>%
  filter(!is.na(NO3_median)) %>%
  select(all_of(c(id_vars, "NO3_median", "NO3_mean", "n_obs", no3_final_vars))) %>%
  drop_na(all_of(no3_final_vars))

cat("NO3 dataset dimensions:", nrow(no3_dataset), "sites x",
    length(no3_final_vars), "covariates\n")
cat("  Burned:", sum(no3_dataset$Burn_Unburn == "Burn"), "\n")
cat("  Unburned:", sum(no3_dataset$Burn_Unburn == "Unburn"), "\n\n")

# --- Create X matrices and y vectors for LASSO/SuSiE ---
# These match the format expected by glmnet() and susie() [1]

# DOC
X_doc <- doc_dataset %>%
  select(all_of(doc_final_vars)) %>%
  as.matrix()
y_doc <- doc_dataset$DOC_median  # using median as response (robust)

# Standardize X (important for LASSO/SuSiE to treat variables comparably)
X_doc_scaled <- scale(X_doc)

# NO3
X_no3 <- no3_dataset %>%
  select(all_of(no3_final_vars)) %>%
  as.matrix()
y_no3 <- no3_dataset$NO3_median
X_no3_scaled <- scale(X_no3)

# Save prepared datasets
saveRDS(list(X = X_doc_scaled, y = y_doc, raw = doc_dataset),
        "covariate_reduction_plots/doc_sparse_input.rds")
saveRDS(list(X = X_no3_scaled, y = y_no3, raw = no3_dataset),
        "covariate_reduction_plots/no3_sparse_input.rds")

cat("Prepared input datasets saved as .rds files.\n\n")











