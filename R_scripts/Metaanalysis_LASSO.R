#
# LASSO for meta-analysis
# 
# Status: In progress
#
# Review status: Not reviewed
#
#
# Notes: I removed climate for first go, probably should add back in 
# ==============================================================================
#
# Author: Brieanne Forbes
# 3 June 2026
#
# ==============================================================================
library(tidyverse) 
library(corrplot)
library(glmnet)
library(ggpmisc)

rm(list=ls(all=T))

# Setting wd to parent folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")
# ========================== read and summarize input ==========================

input <- read_csv("./Output_for_analysis/03_merge_geospatial/03_master_merged_Brie_LASSO.csv") %>%
  select(-c(Comparison_ID, Pair, Sampling_Date, latitude, longitude, comid, Time_Since_Fire)) %>% #remove columns that wont go into the LASSO
  select(-c(maxelevsmo, slope)) %>% # remove because they have too many NA values
  select(-c(Climate)) %>% # remove because its categorical
  group_by(Study_ID, Site, Burn_Unburn) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%  # Updated this line
  rename_with(.fn = ~ paste0("mean_", .x), .cols = where(is.numeric)) %>%
  ungroup()

# remove nitrate as a variable for doc lasso and burn categories for unburned lasso
c_burn <- input %>%
  select(-c(mean_NO3_Interp_mg_N_L)) %>%
  filter(!is.na(mean_DOC_Interp_mg_C_L)) %>%
  filter(Burn_Unburn == 'Burn') %>%
  select(-Burn_Unburn)

c_unburn <- input %>%
  select(-c(mean_NO3_Interp_mg_N_L, mean_burn_percent_fire_year, mean_burn_sev_high, mean_burn_sev_mod, mean_burn_sev_low))%>%
  filter(!is.na(mean_DOC_Interp_mg_C_L))%>%
  filter(Burn_Unburn == 'Unurn')%>%
  select(-Burn_Unburn)

# remove doc as a variable for nitrate lasso and burn categories for unburned lasso
n_burn <- input %>%
  select(-c(mean_DOC_Interp_mg_C_L))%>%
  filter(!is.na(mean_NO3_Interp_mg_N_L))%>%
  filter(Burn_Unburn == 'Burn')%>%
  select(-Burn_Unburn)

n_unburn <- input %>%
  select(-c(mean_DOC_Interp_mg_C_L, mean_burn_percent_fire_year, mean_burn_sev_high, mean_burn_sev_mod, mean_burn_sev_low))%>%
  filter(!is.na(mean_NO3_Interp_mg_N_L))%>%
  filter(Burn_Unburn == 'Unurn')%>%
  select(-Burn_Unburn)

# =============================== check NAs ===============================

# Create NA summary for each dataframe
na_summary <- bind_rows(
  c_burn %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    mutate(dataset = "c_burn"),
  
  c_unburn %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    mutate(dataset = "c_unburn"),
  
  n_burn %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    mutate(dataset = "n_burn"),
  
  n_unburn %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    mutate(dataset = "n_unburn")
) %>%
  select(dataset, everything()) %>%
  pivot_longer(-dataset, names_to = "column", values_to = "na_count") %>%
  pivot_wider(names_from = dataset, values_from = na_count, values_fill = 0)

# View the table, 0 NA's, good to go
na_summary %>%
  clipr::write_clip()

rm(c_burn, c_unburn, n_burn, n_unburn) # remove because I will recreate later

# everything below this is copy and pasted in and needs to be updated before running

## ======== Variable Name Mapping ===========
# Define a tibble mapping original, scaled, and plot labels

variable_names <- tibble(
  original = c("mean_DOC_Interp_mg_C_L", "mean_NO3_Interp_mg_N_L", "mean_Area_watershed_km", "mean_tmax8110ws", 
               "mean_tmean8110ws", "mean_tmin8110ws", "mean_forest_cover", "mean_urban_cover", 
               "mean_grassland_cover", "mean_wetland_cover", "mean_ag_cover", "mean_omws", 
               "mean_rckdepws", "mean_pctcarbresidws", "mean_pctnoncarbresidws", "mean_pctsilicicws", 
               "mean_glacial_till", "mean_pctsallakews", "mean_bfiws", "mean_permws", 
               "mean_runoffws", "mean_burn_percent_fire_year", "mean_burn_sev_high", "mean_burn_sev_mod", 
               "mean_burn_sev_low"),
  labels = c("DOC Interpolated", "NO3 Interpolated", "Watershed Area", "Maximum Temperature", 
             "Mean Temperature", "Minimum Temperature", "Forest Cover", "Urban Cover", 
             "Grassland Cover", "Wetland Cover", "Agricultural Cover", "Soil Organic Matter", 
             "Depth to Bedrock", "Carbonate Residual Material", "Non-carbonate Residual Material", "Silicic Rock", 
             "Glacial Till", "Saline Lake Sediment", "Base Flow Index", "Soil Permeability", 
             "Annual Runoff", "Fire Burn Percentage", "High Severity Burn", "Moderate Severity Burn", 
             "Low Severity Burn")
) %>%
  mutate(cubed = paste0("cube_",original))

## ======== Cube Root Transformation ===========

cube_root <- function(x) sign(x) * (abs(x))^(1/3)

cube_data <- input %>%
  mutate(across(where(is.numeric), cube_root)) %>%
  rename_with(.fn = ~ paste0("cube_", .x), .cols = where(is.numeric))

## ======== Pearson Correlation Matrix with Cube Transformation ===========
renamed_cube_data <- cube_data %>%
  select(-Study_ID, -Site, -Burn_Unburn) %>%
  rename_with(~ ifelse(!is.na(match(., variable_names$cubed)),
                       variable_names$labels[match(., variable_names$cubed)],
                       .), .cols = names(cube_data %>% select(-Study_ID, -Site, -Burn_Unburn)))

pearson_cubed <- cor(renamed_cube_data, method = "pearson", use = "complete.obs")

rdylbu_colors <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090",
                   "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")

png(file = "./initial_plots/LASSO_Figs/Pearson_Correlation_Matrix_Cubed.png", width = 12, height = 12, units = "in", res = 300)
corrplot(pearson_cubed, type = "upper", method = "number",
         col = colorRampPalette(rdylbu_colors)(200), tl.col = "black", tl.cex = .5,
         number.cex = 0.9, cl.cex = 1.25, mar = c(0, 0, 2, 0), bg = "black")
dev.off()

# ======== LASSO  ============


tests <- c('c_burn', 'c_unburn', 'n_burn', 'n_unburn')

## Loop through LASSO to get average over a lot of seeds ####
for (test in tests) {
  
  if(test == 'c_burn'){
    scale_cube_variables <- cube_data %>%
      filter(!is.na(cube_mean_DOC_Interp_mg_C_L)) %>%
      filter(Burn_Unburn == 'Burn') %>%
      select(-Burn_Unburn, -Study_ID, -Site, -cube_mean_NO3_Interp_mg_N_L) %>%
      mutate(across(where(is.numeric), ~ as.numeric(scale(.x)))) %>%  # Scale only numeric
      rename_with(.cols = where(is.numeric), .fn = ~ paste0("scale_", .x)) %>%  # Fix syntax
      as.data.frame()
    response_variable <- 'scale_cube_mean_DOC_Interp_mg_C_L'
    
  } else if (test == 'c_unburn'){
    scale_cube_variables <- cube_data %>%
      filter(!is.na(cube_mean_DOC_Interp_mg_C_L)) %>%
      filter(Burn_Unburn == 'Unburn') %>%
      select(-Burn_Unburn, -Study_ID, -Site, -cube_mean_NO3_Interp_mg_N_L, -cube_mean_burn_percent_fire_year, -cube_mean_burn_sev_high, -cube_mean_burn_sev_mod, -cube_mean_burn_sev_low) %>%
      mutate(across(where(is.numeric), ~ as.numeric(scale(.x)))) %>%  # Scale only numeric
      rename_with(.cols = where(is.numeric), .fn = ~ paste0("scale_", .x)) %>%  # Fix syntax
      as.data.frame()
    response_variable <- 'scale_cube_mean_DOC_Interp_mg_C_L'
    
  } else if(test == 'n_burn'){
    scale_cube_variables <- cube_data %>%
      filter(!is.na(cube_mean_NO3_Interp_mg_N_L)) %>%
      filter(Burn_Unburn == 'Burn') %>%
      select(-Burn_Unburn, -Study_ID, -Site, -cube_mean_DOC_Interp_mg_C_L) %>%
      mutate(across(where(is.numeric), ~ as.numeric(scale(.x)))) %>%  # Scale only numeric
      rename_with(.cols = where(is.numeric), .fn = ~ paste0("scale_", .x)) %>%  # Fix syntax
      as.data.frame()
    response_variable <- 'scale_cube_mean_NO3_Interp_mg_N_L'
    
  } else if (test == 'n_unburn'){
    scale_cube_variables <- cube_data %>%
      filter(!is.na(cube_mean_NO3_Interp_mg_N_L)) %>%
      filter(Burn_Unburn == 'Unburn') %>%
      select(-Burn_Unburn, -Study_ID, -Site, -cube_mean_DOC_Interp_mg_C_L, -cube_mean_burn_percent_fire_year, -cube_mean_burn_sev_high, -cube_mean_burn_sev_mod, -cube_mean_burn_sev_low) %>%
      mutate(across(where(is.numeric), ~ as.numeric(scale(.x)))) %>%  # Scale only numeric
      rename_with(.cols = where(is.numeric), .fn = ~ paste0("scale_", .x)) %>%  # Fix syntax
      as.data.frame()
    response_variable <- 'scale_cube_mean_NO3_Interp_mg_N_L'
  }

  num_seeds = 100
  seeds = sample(1:500, num_seeds)

  ## Set response variable and scale
  yvar <- data.matrix(scale_cube_variables %>% pull(response_variable))
  round(mean(yvar), 4) # mean should be 0 and sd should 1
  sd(yvar)

  # list for storing LASSO iterations
  norm_coeffs = list()
  lasso_coefs_pull = list()
  r2_scores = numeric(num_seeds)

  ## Set predictor variables; exclude response variable(s)

  x_cube_variables = scale_cube_variables %>%
    select(-response_variable)


  xvars <- data.matrix(x_cube_variables)


  for (i in 1:num_seeds) {

    seed = seeds[i]
    set.seed(seed)
    
    lasso = cv.glmnet(xvars, yvar, alpha = 1, nfolds = 5,
                  standardize = FALSE, standardize.response = FALSE, intercept = FALSE)

    best_lambda <- lasso$lambda.min
    #best_lambda
    #plot(lasso)

    best_lasso_model <- glmnet(xvars, yvar, alpha = 1, lambda = best_lambda, family = "gaussian",
                               standardize = FALSE, standardize.response = FALSE, intercept = FALSE
                               #  , standardize = TRUE, standardize.response = TRUE, intercept = FALSE
                               #, standardize = TRUE, standardize.response = FALSE, intercept = FALSE
    )


    lasso_coefs = as.matrix(coef(best_lasso_model, s = best_lambda))

    lasso_coefs_pull[[as.character(seed)]] = lasso_coefs[-1, , drop = FALSE]

    norm_coeffs_scale = lasso_coefs/max(abs(lasso_coefs[-1]))

    norm_coeffs[[as.character(seed)]] = norm_coeffs_scale[-1, , drop = FALSE]

    y_pred = predict(best_lasso_model, newx = xvars, s = best_lambda)

    sst = sum((yvar - mean(yvar))^2)
    sse = sum((y_pred - yvar)^2)
    r2_scores[i] = 1 - (sse / sst)

  }

  lasso_coef_mat = as.data.frame(do.call(cbind, lasso_coefs_pull))
  colnames(lasso_coef_mat) <- paste0("s", seq_len(ncol(lasso_coef_mat)))
  # Make DF of all LASSO results with mean and std. dev
  lasso_coef_means = lasso_coef_mat %>%
    mutate(RowNames = rownames(lasso_coef_mat)) %>%
    rowwise() %>%
    mutate(mean = mean(c_across(contains("s1"))),
           sd = sd(c_across(contains("s1"))),
           cv = sd/mean) %>%
    relocate(mean, .before = s1) %>%
    relocate(sd, .before = s1) %>%
    relocate(RowNames, .before = mean)%>%
    relocate(cv, .after = sd) %>%
    add_column(test = test)

  norm_coeffs_matrix = do.call(cbind, norm_coeffs)

  mean_coeffs = as.data.frame(norm_coeffs_matrix, row.names = rownames(norm_coeffs_matrix))
  colnames(mean_coeffs) <- paste0("s", seq_len(ncol(mean_coeffs)))

  norm_lasso_coef_means = mean_coeffs %>%
    mutate(RowNames = rownames(mean_coeffs)) %>%
    rowwise() %>%
    mutate(mean = mean(c_across(contains("s1"))),
           sd = sd(c_across(contains("s1"))),
           cv = sd/mean) %>%
    relocate(mean, .before = s1) %>%
    relocate(sd, .before = s1) %>%
    relocate(RowNames, .before = mean)%>%
    relocate(cv, .after = sd) %>%
    add_column(test = test)

  results_r2 = as.data.frame(r2_scores)
  mean(results_r2$r2_scores)
  sd(results_r2$r2_scores)

  if(match(test, tests) == 1){

    lasso_coef_means_all <- lasso_coef_means
    norm_lasso_coef_means_all <- norm_lasso_coef_means
    mean_r2_all <- tibble(mean_r2 = mean(results_r2$r2_scores),
                          sd = sd(results_r2$r2_scores),
                          test = test)

  } else{

    lasso_coef_means_all <- lasso_coef_means_all %>%
      add_row(lasso_coef_means)

    norm_lasso_coef_means_all <- norm_lasso_coef_means_all %>%
      add_row(norm_lasso_coef_means)

    mean_r2_all <- mean_r2_all %>%
      add_row(mean_r2 = mean(results_r2$r2_scores),
              sd = sd(results_r2$r2_scores),
              test = test)
  }



}

# ================================ investigate cv ==============================

all_results_long <- bind_rows(
  lasso_coef_means_all %>%
    select(RowNames, mean, sd, cv, test) %>%
    add_column(type = 'Not_Normalized'),

  norm_lasso_coef_means_all %>%
    select(RowNames, mean, sd, cv, test) %>%
    add_column(type = 'Normalized')
) %>%
  mutate(cv = round(cv, 3))

# # absolute cv vs absolute mean; all norm/not norm + response variables
# ggplot(data = all_results_long, aes(x = abs(cv), y = abs(mean))) + 
#   geom_point()+
#   theme_bw()
# 
# # absolute cv vs absolute mean; all norm/not norm + response variables; filtered cv <= 1
# ggplot(data = all_results_long %>% filter(abs(cv) <= 1), aes(x = abs(cv), y = abs(mean))) + 
#   geom_point()+
#   theme_bw()
# 
# 
# # absolute cv vs absolute mean; pivoted by norm/not norm and response variable 
# cv_plot <- ggplot(data = all_results_long %>% mutate(response_variable = case_when(response_variable == 'scale_cube_Mean_degree_decay_rate' ~ 'Kdd',
#                                                                                    response_variable == 'scale_cube_Mean_Decay_Rate_per_day' ~ 'Kcd'),
#                                                      type = str_replace(type, 'Not_Normalized', 'Not Normalized' )), aes(x = abs(cv), y = abs(mean))) + 
#   geom_point()+
#   facet_grid(response_variable ~ type)+
#   theme_bw()
# 
# 
# # ggsave(
# #   paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()), "_Mean_vs_CV.png"),
# #   cv_plot,
# #   width = 8,
# #   height = 8,
# #   units = 'in',
# #   dpi = 300
# # )
# 
# # absolute cv vs absolute mean; pivoted by norm/not norm and response variable; filtered cv <= 1
# ggplot(data = all_results_long %>% filter(abs(cv) <= 1), aes(x = abs(cv), y = abs(mean))) + 
#   geom_point()+
#   facet_grid(response_variable ~ type)+
#   theme_bw()
# 
# #cv histo; all norm/not norm + response variables
# ggplot(data = all_results_long, aes(x = abs(cv))) + 
#   geom_histogram()+
#   theme_bw()
# 
# #cv histo; all norm/not norm + response variables; filtered cv <= 1
# ggplot(data = all_results_long %>% filter(abs(cv) <= 1), aes(x = abs(cv))) + 
#   geom_histogram()+
#   theme_bw()
# 
# #cv histo; pivoted by norm/not norm and response variable
# ggplot(data = all_results_long, aes(x = abs(cv))) + 
#   geom_histogram()+
#   facet_grid(response_variable ~ type)+
#   theme_bw()
# 
# #cv histo; pivoted by norm/not norm and response variable; filtered cv <= 1
# ggplot(data = all_results_long %>% filter(abs(cv) <= 1), aes(x = abs(cv))) + 
#   geom_histogram()+
#   facet_grid(response_variable ~ type)+
#   theme_bw()
# 
# #cv rank; norm decay rate per day
# filtered_data1 <- all_results_long %>%
#   filter(response_variable == 'scale_cube_Mean_Decay_Rate_per_day',
#          type == 'Normalized') %>%
#   arrange(abs(cv)) %>%  # Ensure order before setting factor
#   mutate(RowNames = factor(RowNames, levels = unique(RowNames)))  # Explicit ordering
# ggplot(filtered_data1, aes(x = abs(cv), y = RowNames)) + 
#   geom_point() +
#   theme_bw()+
#   ggtitle("norm decay rate per day")
# 
# #cv rank; not norm decay rate per day
# filtered_data2 <- all_results_long %>%
#   filter(response_variable == 'scale_cube_Mean_Decay_Rate_per_day',
#          type == 'Not_Normalized') %>%
#   arrange(abs(cv)) %>%  # Ensure order before setting factor
#   mutate(RowNames = factor(RowNames, levels = unique(RowNames)))  # Explicit ordering
# 
# ggplot(filtered_data2, aes(x = abs(cv), y = RowNames)) + 
#   geom_point() +
#   theme_bw()+
#   ggtitle("not norm decay rate per day")
# 
# #cv rank; norm degree decay rate
# filtered_data3 <- all_results_long %>%
#   filter(response_variable == 'scale_cube_Mean_degree_decay_rate',
#          type == 'Normalized') %>%
#   arrange(abs(cv)) %>%  # Ensure order before setting factor
#   mutate(RowNames = factor(RowNames, levels = unique(RowNames)))  # Explicit ordering
# 
# ggplot(filtered_data3, aes(x = abs(cv), y = RowNames)) + 
#   geom_point() +
#   theme_bw()+
#   ggtitle("norm degree decay rate")
# 
# #cv rank; not norm degree decay rate
# filtered_data4 <- all_results_long %>%
#   filter(response_variable == 'scale_cube_Mean_degree_decay_rate',
#          type == 'Not_Normalized') %>%
#   arrange(abs(cv)) %>%  # Ensure order before setting factor
#   mutate(RowNames = factor(RowNames, levels = unique(RowNames)))  # Explicit ordering
# 
# ggplot(filtered_data4, aes(x = abs(cv), y = RowNames)) + 
#   geom_point() +
#   theme_bw()+
#   ggtitle("not norm degree decay rate")
# 
# ================================ create out table ============================

output <- all_results_long %>%
  mutate(mean = signif(mean, 3),
         abs_mean = abs(mean),
         sd = signif(sd, 3),
         cv = signif(cv, 3),
         abs_cv = abs(cv),
         cv = case_when(is.na(cv) ~ '',
                        TRUE ~ as.character(cv)),
         RowNames = str_remove(RowNames, 'scale_cube_'),
         type = str_replace(type, 'Not_Normalized', 'Not Normalized' )) %>%
  left_join(variable_names %>% select(original, labels), by = c('RowNames' = 'original')) %>%
  rename(Predictor = labels)%>%
  clipr::write_clip()

out_r2 <- mean_r2_all %>%
  mutate(mean_r2 = signif(mean_r2, 3),
         sd = signif(sd, 3))%>%
  rename('Mean R2' = mean_r2) %>%
  mutate('Response Variable' = case_when(response_variable == 'scale_cube_Mean_degree_decay_rate' ~ 'Kdd',
                                         response_variable == 'scale_cube_Mean_Decay_Rate_per_day' ~ 'Kcd')) %>%
  select('Response Variable', 'Mean R2', sd) %>%
  clipr::write_clip()



