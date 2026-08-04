# =================================== Objectives =================================
#
# Script : 03.1_variable_selection_for_LASSO.R
# Project: rc_sfa-rc-3-wenas-meta
# Purpose: Perform variable selection techniques (PCA and VIF) for final LASSO data frame.  
#         
#
# Inputs :
#   - ~/Library/CloudStorage/OneDrive-PNNL/Documents/GitHub/rc_sfa-rc-3-wenas-meta/Output_for_analysis/03_merge_geospatial/03_master_merged_Brie_LASSO.csv
#   
# Output :
#   - ***
#
#
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 04 August 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")
rm(list=ls(all=T)) #this clears your Environment

library(pacman)
p_load(tidyverse,
       here,
       janitor,
       factoextra,
       FactoMineR,
       corrplot)

# ---- 1. Read inputs --------------------------------------------------------
# This is the data frame with all the variables of interest 
all_variables <- read_csv(
  here("Output_for_analysis", "03_merge_geospatial",
       "03_master_merged_Brie_LASSO.csv"))

# ======== Predictors ===========
predictors_burn <- c(
  "ag_cover",
  "runoffws",
  "bfiws",
  "rckdepws",
  "burn_percent_fire_year",
  "forest_cover",
  "glacial_till",
  "grassland_cover",
  "burn_sev_high",
  "AI_longterm",
  "burn_sev_low",
  "precip8110ws",
  "tmean8110ws",
  "burn_sev_mod",
  "clayws",
  "omws",
  "permws",
  "urban_cover",
  "Area_watershed_km",
  "wetland_cover")

predictors_unburn <- c(
  "ag_cover",
  "runoffws",
  "bfiws",
  "rckdepws",
  
  "forest_cover",
  "glacial_till",
  "grassland_cover",
  
  "AI_longterm",
  
  "precip8110ws",
  "tmean8110ws",
  
  "clayws",
  "omws",
  "permws",
  "urban_cover",
  "Area_watershed_km",
  "wetland_cover")

# ---- 2. PCA function --------------------------------------------------------
run_pca <- function(df, response, predictors){
  
  # Keep only variables needed
  dat <- df %>%
    select(all_of(response), all_of(predictors)) %>%
    drop_na()
  
  # Predictor matrix
  X <- dat %>%
    select(all_of(predictors))
  
  # Remove zero variance variables
  X <- X %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  # PCA
  pca <- prcomp(
    X,
    center = TRUE,
    scale. = TRUE
  )
  
  list(
    pca = pca,
    data = dat,
    predictors_used = names(X)
  )
}


# ---- 3. DOC Burn --------------------------------------------------------
# Collapse all rows into 1
burn_DOC <- all_variables %>%
  filter(
    Burn_Unburn == "Burn",
    !is.na(DOC_Interp_mg_C_L)
  ) %>%
  group_by(Comparison_ID, Pair) %>%
  slice(1) %>%
  ungroup()

# This should be 0 rows 
burn_DOC %>%
  count(Comparison_ID, Pair) %>%
  filter(n > 1)


# Run PCA function 
pca_doc_burn <- run_pca(
  df = burn_DOC,
  response = "DOC_Interp_mg_C_L",
  predictors = predictors_burn
)

doc_burn_loadings <- as.data.frame(
  pca_doc_burn$pca$rotation
)

doc_burn_loadings

# Scree plot (Variance explained)
fviz_eig(
  pca_doc_burn$pca,
  addlabels = TRUE
)

# Variable contribution plot 
fviz_pca_var(
  pca_doc_burn$pca,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
)

# Biplot
fviz_pca_biplot(
  pca_doc_burn$pca,
  repel = TRUE
)

fviz_pca_var(
  pca_doc_burn$pca,
  col.var = "contrib",
  repel = TRUE
) +
  theme_classic()
# Loadings represent the correlation between each variable and the principal component.
  # The absolute value indicates importance:
      # ~0.40+ = strong contribution
      # ~0.25–0.40 = moderate contribution
      # <0.20 = weaker contribution
doc_burn_loadings <- pca_doc_burn$pca$rotation[,1:3]

doc_burn_loadings

doc_burn_loadings %>%
  as.data.frame() %>%
  arrange(desc(abs(PC1)))

# PC1 interpretation: "Burn severity + open landscape vs forested, wetter, mineral watersheds"
  # grassland_cover
  # burn_sev_high
  # forest_corver
  # AI_longterm
  # burn_percent_fire_year
  # glacial_till
  # omws
  # rckdepws
  # precip8110ws 

# Suggested final model:
  # burn_sev_high
  # burn_percent_fire_year
  # AI_longterm
  # runoffws
  # forest_cover
  # grassland_cover
  # omws
  # Area_watershed_km 

# ---- 4. NO3 Burn --------------------------------------------------------
# Collapse all rows into 1
burn_NO3 <- all_variables %>%
  filter(
    Burn_Unburn == "Burn",
    !is.na(NO3_Interp_mg_N_L)
  ) %>%
  group_by(Comparison_ID, Pair) %>%
  slice(1) %>%
  ungroup()

# This should be 0 rows 
burn_NO3 %>%
  count(Comparison_ID, Pair) %>%
  filter(n > 1)


# Run PCA function 
pca_no3_burn <- run_pca(
  df = burn_NO3,
  response = "NO3_Interp_mg_N_L",
  predictors = predictors_burn
)

no3_burn_loadings <- as.data.frame(
  pca_no3_burn$pca$rotation
)

no3_burn_loadings
# Scree plot (Variance explained)
fviz_eig(
  pca_no3_burn$pca,
  addlabels = TRUE
)

# Variable contribution plot 
fviz_pca_var(
  pca_no3_burn$pca,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
)

# Biplot
fviz_pca_biplot(
  pca_no3_burn$pca,
  repel = TRUE
)

fviz_pca_var(
  pca_no3_burn$pca,
  col.var = "contrib",
  repel = TRUE
) +
  theme_classic()
# Loadings represent the correlation between each variable and the principal component.
# The absolute value indicates importance:
# ~0.40+ = strong contribution
# ~0.25–0.40 = moderate contribution
# <0.20 = weaker contribution
no3_burn_loadings <- pca_no3_burn$pca$rotation[,1:3]

no3_burn_loadings

no3_burn_loadings %>%
  as.data.frame() %>%
  arrange(desc(abs(PC1)))

# Suggested final model:
  # burn_sev_high
  # burn_percent_fire_year
  # AI_longterm
  # runoffws
  # clayws
  # forest_cover
 

  # Area_watershed_km 

# NO3 ~ burn_percent_fire_year +
# AI_longterm +
#   runoffws +
#   clayws +
#   forest_cover +
#   Area_watershed_km
# + (random effects)



















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


# ---- 5. DOC Unburn --------------------------------------------------------
# Collapse all rows into 1
unburn_DOC <- all_variables %>%
  filter(
    Burn_Unburn == "Unburn",
    !is.na(DOC_Interp_mg_C_L)
  ) %>%
  group_by(Comparison_ID, Pair) %>%
  slice(1) %>%
  ungroup()

# This should be 0 rows 
unburn_DOC %>%
  count(Comparison_ID, Pair) %>%
  filter(n > 1)

# Run PCA function 
pca_doc_unburn <- run_pca(
  df = unburn_DOC,
  response = "DOC_Interp_mg_C_L",
  predictors = predictors_unburn
)

doc_unburn_loadings <- as.data.frame(
  pca_doc_burn$pca$rotation
)

doc_unburn_loadings

# Scree plot (Variance explained)
fviz_eig(
  pca_doc_unburn$pca,
  addlabels = TRUE
)

# Variable contribution plot 
fviz_pca_var(
  pca_doc_unburn$pca,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
)

# Biplot
fviz_pca_biplot(
  pca_doc_unburn$pca,
  repel = TRUE
)

fviz_pca_var(
  pca_doc_unburn$pca,
  col.var = "contrib",
  repel = TRUE
) +
  theme_classic()
# Loadings represent the correlation between each variable and the principal component.
# The absolute value indicates importance:
# ~0.40+ = strong contribution
# ~0.25–0.40 = moderate contribution
# <0.20 = weaker contribution
doc_unburn_loadings <- pca_doc_unburn$pca$rotation[,1:3]

doc_unburn_loadings

doc_unburn_loadings %>%
  as.data.frame() %>%
  arrange(desc(abs(PC1)))

# Suggested final model:
# burn_sev_high
# burn_percent_fire_year
  # AI_longterm
  # runoffws
  # forest_cover
  # wetland_cover
# grassland_cover
  # omws
  # Area_watershed_km 

# DOC ~ forest_cover +
# wetland_cover +
#   AI_longterm +
#   runoffws +
#   omws +
#   Area_watershed_km

# Unburned DOC:
# DOC ~ forest_cover +
#   wetland_cover +
#   AI_longterm +
#   runoffws +
#   omws +
#   Area_watershed_km

# Burned DOC:
# DOC ~ burn_sev_high
#   forest_cover +
#   
#   AI_longterm +
#   runoffws +
#   omws +
#   Area_watershed_km

# ---- 6. NO3 Unburn --------------------------------------------------------
# Collapse all rows into 1
unburn_NO3 <- all_variables %>%
  filter(
    Burn_Unburn == "Unburn",
    !is.na(NO3_Interp_mg_N_L)
  ) %>%
  group_by(Comparison_ID, Pair) %>%
  slice(1) %>%
  ungroup()

# This should be 0 rows 
unburn_NO3 %>%
  count(Comparison_ID, Pair) %>%
  filter(n > 1)

# Run PCA function 
pca_no3_unburn <- run_pca(
  df = unburn_NO3,
  response = "NO3_Interp_mg_N_L",
  predictors = predictors_unburn
)

no3_unburn_loadings <- as.data.frame(
  pca_no3_unburn$pca$rotation
)

no3_unburn_loadings

# Scree plot (Variance explained)
fviz_eig(
  pca_no3_unburn$pca,
  addlabels = TRUE
)

# Variable contribution plot 
fviz_pca_var(
  pca_no3_unburn$pca,
  col.var = "contrib",
  gradient.cols = c("blue", "yellow", "red"),
  repel = TRUE
)

# Biplot
fviz_pca_biplot(
  pca_no3_unburn$pca,
  repel = TRUE
)

fviz_pca_var(
  pca_no3_unburn$pca,
  col.var = "contrib",
  repel = TRUE
) +
  theme_classic()
# Loadings represent the correlation between each variable and the principal component.
# The absolute value indicates importance:
# ~0.40+ = strong contribution
# ~0.25–0.40 = moderate contribution
# <0.20 = weaker contribution
no3_unburn_loadings <- pca_no3_unburn$pca$rotation[,1:3]

no3_unburn_loadings

no3_unburn_loadings %>%
  as.data.frame() %>%
  arrange(desc(abs(PC1)))

# unburned nitrate 
# NO3 ~ AI_longterm +
#   runoffws +
#   forest_cover +
#   omws +
#   Area_watershed_km

# burned nitrate 
# NO3 ~ burn_percent_fire_year +
# AI_longterm +
#   runoffws +
#   clayws +
#   forest_cover +
#   Area_watershed_km
# + (random effects)

# ---- 7. Synthesize all PCAs --------------------------------------------------------
# AI_longterm
# runoffws
# forest_cover 
# omws
# Area_watershed_km 
# wetland_cover
# bfiws>permws
# clayws
# burn_percent_fire_year or burn_sev_high 
