# =================================== Objectives =================================
#
# Script : 03.5_Sparse_Modeling.R
# Project: rc_sfa-rc-3-wenas-meta
# Purpose: Introduction to Sparse Modeling in R
#          
# Follows this workflow: https://zenodo.org/records/17101921
  # title: "Sparse Modeling Introduction and Practice"
  # author: "Modelscapes"
  # date: "2025-05-15"
  # output: html_document
  # self-contained: true
# Inputs :
#   - Output_for_analysis/03_merge_geospatial/03_master_merged.csv
#
# Output :
#   - 
#
#
#
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 13 May 2026

# ============================ Libraries ===========================
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")
rm(list=ls(all=T)) #this clears your Environment

library(pacman)
p_load(tidyverse,
       here,
       janitor)

# ---- 1. Read inputs --------------------------------------------------------
master_df <- read_csv(
  here("Output_for_analysis", "03_merge_geospatial",
       "03_master_merged.csv"),
  na = c("", "NA", "-9999", "N/A"),
  show_col_types = FALSE
)

---
  title: "03.5 Sparse Modeling — Identifying Key Covariates for DOC and NO3"
author: "Your name"
date: "`r Sys.Date()`"
output: html_document
---
  

library(tidyverse)
library(here)
library(glmnet)      # LASSO
library(susieR)      # SuSiE
library(corrplot)    # covariate collinearity check
library(janitor)     # clean names

