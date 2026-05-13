# =================================== Objectives =================================
#
# Script : 03.5_Sparse_Modeling.R
# Project: rc_sfa-rc-3-wenas-meta
# Purpose: Introduction to Sparse Modeling in R
#          
# Follow this workflow: https://zenodo.org/records/17101921
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
