# ==============================================================================
# Script: 05_fit_grouped_lasso_agent_v1.R
# Status: PLACEHOLDER — requires finalized predictor dictionary
# Purpose: Compare benchmark models with annual-lnRR LASSO using held-out studies.
# Replaces, without modifying: R_scripts/Metaanalysis_LASSO.R.
# ==============================================================================

library(tidyverse)
library(here)
library(glmnet)

# ---- 1. Paths and analysis settings ----------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table_path <- file.path(workflow_dir, "data", "derived", "lasso_model_table.csv")
predictor_dictionary_path <- file.path(workflow_dir, "config", "predictor_dictionary.csv")
model_dir <- file.path(workflow_dir, "output", "models")
table_dir <- file.path(workflow_dir, "output", "tables")

primary_alpha <- 1
primary_lambda_rule <- "lambda.1se"
outer_group <- "Study_ID"

if (!file.exists(model_table_path) || !file.exists(predictor_dictionary_path)) {
  stop("Complete scripts 02 and 03, including predictor review, before LASSO.")
}

stop("PLACEHOLDER: implement grouped nested validation after predictor review.")

# ---- 2. Lock response and predictor definitions -----------------------------

# TODO: Use annual non-normalized lnRR_mean as the response.
# TODO: Fit DOC and nitrate separately with the same workflow.
# TODO: Read only author-approved primary predictors from predictor_dictionary.csv.
# TODO: Keep post-fire year in all scientific benchmark comparisons.

# ---- 3. Define outer leave-one-study-out splits -----------------------------

# TODO: Hold out every row from one study at a time.
# TODO: Use pair-grouped validation only as a labeled secondary analysis.
# TODO: Store fold membership so every result is reproducible.

# ---- 4. Preprocess inside each training fold --------------------------------

# TODO: Estimate transformations, missing-data handling, dummy variables,
#       centering, and scaling from the training data only.
# TODO: Apply the learned preprocessing to the held-out study.
# TODO: Remove zero-variance columns using training data only.

# ---- 5. Tune and fit within each outer training set -------------------------

# TODO: Select lambda using grouped inner folds; use lambda.1se as primary.
# TODO: Fit intercept-only, time-only, time-plus-fire, and full LASSO models.
# TODO: Decide and document how approximate sampling weights enter glmnet.

# ---- 6. Evaluate held-out predictions --------------------------------------

# TODO: Calculate RMSE, MAE, and cross-validated R2 from pooled held-out
#       predictions, not predictions on the training data.
# TODO: Save predictions, fold-specific lambdas, coefficients, and metrics.

