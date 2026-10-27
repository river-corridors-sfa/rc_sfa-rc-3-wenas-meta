# Main text references to statistical supporting information

## Results Section 3.1 and pooled synthesis Methods

Replace the generic pooled-response sensitivity reference “(SI)” with “(Supporting Information Section S1, Tables S1–S4)”. Table S2 covers clustering; Table S3 covers interpolation on matched annual inventories; Table S4 covers point-estimate influence. Deletion confidence intervals were not calculated.

## Results Section 3.2 and cross validation Methods

Use “(Supporting Information Section S2, Tables S5–S9; SI Figures 1 and 2)” for predictors, preprocessing, performance metrics, publication-specific errors and conditional uncertainty. In particular, “Publication-specific prediction errors and the consistency and conditional uncertainty of predictive improvements are reported in Supporting Information Section S2, Tables S7 and S8.”

For the new uncertainty calculation, add: “We quantified conditional uncertainty in RMSE improvement using 100,000 paired resamples of connected publication–wildfire groups, keeping publications sharing a wildfire together. Resampling used saved held-out publication MSE vectors without refitting models; overall RMSE was recomputed as the square root of the equally weighted mean publication MSE. We report 95% percentile intervals.” These intervals do not remove shared-fire information across the original publication validation splits or fully capture overlapping-training/refitting uncertainty.

The Figure 3 group models leave time unpenalized. Distinguish their two-fire-metric LASSO group from the one-fire-metric linear benchmark accompanying the all-penalized full-set models (Table S9). Pooled annual-record RMSE, MAE and R² for all five Figure 3 models are now explicitly calculated in Table S6 from their saved predictions.

## Results Section 3.3 and bootstrap Methods

Use “(Supporting Information Section S3, Tables S10–S14; SI Figure 3)” for full-set selection/sign summaries, coefficient distributions, alternative weighting and individual/group omissions. A replacement closing sentence is: “Selection frequencies, coefficient distributions, alternative-weighting comparisons, and structured individual and group predictor-omission analyses are reported in Supporting Information Section S3 (Tables S10–S14; SI Figure 3).”

Alternative weighting applies to the all-candidate-penalized full-set models, not to a rerun of the Figure 3 group models. Precision/family weights use provisional variances and change both precision and the reference multiplier. Within-publication inner scoring also differs between primary balance weights and alternative weighting conditions; these are not strictly isolated fitting-weight comparisons.
