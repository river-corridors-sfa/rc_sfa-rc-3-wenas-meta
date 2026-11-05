# Revised primary predictor sets — 2026-09-29

The user-defined primary sets are recorded in `config/issue6_revised_primary_predictors.csv` and flagged in `config/issue6_revised_primary_dictionary.csv`.

- **DOC:** `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, `omws`.
- **Nitrate:** `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, `clayws`.

`all_revised_primary_predictor_violins.png` shows all nine distinct variables in one figure. DOC and nitrate are side by side where both use a predictor. Watershed attributes contribute one value per watershed pair; post-fire year contributes one value per pair-year. Points show observations, bars mark medians, and panel labels show sample variance. Exact values are in `revised_primary_predictor_distribution_summary.csv`.

The LASSOs were rerun with `N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/11_fit_issue6_revised_primary_agent_v1.R`. The combined figure is regenerated with `Rscript agent_workflows/vibe_coding/R/12_plot_issue6_revised_primary_violins_agent_v1.R`. The run completed with no fit or convergence warnings using a higher glmnet iteration limit.

| Analyte | Current shared-set RMSE | Revised primary RMSE | Intercept-only RMSE | Held-out studies |
|---|---:|---:|---:|---:|
| DOC | 0.253 | 0.247 | 0.331 | 7 |
| Nitrate | 1.100 | 1.100 | 1.100 | 12 |

RMSE is from leave-one-study-out held-out predictions using `lambda.1se`. For nitrate, every revised-primary predictor was shrunk to zero in all outer folds. Study bootstrap selection frequencies are in `selection_stability.csv`; 1,000 iterations completed per analyte. The two burn measures have Spearman correlation 0.92 in the existing audit. They are both included as requested, so individual selection frequencies may be sensitive to their redundancy. The current effect-size variance and shared-control assumptions remain provisional. Preprocessing is estimated on each outer training set; inner `cv.glmnet` folds share that transformation, following the previous workflow.

The prior issue #6 human-curated run remains under `output/issue_6_curated_lasso/` for comparison.
