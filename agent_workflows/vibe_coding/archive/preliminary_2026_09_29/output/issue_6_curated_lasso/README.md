# Issue #6: human-curated predictor rerun

**Later primary sets:** The user-defined DOC and nitrate primary sets from 2026-09-29 are recorded in `config/issue6_revised_primary_predictors.csv`, with a combined violin figure and updated model results under `output/issue_6_revised_primary/`. This page documents the earlier human-curated comparison.

Source: [GitHub issue #6](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/6). Run on 2026-09-28 with `N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/09_issue6_curated_lasso_agent_v1.R`.

The paired annual lnRR table contains 37 DOC rows from 7 studies and 73 nitrate rows from 12 studies. The DOC candidate set combines high-severity burn, forest cover, wetland cover, annual runoff, soil organic matter, watershed area, and post-fire year. The nitrate set combines burned watershed percentage, forest cover, annual runoff, clay, soil organic matter, watershed area, and post-fire year. These are unions of the relevant human burned and reference concentration lists, adapted to one paired response. The two burn metrics have Spearman rho 0.92 in the existing audit, so each curated set contains one. The source snapshot and model table have no long-term aridity measurement. This rerun omits it and uses no proxy. Baseflow index replaces runoff in a separate sensitivity set.

Leave-one-study-out RMSE (`lambda.1se`, weighted LASSO; lower is better):

| Analyte | Current shared | Curated | Baseflow sensitivity | Intercept only |
|---|---:|---:|---:|---:|
| DOC | 0.253 | 0.263 | 0.269 | 0.331 |
| Nitrate | 1.100 | 1.100 | 1.100 | 1.100 |

The curated DOC model improves on the simple intercept benchmark but does not improve on the earlier shared predictor set. The nitrate LASSO shrank every predictor to zero in all 12 outer folds, so its held-out predictions match the intercept benchmark. In 1,000 study bootstrap refits of the curated sets, DOC soil organic matter was selected in 87.5%, runoff in 61.7%, and watershed area in 51.2%; other DOC predictors were selected less often. Every nitrate predictor was selected in fewer than 14% of refits. These selection rates are descriptive, not causal evidence.

The analysis retains the current response and variance construction, including its unresolved variance and shared-control assumptions. Preprocessing is estimated on each outer training set before held-out study prediction. The inner `cv.glmnet` folds share that outer-training transformation, following the existing workflow; inner-fold-specific imputation and scaling would require a separate nested preprocessing implementation. The 7-study DOC sample supports cautious interpretation.

Files: `performance.csv`, `heldout_predictions.csv`, `outer_coefficients.csv`, `selection_stability.csv`, and `bootstrap_coefficients.csv`. Failure logs are empty because all fits completed. The set definitions and rationale are in `config/issue6_predictor_dictionary.csv` and the executable script.

## Predictor distributions

`doc_predictor_boxplots.png` and `no3_predictor_boxplots.png` show raw predictor distributions, with sample variance (`s²`) and observation count in each panel. They include curated predictors, the extra burn predictor in the current shared set, and baseflow index from the sensitivity models. Watershed attributes contribute one value per watershed pair; post-fire year contributes one value per pair-year. Panel scales differ because the predictors have different units and ranges. Exact quartiles, ranges, and variances are in `predictor_distribution_summary.csv`. Regenerate these plots with `R/10_plot_issue6_predictor_boxplots_agent_v1.R`.

`doc_predictor_violins.png` and `no3_predictor_violins.png` show the same data as smoothed density violins, with individual observations and median bars overlaid. The script above generates both plot types.
