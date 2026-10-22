# Annual uncertainty and weighting implementation

The primary predictive analysis now assigns equal total weight to each study, distributed equally among its pairs and then the retained annual records of each pair. It does not use the provisional daily delta variance to assign precision. Following the October 22 author-directed exclusion, the paper inventory is 103 records (35 DOC and 68 nitrate); the annual arithmetic-mean ratio definition is unchanged. The excluded Crandall 2018 Site_2–Control_2 nitrate record is preserved upstream and in the exclusion audit. This is a post hoc response-based exclusion, not a confirmed data error.

## Actual sampling support

The audit in `data/audit/paper_sampling_support.csv` (relative to the workflow root) reports positive matched observed dates, observed spacing, interpolated fraction, and covered duration for every paper record. Median matched observed-date counts are six for DOC and 4.5 for nitrate. An observed-only analysis requiring at least two such dates retains 32 DOC and 56 nitrate annual records. The other 15 records remain in the primary analysis but are explicitly excluded from this sensitivity.

Only 18 records, all from Murphy, pass an exploratory screen of at least 20 matched observed dates spanning at least 90 days; even these have maximum gaps of 42–57 days. The screen is diagnostic rather than a validated threshold. No automatic temporal block-bootstrap variance has been assigned. Defining defensible seasonal/time blocks, preserving joint shared-reference trajectories, and evaluating interpolation after resampling remain necessary before such a variance could replace the provisional delta estimate. Resampling interpolated daily rows independently would not solve the problem.

## Prediction and sensitivity design

- Primary: equal study totals, equal pair totals within each study, equal annual weights within each pair. Weights are recalculated within each training subset. In study bootstraps, each draw receives equal total mass, preserving selection multiplicity.
- Equal-row sensitivity: every annual record receives the same fitting weight.
- Precision/family sensitivity: retain the earlier inverse-delta-variance weights, training-data median fallback, 95th-percentile cap, and shared-reference family multiplier. This is explicitly a provisional weighting comparison.
- Observed-only sensitivity: recompute both annual concentration means, their log ratio, and the diagnostic delta variance using only same-date observations in both watersheds.
- Matched-interpolated sensitivity: rerun the primary response on exactly the observed-only-eligible record inventory. Compare this with observed-only results to distinguish interpolation effects from removal of records.

All scenarios use the author-selected eight-predictor sets, leave-one-study-out evaluation, and 1,000 study-bootstrap attempts per analyte. Inner-fold screening, median imputation, scaling, and fitting-weight estimation use only inner training data. Repeated copies of an original study always share a tuning fold. A fixed 80-point penalty grid from 100 to 0.0001 avoids deriving a grid from validation outcomes; the largest penalty within one standard error of the best study-level validation loss is selected. Inner loss is averaged across original studies, with bootstrap multiplicity retained in its mean and unique studies used for its SE. Primary inner losses also balance pairs within studies. Other weighting scenarios retain study-level tuning loss so their target remains transfer across studies.

Outer performance files report pooled row RMSE/MAE/R², per-study errors, and equal-study RMSE/MAE. Equal-study RMSE is the square root of mean study MSE; mean study RMSE is exported separately. Pooled R² uses deviations from the pooled observed response mean as its denominator. The benchmark models are trained only on outer training studies.

Bootstrap attempts with fewer than three distinct studies are recorded as failures. Selection frequencies use successful refits only; failed fits do not count as zero coefficients. See each scenario's `bootstrap_summary.csv` and `bootstrap_failures.csv`. Predictors screened out of a successful fit count as unselected.

## Primary balanced pooled inference

Equal-study → equal-pair → equal-available-year weighted pooled log ratios use connected study/fire-component CR2 intervals with Satterthwaite degrees of freedom, nonprecision weights and an identity working target. Study/fire clustering links Rhea and Writer via High Park, leaving six DOC and eleven nitrate components. Publication-cluster, observed-only and matched-interpolated summaries are supporting comparisons; time associations remain exploratory. The exclusion is applied before recomputing balance weights for every inventory. Current coefficients, weights and deletion diagnostics are under `balanced_synthesis/`; the inclusion comparison is in `nitrate_exclusion_report.md` and the archived pre-exclusion package.

## Conditional pooled-effect inference

The conditional covariance script fits REML pooled-effect and time-since-fire models with study/pair random intercepts. It evaluates nine working sampling-covariance scenarios using reference-family correlation rho and annual temporal correlation phi in {0, 0.5, 0.8}. The matrix is a positive-semidefinite mixture of pair and shared-reference AR(1) kernels scaled by the provisional annual delta standard errors. These scenarios are assumed correlations, not estimates of daily autocorrelation. Shared-reference dependence is represented in the working covariance, not by a third potentially redundant random intercept.

Study-clustered CR2 standard errors and Satterthwaite degrees of freedom are computed through `metafor::robust(..., clubSandwich=TRUE)`. This follows the [metafor robust inference documentation](https://wviechtb.github.io/metafor/reference/robust.html). Estimates, confidence intervals, degrees of freedom, heterogeneity components, model objects, and failures are saved. These conditional fits do **not** validate annual precision or correct the effective information content of interpolated observations. These REML fits are conditional sensitivities. Primary manuscript synthesis is the author-selected balanced pooled estimator with connected study/fire-component CR2/Satterthwaite intervals; annual precision validation remains unresolved, and DOC has only six inference components across seven studies.

## Files and reproduction

Run `N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R` from the repository root. The sampling-support audit and both supporting sensitivity scripts are included in `all` mode. Install `clubSandwich` in the normal R library or the workflow-local `.R-library/`; package versions are recorded in `output/logs/uncertainty_session_info.txt` relative to the workflow root.

- `tables/fitting_weights.csv`: primary fitting weights.
- `tables/observed_only_response_comparison.csv`: annual response differences and eligibility.
- `tables/weighting_interpolation_performance.csv`: scenario comparisons.
- `tables/bootstrap_summary.csv`: primary success denominators.
- `sensitivity/`: separate outputs for four sensitivity scenarios.
- `tables/working_covariance_inference.csv` and `working_covariance_models.rds`: conditional pooled/time results.
- `uncertainty_weighting_run.log` and `weighting_checks.log`: execution and focused validation.

Temporal block-bootstrap variances have deliberately not been manufactured for sparsely observed years. The completed audit identifies the remaining methodological work rather than marking annual uncertainty finalized.

## Completed run and checks

All outer fits completed. Each scenario completed 994/1,000 DOC and 1,000/1,000 nitrate bootstrap attempts; the six DOC failures contain fewer than three distinct studies and are explicitly logged. All 36 conditional multilevel fits completed (54 coefficient rows). Equal-study LASSO held-out RMSE was:

| Scenario | DOC | Nitrate |
|---|---:|---:|
| Primary | 0.279 | 0.867 |
| Equal row | 0.259 | 0.863 |
| Precision/family | 0.418 | 0.838 |
| Observed only | 0.330 | 0.921 |
| Matched interpolated | 0.191 | 0.820 |

The observed-only and interpolated targets differ, so these error differences do not establish that interpolation improves prediction of the underlying true annual response. They demonstrate sensitivity to response construction. Comparisons of primary versus matched-subset results additionally reflect coverage differences.

Checks verified equal primary study totals, equal pair totals within studies, bootstrap-draw weighting, training-only preprocessing, observed-only eligibility, unique held-out predictions with complete coverage, positive finite CR2 degrees of freedom, and recorded bootstrap denominators. All five figures regenerated; Figure 3 was visually inspected. The updated inventory is 103 records; all inputs retain the exact-key exclusion.
