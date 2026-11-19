# Exploratory working-covariance REML

Reclassified at author request October 27, 2026. These analyses are excluded from all paper runner modes and manuscript readiness requirements. Saved results preserve the post-exclusion 103-record input; moving them does not refit or change estimates.

## Exploratory conditional REML inference

The conditional covariance script fits REML pooled-effect and time-since-fire models with study/pair random intercepts. It evaluates nine working sampling-covariance scenarios using reference-family correlation rho and annual temporal correlation phi in {0, 0.5, 0.8}. The matrix is a positive-semidefinite mixture of pair and shared-reference AR(1) kernels scaled by the provisional annual delta standard errors. These scenarios are assumed correlations, not estimates of daily autocorrelation. Shared-reference dependence is represented in the working covariance, not by a third potentially redundant random intercept.

Study-clustered CR2 standard errors and Satterthwaite degrees of freedom are computed through `metafor::robust(..., clubSandwich=TRUE)`. This follows the [metafor robust inference documentation](https://wviechtb.github.io/metafor/reference/robust.html). Estimates, confidence intervals, degrees of freedom, heterogeneity components, model objects, and failures are saved. These conditional fits do **not** validate annual precision or correct the effective information content of interpolated observations. These REML fits are exploratory and are not required for manuscript synthesis. Primary manuscript synthesis is the author-selected balanced pooled estimator with connected study/fire-component CR2/Satterthwaite intervals; annual precision validation remains unresolved, and DOC has only six inference components across seven studies.

## Reproduction

From the repository root, after preparing the primary model table:

```sh
Rscript agent_workflows/vibe_coding/exploratory/R/02_fit_working_covariance_inference_agent_v1.R
```

The script writes `working_covariance_inference.csv`, `working_covariance_failures.csv`, and `working_covariance_models.rds` to `exploratory/output/working_covariance/`. The saved run completed all 36 fits (54 coefficient rows). Historical execution logs remain in `output/logs/`. Annual variance validation is required before promoting this method to manuscript inference.
