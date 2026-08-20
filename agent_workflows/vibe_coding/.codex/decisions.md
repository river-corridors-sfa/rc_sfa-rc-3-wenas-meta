# Project decisions

This file records current architectural and analytical decisions for the `agent_workflows/vibe_coding/` workflow. Items marked **provisional** require author review before the final analysis.

## Workflow organization

- All LLM-generated files belong under `agent_workflows/vibe_coding/`.
- Human-written scripts elsewhere in the repository will not be overwritten.
- When human code is adapted, a new file with an `_agent_v1` suffix will be created and the source script will be cited in its header.
- R scripts should be linear and readable from top to bottom. Functions will be introduced only for genuinely repeated or complex logic.
- Generated data, figures, tables, models, and logs will be separated from immutable source snapshots.
- Retained source files will be documented in `config/source_manifest.csv` with source commit and blob SHAs.

## Scientific scope

- The primary question is which watershed, fire, climate, and recovery characteristics explain heterogeneity in post-wildfire DOC and nitrate responses.
- The main response is the non-area-normalized burned-to-reference log response ratio:
  
  `lnRR = log(concentration_burned / concentration_reference)`.
- DOC and nitrate will be modeled separately in the primary analysis and compared using the same analytical framework.
- Watershed area will be considered as a predictor, not used as a concentration denominator.
- Area-normalized effect sizes, pseudo-yield terminology, and normalized-versus-nonnormalized comparisons are excluded from the new primary analysis.

## Analysis unit and dependence

- The intended modeling row is watershed pair × analyte × post-fire year.
- The hierarchy is study → fire/comparison → watershed pair → post-fire year.
- Genuine watershed pairs and repeated post-fire years will be retained rather than collapsed to one observation per study.
- Records from the same watershed pair must remain together during resampling.
- Study-grouped or leave-one-study-out validation will be the primary test of transferability to new studies.
- Pair-grouped validation may be reported secondarily to assess prediction for new pairs within represented study contexts.
- **Provisional:** Automatically generated burned × unburned combinations will not be accepted as independent pairs without review of the original study design.
- **Provisional:** Effect sizes sharing a reference watershed will be retained only with explicit shared-control identifiers and appropriate dependence handling.

## Source data

- The paired daily chemistry table and site-level geospatial attribute table are the primary upstream inputs.
- Site metadata is an active reference for reconciling study, fire, comparison, and pairing identifiers.
- The existing annual effect-size table is a QC benchmark, not the authoritative final modeling table.
- GIS files, GridMET rasters, watershed polygons, and large duplicate merged tables will remain in their existing repository locations rather than being copied into the active workflow.
- The manual Gaviota COMID correction must remain documented.
- Interpolated observations must be identifiable so observed-only sensitivity analyses can be performed.

## Modeling strategy

- Multilevel meta-analysis will estimate average effects, temporal recovery, heterogeneity, and uncertainty.
- LASSO or elastic net will assess out-of-study prediction and predictor-selection stability; selected variables will not be interpreted as causal mechanisms.
- Predictor preprocessing, including transformations, imputation, and standardization, must be estimated inside each training fold.
- `lambda.1se` will be the primary LASSO penalty choice; `lambda.min` may be reported as a sensitivity analysis.
- Model performance must be calculated from held-out predictions, not fitted values from the training data.
- Benchmark models will include intercept-only, time-only, and time-plus-fire specifications.
- Selection frequency, coefficient direction, and sign stability across cluster-level resamples will be reported.
- Elastic net, weighting choices, influential-study removal, shared-control handling, interpolation handling, and matched DOC–nitrate subsets will be sensitivity analyses.

## Predictor selection

- Candidate predictors will be prespecified from scientific hypotheses, data completeness, and redundancy diagnostics.
- PCA and correlation matrices are diagnostic tools, not supervised predictor-selection procedures.
- Continuous predictors are preferred over arbitrary bins.
- Highly redundant predictors will not be entered together without an explicit ecological or statistical justification.
- Final predictor inclusion decisions and transformations will be recorded in a predictor dictionary before fitting final models.

## Historical analyses retained but inactive

The following remain preserved elsewhere in the repository but are not part of the active workflow:

- random-forest analyses;
- burned and unburned absolute-concentration LASSOs;
- PCA-derived suggested models;
- area-normalization analyses;
- exploratory concentration scatterplots;
- `LASSO_results.docx`.

## Unresolved decisions

1. Confirm designated watershed pairs versus automatically constructed comparisons for every study.
2. Decide how to represent multiple controls and effect sizes sharing one control.
3. Determine the final effect-size variance or weighting approach given interpolation and temporal autocorrelation.
4. Confirm whether annual summaries should use calendar year, discrete time-since-fire year, or both.
5. Finalize the candidate predictor set after pair-level missingness and redundancy audits.
6. Decide whether the small DOC study count supports a primary LASSO claim or only an exploratory stability analysis.

## Provisional use of established lnRR pairings

- The 36 distinct pairs already present in `effect_sizes_yearly.csv` are treated as previously manually reviewed for interim workflow development.
- This is an explicit provisional assumption, not a replacement for co-author confirmation.
- The original `pairing_decisions.csv`, review workbook, and final review fields remain unchanged.
- A separate `pairing_decisions_analysis.csv` records all 36 pairs as provisionally included and keeps `Coauthor_Confirmation = pending`.
- Pairs flagged as sharing a reference are provisionally classified as `designated_shared_reference` and retain `shared_control_id` for dependence handling.
- `Comparison_ID` is used as `Fire_ID_Analysis` until multi-fire attribution is confirmed; final fire identifiers are not inferred.
- Final analysis and manuscript reporting remain conditional on co-author confirmation or completion of the review workbook.
