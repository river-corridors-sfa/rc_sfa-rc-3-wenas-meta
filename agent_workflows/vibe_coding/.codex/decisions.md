# Project decisions

## September 30, 2026 — Annual arithmetic mean ratio selected

- User selected `log(mean annual C_b / mean annual C_u)` as the primary response, superseding the mean of daily log ratios. Compute both arithmetic means on the same existing eligible matched positive-concentration dates within each calendar year; retain current interpolation, site selections, and time predictors. Partial-year summaries describe available coverage, not a reconstructed complete year. Zero/nonpositive-date eligibility remains a separate methods review.
- Retain `lnRR_mean` as the downstream response column; export `annual_mean_burn`, `annual_mean_reference`, `effect_size_definition`, and `variance_method`. Retain daily-log summaries only as explicitly named diagnostics.
- Replace daily-log `SD²/n` with provisional paired delta variance: `var(C_b/mean(C_b) - C_u/mean(C_u)) / n`. This includes same-date burned/reference covariance but assumes independent daily pairs; temporal correlation, interpolation, shared-reference dependence, and final manuscript uncertainty remain unresolved. Singletons retain missing variance.
- Restore the archived strict workbook importer and its input inventory/workbook to active paths so reviewed rebuilding works.

## September 30, 2026 — Pairing, gap-fill, and variance status clarified

- The pairing review workbook settled which controls and watershed pairs to retain: 28 approved/included pairs and 8 excluded pairs passed strict import. Do not treat control selection or pair approval as an unresolved decision.
- The observation-preserving gap-fill fix is active. It restored the Writer 2013 nitrate variance lost when an observed date after a gap was accidentally removed. The current model table has 110 annual response rows; six still have no calculable annual variance because each has exactly one matched observed date (four Murphy 2010 rows and two Writer 2012 rows). These are genuine singletons, not evidence that the gap-fill bug persists.
- Approval of a shared control for multiple burned watersheds establishes valid pairings. The separate statistical question is how to account for dependence among effect sizes that reuse that control. The current model table records shared-control IDs and gives each pair a `reference_family_weight` of `1 / n_pairs_sharing_reference`; this is a weighting convention, not an exact sampling covariance model.
- Before the annual arithmetic-mean change, annual variance was `lnRR_sd^2 / lnRR_n` across matched daily log ratios. Whether this is the manuscript's final uncertainty/weighting approach given temporal correlation, interpolation, and singleton rows remains a method decision. This is distinct from the fixed gap-fill bug and approved control choices.
- These clarifications supersede older provisional pairing language below. The active annual effects come from reviewed site selections and corrected chemistry, not the historical established annual-effect source.

## September 29, 2026 — Paper workflow organization

- The active outputs are Figure 1 maps, Figure 2 response boxplots, Figure 3 final-primary LASSO results, SI Figure 1 correlations of all 24 available predictor-like fields, and SI Figure 2 violins for only the final DOC and nitrate sets.
- Active figure code stays in `R/`; manuscript-facing figures and model tables are under `output/paper/`. The paper runner is `R/run_paper_workflow_agent_v1.R` and uses the user-defined sets in `config/issue6_revised_primary_predictors.csv`.
- Earlier model variants, provisional outputs, and obsolete runners are preserved under `archive/preliminary_2026_09_29/`. The human map source outside the agent workflow is unchanged.

## September 22 — Corrected full run completed

The full corrected workflow ran with 1000 bootstrap iterations per analyte/scenario. This supersedes earlier notes stating models and figures are stale. Current outputs use corrected chemistry and reviewed named-site selections. Execution is complete, but current variance, predictor, temporal, and bootstrap-method assumptions remain provisional; see output/reports/corrected_run_2026-09-22.md. No scientific defaults were changed during this run.

## September 22 — Named sites and corrected effects are active

Author confirmed exclusion of Fish Creek, Lower MacDonald Creek, and Upper MacDonald Creek from paired analysis: other references are fire-impacted and other burned sites involve multiple fires and/or large-lake influence. Keep source records for provenance; retain Coal–Pinchot. Enforce Rhea U1 and Crandall Hobble Creek Lower/Mill Race at physical-site level. Script 01c generates an evidence-bearing site configuration and corrected annual effects; script 02 now reads those derived inputs. This supersedes use of the established annual source for active model-table responses. Existing annual variance and timing methods remain provisional; models and figures have not been refitted.

This file records current architectural and analytical decisions for the `agent_workflows/vibe_coding/` workflow. Items marked **provisional** require author review before the final analysis.

## September 28, 2026 — Issue #6 curated predictor rerun

- Adapt the human burned and unburned concentration predictor lists as analyte-specific unions for paired annual lnRR, retaining post-fire year. DOC uses high-severity burn, wetland cover, forest cover, runoff, soil organic matter, and watershed area. Nitrate uses burned watershed percentage, clay, forest cover, runoff, soil organic matter, and watershed area.
- Do not enter the two burn measures together because their audited Spearman correlation is 0.92. Use baseflow index in a hydrology sensitivity that replaces runoff.
- Long-term aridity is unavailable in the current source snapshot and model table; omit it without a proxy. The curated sets therefore remain incomplete relative to the human lists.
- The issue #6 run is a separate comparison and does not replace the primary outputs or settle earlier variance and shared-control decisions. Leave-one-study-out results and 1,000-bootstrap stability are recorded under `output/issue_6_curated_lasso/`; predictor choices are in `config/issue6_predictor_dictionary.csv`.

## September 29, 2026 — Revised primary sets requested by user

- The primary DOC set is `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, and `omws`.
- The primary nitrate set is the same first seven predictors, with `clayws` in place of `omws`.
- These explicit user-defined sets supersede the earlier issue #6 curated candidate sets as the active primary specification. Both correlated burn metrics and both hydrology metrics are included by request. Keep the earlier results for comparison.
- The authoritative ordered lists are in `config/issue6_revised_primary_predictors.csv`; flagged metadata are in `config/issue6_revised_primary_dictionary.csv`. Updated fits and the combined nine-variable violin plot are in `output/issue_6_revised_primary/`.

## September 18, 2026 — Reviewed pairing decisions promoted

- The Excel workbook is authoritative for pair inclusion, fire identifiers, pairing types, and review evidence. Its 28 approved/included and 8 excluded rows passed strict import. This supersedes the provisional pairing-adoption entries below.
- `01b_promote_reviewed_pairings_agent_v1.R` refreshes the strict import and analysis configuration. Both runners use it instead of the provisional generator.
- Analysis status maps workbook `approved` to `confirmed`, and retains `excluded` rows for provenance. Shared-control IDs are preserved; family counts are recalculated among included pairs, with original counts retained separately.
- Pair confirmation does not finalize effect-size construction, timing, predictor choices, or inference. Scripts 02–03 produced 106 annual rows; stop at the pre-model audit checkpoint to resolve these scientific choices.
- Existing August 20 models, figures, and model-result tables remain provisional and are stale relative to the reviewed configuration. No model rerun occurred in this session.

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
- The included burned × reference pairs were approved through the review workbook; no automatically generated combinations are admitted as independent pairs without that review.
- Approved pairs may share a reference watershed. Preserve their shared-control identifiers and evaluate how this dependence affects uncertainty and weighting.

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

1. Decide whether the current family weighting adequately handles dependence among effect sizes sharing an approved control, or whether manuscript inference needs a more explicit covariance approach.
2. Determine the final effect-size variance or weighting approach given temporal autocorrelation, interpolation, and six genuine singleton annual rows. The gap-fill data-loss bug has been fixed.
3. Confirm whether annual summaries should use calendar year, discrete time-since-fire year, or both.
4. Document the rationale for including both highly correlated burn predictors in the current user-defined primary sets, and finalize predictor decisions after missingness and redundancy audits.
5. Decide whether the small DOC study count supports a primary LASSO claim or only an exploratory stability analysis.

## Historical, superseded: provisional use of established lnRR pairings

- The following bullets record the interim state before the reviewed workbook was promoted; they do not describe the active analysis.
- The 36 distinct pairs already present in `effect_sizes_yearly.csv` were treated as previously manually reviewed for interim workflow development.
- This is an explicit provisional assumption, not a replacement for co-author confirmation.
- The original `pairing_decisions.csv`, review workbook, and final review fields remain unchanged.
- A separate `pairing_decisions_analysis.csv` records all 36 pairs as provisionally included and keeps `Coauthor_Confirmation = pending`.
- Pairs flagged as sharing a reference are provisionally classified as `designated_shared_reference` and retain `shared_control_id` for dependence handling.
- `Comparison_ID` is used as `Fire_ID_Analysis` until multi-fire attribution is confirmed; final fire identifiers are not inferred.
- Final analysis and manuscript reporting remain conditional on co-author confirmation or completion of the review workbook.

## Historical, superseded: runnable provisional workflow

- The following bullets describe the earlier interim workflow. The active paper workflow uses the reviewed configuration and corrected annual effects described above.
- Scripts 02–07 are executable as a linear provisional workflow, with `run_all_agent_v1.R` as the entry point.
- **Provisional exception:** `effect_sizes_yearly.csv` is used as the response source so the established pair inventory is preserved while pairing provenance is confirmed. It remains subject to final rebuilding or variance revision.
- Burned-watershed attributes are joined from the daily and geospatial source snapshots; no new burned × reference combinations are generated.
- `post_fire_year` uses reported `Time_Since_Fire` when available and pair-specific follow-up year otherwise. The fallback must be reviewed for multi-fire studies.
- The predictor dictionary is generated from prespecified scientific groups plus completeness and variation checks; its inclusion flags remain provisional.
- The meta-analysis fits reported-variance models and family-adjusted sensitivity models. Family adjustment is not presented as an exact shared-control covariance matrix.
- LASSO performance uses leave-one-study-out outer validation, grouped inner folds, training-fold preprocessing, `lambda.1se`, and held-out predictions.
- Study-bootstrap stability defaults to 100 iterations for routine runs; set `N_BOOTSTRAP=1000` for final estimates.

## Manuscript figure styling

- Do not print figure titles, subtitles, or explanatory notes inside manuscript figures; place that information in manuscript captions.
- Use one consistent color for axis text, axis titles, and other figure labels unless the user explicitly requests varying label colors.
- Figure 2 retains the observation count (`n`) and distinct study count under each analyte.
- Prefer a complete, thin box around manuscript plot panels.
- Figure 3 displays bootstrap predictor-selection frequency only. Keep leave-one-study-out performance in supporting tables while the row-weighted RMSE interpretation is reviewed.
