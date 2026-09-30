# Finalize manuscript analysis and figures

Bring the reviewed annual DOC and nitrate analysis to manuscript readiness by resolving the remaining statistical choices, validating the final models, and completing the five figure products. Status reflects project history and decisions through September 30, 2026, reconciled with the Methods draft supplied on that date. The Methods commit to multilevel inference, nested study-grouped LASSO, and sensitivity analyses; these are required manuscript deliverables, with implementation and wording to be reconciled before reporting. Existing figures are manuscript candidates; completed execution and publication styling do not establish final statistical inference.

This updates the workflow described in [issue 7](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/7) and coordinates the remaining work from [predictor issue 6](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/6) and [figure issue 10](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/10). All paths below are relative to `agent_workflows/vibe_coding/`.

## Completed foundation

- [x] Strict workbook review and import established 28 included and 8 excluded pairs. Control selection and pair approval are settled.
- [x] Reviewed physical-site restrictions are active, including Rhea U1, Crandall reference selections, and Mast & Clow Coal–Pinchot. Other Mast & Clow sites remain outside the paired analysis.
- [x] Observation-preserving interpolation is active and restored the Writer 2013 nitrate variance. The gap-fill data-loss bug is fixed.
- [x] The current model table contains 110 finite annual responses: 37 DOC responses from 7 studies and 73 nitrate responses from 12 studies. There are 35 DOC and 69 nitrate rows with usable variances.
- [x] The six remaining missing variances are genuine singletons: four Murphy 2010 rows and two Writer 2012 rows, each with one matched observed date.
- [x] User-defined eight-predictor sets, leave-one-study-out fits, and 1,000 study-bootstrap refits per analyte have been generated. Five candidate figures and source tables are organized under `output/paper/`.

The completed pairing and correction work is documented in [issue 4](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/4), [issue 8](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/8), and [issue 9](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/9); older unchecked tasks there may predate the current local results.

## Methods reconciliation priorities

- [x] **Annual effect-size definition selected and implemented.** User selected `log(mean annual C_b / mean annual C_u)`. Both means use the same eligible matched positive-concentration dates in each calendar year. The response, provisional paired delta variance, model table, and downstream workflow now use this definition; daily-log summaries remain diagnostics. Final uncertainty and coverage choices remain below.
- [ ] **Reconcile the study inventory.** Verify the Methods counts of 226 screened papers, 12 retained publications, 52 distinct watersheds (32 burned and 20 reference), three reported biome/climate categories, and 14 unique fires against screening records and a deduplicated physical-site inventory. Distinguish compiled sites from the 28 approved analysis pairs and the analyte-specific model subsets; pair counts cannot establish unique watershed counts.
- [ ] **Complete study-identification provenance.** Preserve the September 2023 search date, recover exact database-specific queries and Boolean grouping, and reconcile screening-stage counts and exclusion reasons. Verify that the downstream-site and pre-MTBS examples accurately describe the reviewed decisions. Any update to the literature search is a separate scope decision, not assumed here.
- [ ] **Resolve table and SI references.** The Methods refer to Table 1 for pair counts and burned/reference area comparisons, but the supplied Table 1 is a predictor table. Provide and correctly cite a separate study/pair inventory table. Correct the predictor table headers (the current “Analyte” column contains predictor names). The screening flowchart is called Fig. S1, conflicting with SI Figure 1 correlations; assign unique numbering and update every cross-reference. Retain the five existing analysis figures and account separately for the screening flowchart promised by the Methods.

Acceptance: the Methods, screening inventory, response definition, and figure/table numbering agree before final model results are regenerated.

## 1. Record final analysis decisions before refitting

- [ ] Specify the annual estimand and time axis: calendar-year aggregation, discrete time-since-fire years, or a primary definition with the other as sensitivity. Audit reported fire timing and follow-up-year fallbacks, especially in multi-fire studies. Implement the estimand selected in the reconciliation above. Specify common-date coverage, observed/interpolated contributions, minimum annual coverage, handling of zero/nonpositive concentrations, and whether partial years are retained. State explicitly that each row is one pair × analyte × year, since pairs contribute repeated annual effects.
- [ ] Finalize annual uncertainty and weighting. The current paired delta variance `var(C_b/mean(C_b) - C_u/mean(C_u)) / n` needs evaluation for temporal correlation and interpolated dates. It replaces the variance of the former daily-log mean. Document the chosen approach, assumptions, and observed-only sensitivity; do not count interpolated days as independent measurements without justification.
- [ ] Specify treatment of the six singleton rows separately for descriptive plots, prediction, and variance-based inference. Document missing-precision fallback and weight capping where used; do not manufacture singleton variances.
- [ ] Finalize shared-reference and repeated-year dependence treatment. `reference_family_weight = 1 / n_pairs_sharing_reference` is a weighting convention, not an exact covariance model. Document the primary treatment and a defensible sensitivity while retaining approved pairs and their identifiers.
- [ ] Record whether the limited DOC study count supports a primary predictive claim or exploratory stability results. Keep claims conditional on study coverage and avoid causal interpretation of selected predictors.
- [ ] Preserve the requested primary sets and finalize their dictionary, units, transformations, missingness, and rationale. DOC: `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, `omws`. Nitrate replaces `omws` with `clayws`.
- [ ] Match Table 1 to the predictor dictionary and source metadata: verify percent versus area for watershed burned area, the denominator for high-severity percentage, runoff units and temporal coverage, baseflow-index definition, and the provenance of each fire and watershed field. Avoid attributing every field to StreamCat unless supported by its source.
- [ ] Explain inclusion of both highly correlated burn metrics and both hydrology metrics; assess alternatives as sensitivities without silently replacing the user-defined primary sets. Long-term aridity is unavailable and must not be represented by an undocumented proxy.

Acceptance: final choices and their rationale are recorded in `.codex/decisions.md` and the predictor dictionary before the manuscript run.

## 2. Correct and verify resampling and performance

- [ ] Update `R/11_fit_issue6_revised_primary_agent_v1.R` so imputation, predictor screening, scaling, and data-derived weight rules are learned separately within every inner training fold. Current outer-fold preprocessing occurs before `cv.glmnet`, leaving inner-fold preprocessing unresolved. Apply the same discipline to bootstrap tuning.
- [ ] Audit bootstrap tuning groups. Current code assigns folds using the resampled `bootstrap_cluster`; repeated copies of the same original study can therefore enter different folds. Keep copies of an original study together and define behavior when a resample contains too few unique studies for tuning.
- [ ] Verify that all records for a watershed pair and its repeated years stay together and that outer test studies never contribute to preprocessing, tuning, or fitting. Export fold assignments and add targeted leakage checks.
- [ ] Retain `lambda.1se` as primary and compare LASSO with intercept-only, time-only, and time-plus-fire benchmarks on identical held-out rows and folds.
- [ ] Define the performance target explicitly. Report current pooled pair-year RMSE alongside equal-study performance, clearly distinguishing root mean study MSE from mean study RMSE. Report per-study performance and document fitting weights separately from evaluation weights. Include the Methods-promised MAE and out-of-sample R², with explicit aggregation and R² denominator/baseline definitions; use held-out predictions rather than training fit or squared correlation.
- [ ] Regenerate selection frequency, standardized coefficient magnitude and direction, and sign stability using 1,000 bootstrap attempts per analyte. Report attempted, successful, and failed refits, failure reasons, and the denominator used for every frequency; unavailable fits must not become silent zero selections.

Acceptance: reproducible held-out predictions, benchmark comparisons, fold checks, coefficient tables, and transparent bootstrap diagnostics support every predictive claim. Figure 3 remains selection-frequency only.

## 3. Complete inference and sensitivity analyses

- [ ] Restore an updated agent-versioned multilevel inference step to `R/run_paper_workflow_agent_v1.R`. The supplied Methods explicitly include this analysis, but the active runner omits the archived meta-analysis scripts. Fit analyte-specific pooled-effect and time-since-fire models using REML in `metafor` after the estimand and variance decisions are settled.
- [ ] Verify the Methods-promised random intercepts for study, burned–reference pair, and shared reference against the implemented model formula and identifier nesting. Assess estimability/redundancy with the available studies and report convergence or singular components. Distinguish random-effect structure from sampling covariance and family weighting.
- [ ] Implement and document the claimed study-level adjustment of standard errors and confidence intervals: estimator, small-sample correction, degrees of freedom, and limitations with 7 DOC and 12 nitrate studies. Do not describe inference as adjusted merely because random intercepts are present.
- [ ] Export pooled effects, temporal coefficients, intervals, heterogeneity, study/pair/row counts, and influence diagnostics. Ensure any back-transformed interpretation corresponds to the selected annual estimand.
- [ ] Complete the sensitivities explicitly promised in the Methods/SI: gap-filling sensitivity including observed-only chemistry, reduced shared-reference weighting, elastic net with `alpha = 0.5`, and unweighted LASSO. Use the final predictor sets and corrected validation pipeline; archived results from earlier specifications do not establish completion for the final analysis.
- [ ] Complete the additional project-planned sensitivities for influential-study removal, matched DOC–nitrate subsets, and alternative burn/hydrology specifications. Include temporal-definition sensitivity if warranted by step 1; treat `lambda.min` as secondary if used. Document any author-approved changes to this scope.

- [ ] Summarize which conclusions persist across sensitivities and which depend on particular studies, weight rules, or correlated predictors. Do not infer predictive skill from selection frequency alone.

Acceptance: saved multilevel model results support the Methods claims, and a compact sensitivity table and manuscript interpretation distinguish stable findings from conditional or exploratory results.

## 4. Finish the five manuscript figures

- [ ] **Figure 1, maps:** verify coordinate provenance and remaining approximate positions, including Murphy and Writer. Reconcile 14 unique fires with 15 study/fire records across 12 studies. Keep one High Park marker representing three included burned watersheds, preserve Rhea and Writer detail, and keep Wagner excluded. Check nearby-point overlap, symbol counts, left-side legend, scale, north arrow, projection, and boundary attribution. Explain coordinate precision in the caption.
- [ ] **Figure 2, responses:** regenerate from the final table; retain unweighted annual pair-year points, zero reference, and observation/study counts (currently nitrate 73/12 and DOC 37/7). Explain the box/whisker definitions and singleton inclusion in the caption. Treat this as a descriptive distribution rather than a pooled meta-analytic estimate.
- [ ] **Figure 3, selection:** regenerate after resampling corrections; show bootstrap selection frequency only, order predictors within analyte by frequency, and retain nitrate before DOC with matching blue/ochre colors. Describe the candidate sets, `lambda.1se`, successful-refit denominator, and limits of selection-frequency interpretation. Keep held-out performance in supporting tables.
- [ ] **SI Figure 1, correlations:** verify all 24 fields, labels, units, missing-data handling, and sample counts. The current matrix repeats static attributes across pair-years; explicitly document that weighting and assess whether a pair-level static-predictor diagnostic is needed before using it to justify redundancy decisions.
- [ ] **SI Figure 2, distributions:** show only the nine-variable union of the final sets; verify one pair-level value for static attributes and pair-year values for post-fire year, with readable units, points, medians, and distribution summaries.
- [ ] Apply established styling across figures: no in-figure titles, subtitles, or explanatory notes; consistent dark label text; thin complete panel borders where applicable. Preserve Figure 2 counts and Figure 3 scope.
- [ ] Reconcile the screening flowchart and its counts with the final SI numbering; confirm whether an existing artifact can be retained or must be produced.
- [ ] Write standalone captions identifying analysis units, sample sizes, methods, symbols, and relevant limitations. Select target-journal dimensions and export formats; inspect all five figures at final manuscript size for legibility, clipping, overlap, and color accessibility.

Acceptance: five visually checked analysis figures, captions, and matching source tables under `output/paper/`, plus a verified screening-flowchart artifact and consistent manuscript/SI numbering, with no hand-edited numerical results.

## 5. Rebuild and freeze the manuscript package

- [ ] Verify the from-source path after the archive reorganization, including the strict workbook importer and all referenced files. The cleanup run was verified from the prepared table; its optional from-source mode was not rerun then.
- [ ] After the method/code updates, run from a clean R session at repository root:

```sh
PAPER_FROM_SOURCE=1 N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R
```

- [ ] Recheck approved/excluded pair membership, physical-site restrictions, unique analysis keys, observed-value preservation, singleton provenance, joins, fire/time attribution, and shared-control IDs. Reconcile any changed counts with the current 110-row baseline.
- [ ] Capture seeds, package/session versions, source/configuration provenance, code revision, execution logs, and failure summaries. Ensure the runner includes all analyses retained for manuscript claims, not only the five figures.
- [ ] Complete the Methods/SI text: replace R version/year placeholders using the final session record, remove editorial comment markers, verify cited data/code availability, and document exact model formulas, preprocessing, weighting, variance, sensitivity settings, and output locations. Replace unsupported claims that interpolation supplies observed daily hydrologic variability with a precise description of what interpolation contributes and what sensitivity analysis tests.
- [ ] Assemble the study/pair inventory, final analysis dictionary, model summaries, held-out performance, stability/sensitivity tables, captions, and Methods/Results text. Update `output/paper/README.md` to link the final products and append the outcome to `.codex/history.md`.
- [ ] Review related issue status against saved evidence; preserve historical outputs under `archive/preliminary_2026_09_29/` and keep human scripts unchanged.

Acceptance: every manuscript number and figure can be regenerated from documented inputs; all method decisions are explicit; no unexplained fitting failures or unresolved figure-provenance gaps remain.

## Project evidence

- User-supplied Methods draft, September 30, 2026: Study Identification; Data Compilation, harmonization and effect-size calculation; Statistical data analysis; and predictor Table 1. Its numerical and methodological claims are targets for verification, not evidence that every described analysis has been completed.

- `.codex/history.md` and `.codex/decisions.md`, especially the September 29–30 entries.
- `README.md` and `output/paper/README.md` for active paths and figure specifications.
- `config/issue6_revised_primary_predictors.csv` and `config/issue6_revised_primary_dictionary.csv` for the primary sets.
- `R/run_paper_workflow_agent_v1.R` and `R/11_fit_issue6_revised_primary_agent_v1.R` for the current execution and resampling behavior.
- `output/paper/tables/figure_1_map_locations.csv` and `output/paper/tables/figure_1_study_fire_records.csv` for map reconciliation.
