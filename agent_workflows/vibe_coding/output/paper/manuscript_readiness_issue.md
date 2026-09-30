# Finalize manuscript analysis and figures

Bring the reviewed annual DOC and nitrate analysis to manuscript readiness by resolving the remaining statistical choices, validating the final models, and completing the five figure products. Status reflects project history and decisions through September 30, 2026, reconciled with the Methods draft supplied on that date. The Methods commit to multilevel inference, nested study-grouped LASSO, and sensitivity analyses; these are required manuscript deliverables, with implementation and wording to be reconciled before reporting. Existing figures are manuscript candidates; completed execution and publication styling do not establish final statistical inference.

This updates the workflow described in [issue 7](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/7) and coordinates the remaining work from [predictor issue 6](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/6) and [figure issue 10](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/10). All paths below are relative to `agent_workflows/vibe_coding/`.

## Completed foundation

- [x] Strict workbook review and import established 28 included and 8 excluded pairs. Control selection and pair approval are settled.
- [x] Reviewed physical-site restrictions are active, including Rhea U1, Crandall reference selections, and Mast & Clow Coal–Pinchot. Other Mast & Clow sites remain outside the paired analysis.
- [x] Observation-preserving interpolation is active and restored the Writer 2013 nitrate variance. The gap-fill data-loss bug is fixed.
- [x] The paper model table contains 104 annual responses: 35 DOC responses from 7 studies and 69 nitrate responses from 12 studies, all with usable provisional variances.
- [x] Six genuine singleton rows (four Murphy 2010 and two Writer 2012) are excluded from paper analyses and figures by user decision; they remain in upstream annual data and the exclusion audit.
- [x] User-defined eight-predictor sets, leave-one-study-out fits, and 1,000 study-bootstrap refits per analyte have been generated. Five candidate figures and source tables are organized under `output/paper/`.

The completed pairing and correction work is documented in [issue 4](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/4), [issue 8](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/8), and [issue 9](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/9); older unchecked tasks there may predate the current local results.

## Methods reconciliation priorities

- [x] **Resolve the effect-size mismatch first — decision recorded September 30, 2026.** The author selected `log(mean annual C_b / mean annual C_u)`. Calculate burned and reference arithmetic means over the same eligible matched positive-concentration dates within each calendar year, then take the log of their ratio. This supersedes the mean of daily log ratios. Aggregation code, model inputs, downstream fits, and figures have been regenerated; daily-log summaries remain diagnostic fields only. The provisional variance is `var(C_b/mean(C_b) - C_u/mean(C_u)) / n`, including same-date covariance. Final temporal/interpolation uncertainty remains open and does not reopen the selected response definition.
- [x] **Reconcile the retained study inventory — decision recorded September 30, 2026.** Report **12 publications, 14 unique wildfires, 28 approved burned–reference pairs, and 46 watersheds (28 burned and 18 reference)** for the paper analysis. Replace the draft's 52 watersheds (32 burned and 20 reference). Counts come from the reviewed physical-site inventory, deduplicating shared references within studies and Uzun seasonal aliases, rather than inferring watershed counts from pair counts. There are 15 study/fire records because High Park is represented in two retained publications.
- [x] **Record the analyte subsets and singleton exclusion.** Exclude six annual records with only one eligible matched date: four Murphy 2010 records and two Writer 2012 nitrate records. The paper retains **104 annual responses: 35 DOC from 7 publications and 16 pairs; 69 nitrate from 12 publications and 28 pairs**. Both publications and all approved pairs remain represented. The complete 110-row annual dataset remains upstream for provenance; it is distinct from the paper analysis subset. Evidence: `output/paper/paper_inventory_report.md`, `output/paper/tables/paper_inventory_summary.csv`, `output/paper/tables/paper_watershed_inventory.csv`, and `data/audit/paper_singleton_exclusions.csv`.
- [x] **Verify the remaining screening and biome claims.** The retained-analysis inventory does not verify the original **226 screened papers** or **three biome/climate categories**. Check those against original screening records and documented classification evidence before reporting them as verified; keep compiled-site totals distinct from retained-analysis totals.
- [ ] **Resolve table and SI references.** The Methods refer to Table 1 for pair counts and burned/reference area comparisons, but the supplied Table 1 is a predictor table. Provide and correctly cite a separate study/pair inventory table. Correct the predictor table headers (the current “Analyte” column contains predictor names). The screening flowchart is called Fig. S1, conflicting with SI Figure 1 correlations; assign unique numbering and update every cross-reference. Retain the five existing analysis figures and account separately for the screening flowchart promised by the Methods.

Acceptance: the Methods, screening inventory, response definition, and figure/table numbering agree before final model results are regenerated.

## 1. Record final analysis decisions before refitting

- [x] **Select the analysis time axis: time since fire.** Author confirmed time since fire as the time axis. The annual response remains `log(mean annual C_b / mean annual C_u)`.
- [ ] Verify that `post_fire_year` consistently represents time since the assigned fire, including multi-fire studies; audit and resolve follow-up-year fallbacks. Calendar-year concentration aggregation is distinct from the selected time-since-fire axis; the time-axis decision alone does not redefine annual aggregation windows. Document common-date coverage, observed/interpolated contributions, minimum annual coverage, zero/nonpositive-concentration handling, and partial-year treatment. Each analysis row represents one pair × analyte × annual period, indexed by time since fire.
- [ ] Finalize annual uncertainty after sampling-support review. Primary LASSO now uses equal-study → equal-pair → equal-year weights; equal-row and provisional precision/family comparisons are implemented. Observed-only and matched-interpolated sensitivity pipelines are implemented (32 DOC and 57 nitrate records). The sampling audit flags sparse/irregular coverage and does not establish automatic temporal-bootstrap validity. The current paired delta variance `var(C_b/mean(C_b) - C_u/mean(C_u)) / n` needs evaluation for temporal correlation and interpolated dates. It replaces the variance of the former daily-log mean. Document the chosen approach, assumptions, and observed-only sensitivity; do not count interpolated days as independent measurements without justification.
- [x] Exclude all six singleton annual records from descriptive paper plots, prediction, and variance-based inference. Preserve their upstream data and exclusion reasons.
- [ ] Finalize shared-reference and repeated-year dependence treatment. `reference_family_weight = 1 / n_pairs_sharing_reference` is a weighting convention, not an exact covariance model. Document the primary treatment and a defensible sensitivity while retaining approved pairs and their identifiers.
- [ ] Record whether the limited DOC study count supports a primary predictive claim or exploratory stability results. Keep claims conditional on study coverage and avoid causal interpretation of selected predictors.
- [ ] Preserve the requested primary sets and finalize their dictionary, units, transformations, missingness, and rationale. DOC: `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, `omws`. Nitrate replaces `omws` with `clayws`.
- [ ] Match Table 1 to the predictor dictionary and source metadata: verify percent versus area for watershed burned area, the denominator for high-severity percentage, runoff units and temporal coverage, baseflow-index definition, and the provenance of each fire and watershed field. Avoid attributing every field to StreamCat unless supported by its source.
- [ ] Explain inclusion of both highly correlated burn metrics and both hydrology metrics; assess alternatives as sensitivities without silently replacing the user-defined primary sets. Long-term aridity is unavailable and must not be represented by an undocumented proxy.

Acceptance: final choices and their rationale are recorded in `.codex/decisions.md` and the predictor dictionary before the manuscript run.

## 2. Correct and verify resampling and performance

- [x] Update `R/11_fit_issue6_revised_primary_agent_v1.R` so imputation, predictor screening, scaling, and data-derived weight rules are learned separately within every inner training fold. Implemented explicit inner-fold preprocessing for outer fits and bootstrap tuning, with a fixed penalty grid.
- [x] Correct bootstrap tuning groups: all copies of an original study now share a tuning fold. Draws with fewer than three distinct studies are logged as failures and excluded from frequency denominators.
- [ ] Verify that all records for a watershed pair and its repeated years stay together and that outer test studies never contribute to preprocessing, tuning, or fitting. Export fold assignments and add targeted leakage checks.
- [ ] Retain `lambda.1se` as primary and compare LASSO with intercept-only, time-only, and time-plus-fire benchmarks on identical held-out rows and folds.
- [ ] Define the performance target explicitly. Report current pooled pair-year RMSE alongside equal-study performance, clearly distinguishing root mean study MSE from mean study RMSE. Report per-study performance and document fitting weights separately from evaluation weights. Include the Methods-promised MAE and out-of-sample R², with explicit aggregation and R² denominator/baseline definitions; use held-out predictions rather than training fit or squared correlation.
- [ ] Regenerate selection frequency, standardized coefficient magnitude and direction, and sign stability using 1,000 bootstrap attempts per analyte. Report attempted, successful, and failed refits, failure reasons, and the denominator used for every frequency; unavailable fits must not become silent zero selections.

Acceptance: reproducible held-out predictions, benchmark comparisons, fold checks, coefficient tables, and transparent bootstrap diagnostics support every predictive claim. Figure 3 remains selection-frequency only.

## 3. Complete inference and sensitivity analyses

- [ ] Restore an updated agent-versioned multilevel inference step to `R/run_paper_workflow_agent_v1.R`. The supplied Methods explicitly include this analysis, but the active runner omits the archived meta-analysis scripts. Conditional REML pooled/time fits with CR2 inference and nine working-covariance scenarios are now implemented in script 19; finalize annual variance assumptions before treating these as manuscript inference.
- [ ] Verify the Methods-promised random intercepts for study, burned–reference pair, and shared reference against the implemented model formula and identifier nesting. Assess estimability/redundancy with the available studies and report convergence or singular components. Distinguish random-effect structure from sampling covariance and family weighting.
- [ ] Implement and document the claimed study-level adjustment of standard errors and confidence intervals: estimator, small-sample correction, degrees of freedom, and limitations with 7 DOC and 12 nitrate studies. Do not describe inference as adjusted merely because random intercepts are present.
- [ ] Export pooled effects, temporal coefficients, intervals, heterogeneity, study/pair/row counts, and influence diagnostics. Ensure any back-transformed interpretation corresponds to the selected annual estimand.
- [ ] Complete the sensitivities explicitly promised in the Methods/SI: gap-filling sensitivity including observed-only chemistry, reduced shared-reference weighting, elastic net with `alpha = 0.5`, and unweighted LASSO. Use the final predictor sets and corrected validation pipeline; archived results from earlier specifications do not establish completion for the final analysis.
- [ ] Complete the additional project-planned sensitivities for influential-study removal, matched DOC–nitrate subsets, and alternative burn/hydrology specifications. Include temporal-definition sensitivity if warranted by step 1; treat `lambda.min` as secondary if used. Document any author-approved changes to this scope.

- [ ] Summarize which conclusions persist across sensitivities and which depend on particular studies, weight rules, or correlated predictors. Do not infer predictive skill from selection frequency alone.

Acceptance: saved multilevel model results support the Methods claims, and a compact sensitivity table and manuscript interpretation distinguish stable findings from conditional or exploratory results.

## 4. Finish the five manuscript figures

- [ ] **Figure 1, maps:** verify coordinate provenance and remaining approximate positions, including Murphy and Writer. Reconcile 14 unique fires with 15 study/fire records across 12 studies. Keep one High Park marker representing three included burned watersheds, preserve Rhea and Writer detail, and keep Wagner excluded. Check nearby-point overlap, symbol counts, left-side legend, scale, north arrow, projection, and boundary attribution. Explain coordinate precision in the caption.
- [ ] **Figure 2, responses:** regenerate from the final table; retain unweighted annual pair-year points, zero reference, and observation/study counts (currently nitrate 69/12 and DOC 35/7). Explain the box/whisker definitions and singleton exclusion in the caption. Treat this as a descriptive distribution rather than a pooled meta-analytic estimate.
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

- [ ] Recheck approved/excluded pair membership, physical-site restrictions, unique analysis keys, observed-value preservation, singleton provenance, joins, fire/time attribution, and shared-control IDs. Reconcile any changed counts with the 104-row paper baseline (110 upstream rows minus six excluded singletons).
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
