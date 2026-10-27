# Finalize manuscript analysis and figures

Use the [active workflow index](../../README.md) for current methods, inputs, execution modes, and the summary of remaining work. This document retains the detailed acceptance checklist.

Bring the reviewed annual DOC and nitrate analysis to manuscript readiness by resolving the remaining statistical choices, validating the final models, and completing the seven main/SI figure products. Status updated October 27, 2026, incorporating the author-selected balanced pooled synthesis, September 30 decisions, completed workflow runs, and supplied Methods draft. The supplied Methods commit to multilevel inference, nested study-grouped LASSO, and sensitivity analyses. The October 22 decision makes balanced pooled CR2 synthesis primary. On October 27 the author moved conditional REML to exploratory work; it is excluded from all paper runner modes and is not a manuscript requirement. Existing figures are manuscript candidates; completed execution and publication styling do not establish final statistical inference.

This updates the workflow described in [issue 7](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/7) and coordinates the remaining work from [predictor issue 6](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/6) and [figure issue 10](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/10). All paths below are relative to `agent_workflows/vibe_coding/`.

## Completed foundation

- [x] Strict workbook review and import established 28 included and 8 excluded pairs. Control selection and pair approval are settled.
- [x] Reviewed physical-site restrictions are active, including Rhea U1, Crandall reference selections, and Mast & Clow Coal–Pinchot. Other Mast & Clow sites remain outside the paired analysis.
- [x] Observation-preserving interpolation is active and restored the Writer 2013 nitrate variance. The gap-fill data-loss bug is fixed.
- [x] The paper model table contains 103 annual responses: 35 DOC responses from 7 studies and 68 nitrate responses from 12 studies, all with usable provisional variances.
- [x] Six genuine singleton rows (four Murphy 2010 and two Writer 2012) are excluded from paper analyses and figures by user decision; they remain in upstream annual data and the exclusion audit.
- [x] User-defined eight-predictor sets, leave-one-study-out fits, and 1,000 study-bootstrap attempts per analyte have been generated for the primary analysis and each of four sensitivities: 994 successful DOC refits and 1,000 successful nitrate refits per scenario. Five candidate figures and source tables are organized under `output/paper/`.

## Implemented decisions and exclusions

**October 22 author-directed exclusion:** additionally remove the Crandall et al. 2021, 2018 Site_2–Control_2 nitrate annual response (lnRR −4.506808658089654) from every active manuscript analysis, sensitivity and figure. This is a post hoc outlier exclusion, not a confirmed data error. The exact key/rationale is in `config/paper_record_exclusions.csv`; the full excluded row is in `data/audit/paper_record_exclusions.csv`. Source chemistry and all 110 upstream annual records remain unchanged; prior full-inventory results are archived. See the [before/after comparison and rerun evidence](nitrate_exclusion_report.md). The pair's DOC response remains, so the overall inventory is still 28 pairs/46 watersheds, while nitrate covers 27 pairs/44 watersheds (27 burned, 17 reference).

The singleton exclusion removes **six pair × analyte × year records, not six entire sites or studies**. Each excluded record has only one eligible matched concentration date.

| Publication | Calendar year | Burned pair IDs | Analytes excluded | Annual records removed |
|---|---:|---|---|---:|
| Murphy et al. 2018 | 2010 | Site_1 and Site_2 | DOC and nitrate | 4 |
| Writer et al. 2014 | 2012 | Site_1 and Site_2 | Nitrate only | 2 |

- [x] Enforce `lnRR_n >= 2` in the paper model table. Retain all 110 upstream annual records and save the six excluded rows with reasons in `data/audit/paper_singleton_exclusions.csv`. Both publications and all 28 approved pairs remain represented; paper analyses and figures use 103 records after also excluding the author-selected nitrate outlier.
- [x] Use the log ratio of annual arithmetic mean concentrations as the response and **time since fire** as the analysis axis. Current calendar-year concentration aggregation is retained; fire attribution and fallback checks remain below.
- [x] Use equal total study weight for primary LASSO, divided equally among retained pairs and then annual records within each pair. Recalculate weights inside training folds and preserve study-bootstrap draw multiplicity. Primary predictive weights do not use interpolated-day precision.
- [x] Run equal-row and provisional precision/family weighting sensitivities. Run observed-only and matched-interpolated sensitivities on the same **88 records: 32 DOC and 56 nitrate**. The additional 15 sensitivity exclusions do not change the primary inventory.
- [x] Complete the sampling-support audit. Only 18 records, all Murphy, meet the exploratory screen of at least 20 matched observed dates over at least 90 days; even these have 42–57-day maximum gaps. This is a diagnostic screen, not a validated eligibility threshold for uncertainty estimation. No automatic temporal block-bootstrap variances were assigned.
- [x] Retain established study-identification provenance from previous versions, as confirmed by the author; rebuilding that provenance is not an issue task.

Evidence: `output/paper/uncertainty_weighting_report.md`, `data/audit/paper_sampling_support.csv`, `data/audit/observed_only_exclusions.csv`, and `output/paper/tables/fitting_weights.csv`.

The completed pairing and correction work is documented in [issue 4](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/4), [issue 8](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/8), and [issue 9](https://github.com/river-corridors-sfa/rc_sfa-rc-3-wenas-meta/issues/9); older unchecked tasks there may predate the current local results.

## Methods reconciliation priorities

- [x] **Resolve the effect-size mismatch first — decision recorded September 30, 2026.** The author selected `log(mean annual C_b / mean annual C_u)`. Calculate burned and reference arithmetic means over the same eligible matched positive-concentration dates within each calendar year, then take the log of their ratio. This supersedes the mean of daily log ratios. Aggregation code, model inputs, downstream fits, and figures have been regenerated; daily-log summaries remain diagnostic fields only. The provisional variance is `var(C_b/mean(C_b) - C_u/mean(C_u)) / n`, including same-date covariance. Final temporal/interpolation uncertainty remains open and does not reopen the selected response definition.
- [x] **Reconcile the retained study inventory — decision recorded September 30, 2026.** Report **12 publications, 14 unique wildfires, 28 approved burned–reference pairs, and 46 watersheds (28 burned and 18 reference)** for the paper analysis. Replace the draft's 52 watersheds (32 burned and 20 reference). Counts come from the reviewed physical-site inventory, deduplicating shared references within studies and Uzun seasonal aliases, rather than inferring watershed counts from pair counts. There are 15 study/fire records because High Park is represented in two retained publications.
- [x] **Record the analyte subsets and exclusions.** Exclude six annual records with only one eligible matched date: four Murphy 2010 records and two Writer 2012 nitrate records. The paper retains **103 annual responses: 35 DOC from 7 publications and 16 pairs; 68 nitrate from 12 publications and 27 pairs**. Both publications and all approved pairs remain represented. The complete 110-row annual dataset remains upstream for provenance; it is distinct from the paper analysis subset. Evidence: `output/paper/paper_inventory_report.md`, `output/paper/tables/paper_inventory_summary.csv`, `output/paper/tables/paper_watershed_inventory.csv`, and `data/audit/paper_singleton_exclusions.csv`.
- [x] **Verify the remaining screening and biome claims.** The retained-analysis inventory does not verify the original **226 screened papers** or **three biome/climate categories**. Check those against original screening records and documented classification evidence before reporting them as verified; keep compiled-site totals distinct from retained-analysis totals.
- [ ] **Resolve table and SI references.** The Methods refer to Table 1 for pair counts and burned/reference area comparisons, but the supplied Table 1 is a predictor table. Provide and correctly cite a separate study/pair inventory table. Correct the predictor table headers (the current “Analyte” column contains predictor names). The screening flowchart is called Fig. S1, conflicting with SI Figure 1 correlations; assign unique numbering and update every cross-reference. Retain the five existing analysis figures and account separately for the screening flowchart promised by the Methods.

Acceptance: the Methods, screening inventory, response definition, and figure/table numbering agree before final model results are regenerated.

## 1. Record final analysis decisions before refitting

October 21 uncertainty review: `exploratory/annual_uncertainty_review.md` and the exploratory annual-uncertainty script compare balanced CR2 synthesis without precision weights against conditional REML. Connected study/fire clustering links Rhea and Writer through High Park (six DOC and eleven nitrate components). Balanced versus REML time slopes reverse signs; balanced slope degrees of freedom are about 1.3. On October 22 the author selected balanced pooled summaries with connected study/fire-component CR2 uncertainty as primary; publication-cluster intervals remain sensitivities. Conditional REML was subsequently moved to exploratory work on October 27. Annual sampling variances remain unvalidated, and prediction folds/bootstrap groups remain unchanged.

- [x] **Select primary synthesis — October 22, 2026.** Use equal study → pair → available-year pooled summaries, without annual precision weights, and connected study/fire-component CR2/Satterthwaite intervals (six DOC and eleven nitrate components). Run `R/analysis/02_fit_balanced_synthesis_agent_v1.R` in `primary`/`all`; outputs are in `output/paper/balanced_synthesis/`. Keep temporal associations and REML exploratory, annual variance validation open for exploratory precision-based inference, and publication-based LASSO groups unchanged.
- [x] **Select the analysis time axis: time since fire.** Author confirmed time since fire as the time axis. The annual response remains `log(mean annual C_b / mean annual C_u)`.
- [ ] Verify that `post_fire_year` consistently represents time since the assigned fire, including multi-fire studies; audit and resolve follow-up-year fallbacks. Calendar-year concentration aggregation is distinct from the selected time-since-fire axis; the time-axis decision alone does not redefine annual aggregation windows. Document common-date coverage, observed/interpolated contributions, minimum annual coverage, zero/nonpositive-concentration handling, and partial-year treatment. Each analysis row represents one pair × analyte × annual period, indexed by time since fire.
- [ ] Finalize annual uncertainty after sampling-support review. Primary LASSO now uses equal-study → equal-pair → equal-year weights; equal-row and provisional precision/family comparisons are implemented. Observed-only and matched-interpolated sensitivity pipelines are implemented (32 DOC and 56 nitrate records). The sampling audit flags sparse/irregular coverage and does not establish automatic temporal-bootstrap validity. The current paired delta variance `var(C_b/mean(C_b) - C_u/mean(C_u)) / n` needs evaluation for temporal correlation and interpolated dates. It replaces the variance of the former daily-log mean. Document the chosen approach, assumptions, and observed-only sensitivity; do not count interpolated days as independent measurements without justification.
- [x] Exclude all six singleton annual records from descriptive paper plots, prediction, and variance-based inference. Preserve their upstream data and exclusion reasons.
- [ ] Verify the selected dependence treatment before freeze: keep shared references and repeated years within connected study/fire components; audit other cross-publication links and component independence. Primary clustering is selected; link validation and a controlled family-balancing sensitivity remain outstanding. `reference_family_weight = 1 / n_pairs_sharing_reference` is a weighting convention, not an exact covariance model. Document the primary treatment and a defensible sensitivity while retaining approved pairs and their identifiers.
- [ ] Record whether the limited DOC study count supports a primary predictive claim or exploratory stability results. Keep claims conditional on study coverage and avoid causal interpretation of selected predictors.
- [ ] Preserve the requested primary sets and finalize their dictionary, units, transformations, missingness, and rationale. DOC: `post_fire_year`, `burn_percent_fire_year`, `burn_sev_high`, `Area_watershed_km`, `runoffws`, `bfiws`, `forest_cover`, `omws`. Nitrate replaces `omws` with `clayws`.
- [ ] Match Table 1 to the predictor dictionary and source metadata: verify percent versus area for watershed burned area, the denominator for high-severity percentage, runoff units and temporal coverage, baseflow-index definition, and the provenance of each fire and watershed field. Avoid attributing every field to StreamCat unless supported by its source.
- [ ] Explain inclusion of both highly correlated burn metrics and both hydrology metrics; assess alternatives as sensitivities without silently replacing the user-defined primary sets. Long-term aridity is unavailable and must not be represented by an undocumented proxy.

Acceptance: final choices and their rationale are recorded in the active workflow index and the predictor dictionary (preserve decision rationale in `.codex/decisions_history.md`) before the manuscript run.

## 2. Correct and verify resampling and performance

- [x] Update `R/analysis/01_fit_issue6_revised_primary_agent_v1.R` so imputation, predictor screening, scaling, and data-derived weight rules are learned separately within every inner training fold. Implemented explicit inner-fold preprocessing for outer fits and bootstrap tuning, with a fixed penalty grid.
- [x] Correct bootstrap tuning groups: all copies of an original study now share a tuning fold. Draws with fewer than three distinct studies are logged as failures and excluded from frequency denominators.
- [x] Group tuning by original study, keeping pair histories and repeated bootstrap copies together. Targeted checks passed for equal study/pair/draw weights, training-only preprocessing, observed-only eligibility, and complete unique held-out predictions.
- [ ] Export reusable outer/inner fold-assignment tables for manuscript reproducibility.
- [x] Retain the one-standard-error penalty rule and compare LASSO with intercept-only, time-only, and time-plus-fire benchmarks on identical held-out rows and study folds. Tuning now uses a fixed 80-value penalty grid from 100 to 0.0001 and study-level validation loss; this implementation replaces the former `cv.glmnet` preprocessing path.
- [x] Export pooled pair-year RMSE, MAE, and R²; per-study errors; and equal-study RMSE/MAE. Equal-study RMSE is the square root of mean study MSE; mean study RMSE is a separate column. Pooled R² uses the pooled observed-mean denominator. Fitting weights and evaluation summaries are saved separately in `fitting_weights.csv`, `performance.csv`, `performance_by_study.csv`, and `performance_equal_study.csv` under `output/paper/tables/`.
- [x] Regenerate selection frequency and standardized coefficient magnitude/direction from 1,000 bootstrap attempts per analyte and scenario. Each scenario completed 994 DOC and 1,000 nitrate refits; six DOC draws had fewer than three distinct studies for tuning. Failure logs and `bootstrap_summary.csv` record these exclusions. Frequencies use successful refits only; failed fits are not zero selections. Positive/negative frequencies and bootstrap coefficients are exported.
- [ ] Finalize manuscript interpretation of coefficient sign stability and predictive performance; do not equate selection frequency with predictive skill.

Acceptance: reproducible held-out predictions, benchmark comparisons, fold checks, coefficient tables, and transparent bootstrap diagnostics support every predictive claim. Figure 4 remains selection-frequency only.

## 3. Complete inference and sensitivity analyses

- [x] **October 27 scope change:** move conditional REML out of the manuscript workflow. The standalone script is `exploratory/R/02_fit_working_covariance_inference_agent_v1.R`; all 36 previously completed fits are preserved under `exploratory/output/working_covariance/`. No paper runner mode executes it.
- [x] Implement study/pair random intercepts with shared-reference dependence in the sampling covariance. The positive-semidefinite covariance mixes pair and shared-reference temporal kernels, evaluating reference rho and annual temporal phi in `{0, 0.5, 0.8}`. These are assumed sensitivity parameters, not estimates of daily temporal correlation.
- [x] Implement study-clustered CR2 uncertainty with Satterthwaite degrees of freedom through `clubSandwich`. Export coefficients, intervals, degrees of freedom, study/row counts, heterogeneity components, model objects, and failure logs. Exploratory results are preserved in `exploratory/output/working_covariance/working_covariance_inference.csv` and `exploratory/output/working_covariance/working_covariance_models.rds`.
- [x] Classify REML as exploratory rather than a manuscript sensitivity. Remove manuscript promises of REML inference and a separate shared-reference random intercept. Annual-variance validation and further REML diagnostics belong to exploratory development, not manuscript acceptance criteria.
- [x] Complete observed-only, matched-interpolated, equal-row (unweighted), and provisional precision/family LASSO sensitivities using the current predictor sets and corrected tuning pipeline. Scenario outputs are under `output/paper/sensitivity/`; the combined performance table is `output/paper/tables/weighting_interpolation_performance.csv`.
- [ ] Complete elastic net with `alpha = 0.5` and a controlled shared-reference weight comparison holding other weighting choices fixed. The completed equal-study versus precision/family comparison changes more than the reference-family multiplier and does not isolate that effect.
- [ ] Complete the additional project-planned sensitivities for influential-study removal and matched DOC–nitrate subsets. Alternative burn/hydrology specifications are implemented in the new paired predictor-omission analysis (October 27; 12 specifications, nested LOSO and 1,000 paired study-bootstrap attempts per analyte). Include temporal-definition sensitivity if warranted by step 1; treat `lambda.min` as secondary if used. Document any author-approved changes to this scope.

- [ ] Summarize which conclusions persist across sensitivities and which depend on particular studies, weight rules, or correlated predictors. Do not infer predictive skill from selection frequency alone.

Acceptance: balanced synthesis and completed manuscript sensitivities support the Methods claims; a compact sensitivity table distinguishes stable findings from conditional or exploratory results. Exploratory REML completion or validation is not required.

## 4. Finish the seven manuscript/SI figures

The October 22 selection preserved five figures; October 27 additions produced seven figures: four main and three SI. Figure 3 displays time-baseline group-model performance, and the former selection graphic is Figure 4. See the [figure-by-figure implications](README.md#effect-of-the-selected-balanced-synthesis-on-figures): maps retain geographic counts, Figure 2 now shows a balance-weighted raincloud with annual points and primary pooled CR2 intervals, Figure 4 retains publication-bootstrap selection frequencies, and SI Figures 1–2 remain descriptive predictor diagnostics. Only Figure 2 displays pooled CR2 intervals; the other figures retain their predictive, descriptive or selection roles.

- [ ] **Figure 1, maps:** verify coordinate provenance and remaining approximate positions, including Murphy and Writer. Reconcile 14 unique fires with 15 study/fire records across 12 studies. Keep one High Park marker representing three included burned watersheds, preserve Rhea and Writer detail, and keep Wagner excluded. Check nearby-point overlap, symbol counts, left-side legend, scale, north arrow, projection, and boundary attribution. Explain coordinate precision in the caption.
- [x] Regenerate all five analysis figures from the current paper inputs and updated models; visually check updated Figures 2 and 3. Figure 4 remains selection-frequency only, with performance in supporting tables.
- [x] **Figure 2, raincloud:** replace and archive the boxplot; show balance-weighted half-violins, all unweighted annual points, pooled diamonds and 95% study/fire-component CR2 intervals. Preserve the zero line, full range and record/study/component counts. Export PNG/PDF, source weights/polygons and a standalone caption; visually verify the rendering.
- [ ] **Figure 2, final publication review:** review caption wording and target-journal size with the other final figure exports. Density spread is response variation; bars are pooled uncertainty. Six singleton and one author-selected outlier exclusions remain documented.
- [ ] **Figure 4, selection:** finalize the caption after the completed resampling corrections; show bootstrap selection frequency only, order predictors within analyte by frequency, and retain nitrate before DOC with matching blue/ochre colors. Describe the candidate sets, `lambda.1se`, successful-refit denominator, and limits of selection-frequency interpretation. Keep held-out performance in supporting tables.
- [ ] **SI Figure 1, correlations:** the caption now uses the input-derived count (103 records); verify all 24 fields, labels, units, missing-data handling, and sample counts. The current matrix repeats static attributes across pair-years; explicitly document that weighting and assess whether a pair-level static-predictor diagnostic is needed before using it to justify redundancy decisions.
- [ ] **SI Figure 2, distributions:** show only the nine-variable union of the final sets; verify one pair-level value for static attributes and pair-year values for post-fire year, with readable units, points, medians, and distribution summaries.
- [ ] Apply established styling across figures: no in-figure titles, subtitles, or explanatory notes; consistent dark label text; thin complete panel borders where applicable. Preserve Figure 2 counts and Figure 4 scope.
- [ ] Reconcile the screening flowchart and its counts with the final SI numbering; confirm whether an existing artifact can be retained or must be produced.
- [ ] Write standalone captions identifying analysis units, sample sizes, methods, symbols, and relevant limitations. Select target-journal dimensions and export formats; inspect all five figures at final manuscript size for legibility, clipping, overlap, and color accessibility.

Acceptance: seven visually checked analysis figures, captions, and matching source tables under `output/paper/`, plus a verified screening-flowchart artifact and consistent manuscript/SI numbering, with no hand-edited numerical results.

## 5. Rebuild and freeze the manuscript package

- [x] Restore the strict workbook importer and its input files to active paths; strict import and annual/model-table rebuilding succeeded.
- [x] Complete the active paper runner from prepared data, including sampling audit, primary/sensitivity models, and five figures. Conditional multilevel inference completed in the earlier run is now preserved separately as exploratory work. Record package/session information, run logs, and bootstrap failures.
- [ ] Verify a clean end-to-end from-source run including interpolation; recent full paper runs used the prepared corrected inputs.
- [ ] After the method/code updates, run from a clean R session at repository root:

```sh
PAPER_FROM_SOURCE=1 N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R
```

- [ ] Recheck approved/excluded pair membership, physical-site restrictions, unique analysis keys, observed-value preservation, singleton provenance, joins, fire/time attribution, and shared-control IDs. Reconcile any changed counts with the 103-row paper baseline (110 upstream rows minus six singletons and one author-selected nitrate outlier).
- [ ] Capture seeds, package/session versions, source/configuration provenance, code revision, execution logs, and failure summaries. Ensure the runner includes all analyses retained for manuscript claims, not only the five figures.
- [ ] Complete the Methods/SI text: replace R version/year placeholders using the final session record, remove editorial comment markers, verify cited data/code availability, and document exact model formulas, preprocessing, weighting, variance, sensitivity settings, and output locations. Replace unsupported claims that interpolation supplies observed daily hydrologic variability with a precise description of what interpolation contributes and what sensitivity analysis tests.
- [ ] Assemble the study/pair inventory, final analysis dictionary, model summaries, held-out performance, stability/sensitivity tables, captions, and Methods/Results text. Update `output/paper/README.md` to link the final products and append the outcome to `.codex/history.md`.
- [ ] Review related issue status against saved evidence; preserve historical outputs under `archive/preliminary_2026_09_29/` and keep human scripts unchanged.

Acceptance: every manuscript number and figure can be regenerated from documented inputs; all method decisions are explicit; no unexplained fitting failures or unresolved figure-provenance gaps remain.

## Project evidence

- User-supplied Methods draft, September 30, 2026: Study Identification; Data Compilation, harmonization and effect-size calculation; Statistical data analysis; and predictor Table 1. Its numerical and methodological claims are targets for verification, not evidence that every described analysis has been completed.

- `README.md` for current workflow status; `.codex/history.md` and `.codex/decisions_history.md` for dated evidence, especially the September 29–30 entries.
- `README.md` and `output/paper/README.md` for active paths and figure specifications.
- `config/issue6_revised_primary_predictors.csv` and `config/issue6_revised_primary_dictionary.csv` for the primary sets.
- `R/run_paper_workflow_agent_v1.R` and `R/analysis/01_fit_issue6_revised_primary_agent_v1.R` for the current execution and resampling behavior.
- `output/paper/tables/figure_1_map_locations.csv` and `output/paper/tables/figure_1_study_fire_records.csv` for map reconciliation.

- `output/paper/uncertainty_weighting_report.md`, `output/logs/uncertainty_weighting_run.log`, and `output/logs/uncertainty_session_info.txt` for completed runs and limitations.
- `R/checks/02_audit_sampling_support_agent_v1.R` and `R/sensitivities/01_run_weighting_sensitivities_agent_v1.R` for manuscript audit and sensitivity steps. See `exploratory/working_covariance_review.md` for separate exploratory REML evidence.

## October 27 addition — Predictor-omission SI figure

The author requested a focused omission sensitivity and new SI Figure 3. This adds a sixth analysis figure to the prior five-figure scope. The omission script in `R/sensitivities/02_run_predictor_omission_agent_v1.R` and plot script in `R/figures/07_plot_predictor_omission_agent_v1.R` run in the corresponding paper modes. Full-set primary predictions and bootstrap selection summaries must reproduce unchanged. Outputs and methods are documented in the paper index; complete final-size/SI-numbering review alongside the existing figures. This does not implement elastic net, component-grouped prediction validation, or a general causal-importance ranking.

SI Figure 3 production verification: 994 successful DOC and 1,000 nitrate bootstrap refits, with 986/1,000 paired OOB comparisons. Primary full-set predictions and coefficient stability reproduce; exclusion/grouping/paired-error audits pass. PNG and vector PDF exported; PNG visually checked for labels, full error-range retention, zero lines and omitted-cell distinction. Journal-size review remains pending.

## October 27 figure-number update — Group CV becomes Figure 3

The author requested main Figure 3 for cross-validated model comparisons; selection stability becomes Figure 4. There are seven analysis graphics (four main, three SI). The final Figure 3 row order is intercept only, time, time + wildfire, time + watershed, and time + watershed + wildfire. Intercept-only is the zero reference. Time is unpenalized in the three group models; the full-set selection models in Figure 4 retain their original penalty on time. Group CV uses common publication folds, training-fold preprocessing and equal-study RMSE. DOC watershed/both-group gains are 22.5%/27.3% relative to intercept-only; time and time + wildfire are 1.4% worse. Nitrate time-containing models all improve 8.5%, with no additional group gain beyond time. Exact RMSE differences are in the caption/source tables. Check 08 passes for all 515 predictions; figures visually checked. Nested-CV group fitting and Figure 3 are included in primary/all and figures/all. Final manuscript-size review and prose reconciliation remain pending.

## October 27 paired CV diagnostics

Completed author-requested publication-specific paired RMSE differences and improvement counts for every pair among the five Figure 3 models, plus 100,000 paired error resamples clustered by connected publication/fire group. Full DOC beats intercept-only for six/seven publications and watershed-only for five/seven. Full DOC overall improvement 0.10209 has conditional 95% error-bootstrap interval [0.01361,0.16014]; watershed DOC and nitrate baseline comparisons have intervals spanning zero. Outputs and limitations are in `predictor_group_cv/paired_uncertainty/`; analysis 04 runs in primary/all, with independent check 09. These intervals condition on existing publication-validation predictions; fire-component validation/model refitting is a separate unresolved question. Figure 3 was not modified.
