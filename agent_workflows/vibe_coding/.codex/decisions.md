# Project decisions and conventions

The [active workflow index](../README.md) is the current source for selected methods, inputs, execution modes, outputs, and unresolved decisions. The [historical decision log](decisions_history.md) preserves the previous dated entries verbatim, including superseded choices. Use the current index to interpret those entries.

## Working conventions

- Keep all LLM-generated files under `agent_workflows/vibe_coding/`; preserve human-authored scripts elsewhere. Adapt human code into a separate `_agent_v1` file and cite its source.
- Keep R code linear and readable; extract functions for repeated or complex logic.
- Preserve source snapshots and their provenance in `config/source_manifest.csv`; keep generated data and outputs separate from source data.
- The paper runner owns preparation order. Preparation scripts consume the previous step's files and do not implicitly rerun earlier steps. The weighting sensitivity script deliberately reuses `R/analysis/01_fit_issue6_revised_primary_agent_v1.R` for its four sensitivity scenarios.
- With no arguments, the runner retains the existing full analysis from prepared data. `PAPER_FROM_SOURCE=1` with `all` retains the from-source rebuild. Explicit modes select a subset without changing scientific defaults.
- Archive obsolete scripts and decisions; do not delete historical evidence or silently promote candidate methods to primary analysis.

## October 22, 2026 — Workflow simplification

Added prepare, primary, sensitivities, figures, and all modes with a dry-run option. Centralized workbook import, promotion, and site-selection order in the runner. Consolidated current workflow status in the active index; retained the complete prior decision log separately. Analysis methods and the pending synthesis choice are unchanged.

## October 22, 2026 — Analysis and exploratory layout

Active scripts are grouped under `R/preparation/`, `R/analysis/`, `R/sensitivities/`, `R/figures/`, and `R/checks/`. The runner stays at its established path. The unselected annual-uncertainty script, its review, and its saved outputs live under `exploratory/` and are not called by the runner. Paper outputs retain their established paths; loose execution logs moved to `output/logs/`. Human-authored source, historical archives, and scientific defaults remain preserved. Current documentation uses the new paths; historical logs and archived decisions retain original paths as evidence.

## October 22, 2026 — Sequential script numbering

Scripts are numbered sequentially within each role folder: preparation 01–06, checks 01–04, analysis 01, sensitivities 01–02, figures 01–05, and exploratory/R 01. The runner retains its existing name and defines execution order across folders. Checks 03–04 remain standalone. Current code and documentation use the new names; archived instructions and historical logs retain old names. Data/output filenames and scientific behavior are unchanged.


## October 22, 2026 — Author selected balanced primary synthesis

Use equal study → equal pair → equal available-year pooled log response ratios with connected study/fire-component CR2/Satterthwaite intervals as primary. Rhea and Writer connect through High Park (six DOC and eleven nitrate components); component membership changes uncertainty, not study balance. No annual precision weights enter this synthesis. Publication-cluster intervals, observed-only/matched-interpolated summaries and provisional-variance REML remain supporting sensitivities. Time associations are exploratory with about 1.3 effective slope df. Annual variance validation, cross-publication link review and time attribution remain open; LASSO validation/bootstrap groups stay publication-based. Promote the candidate implementation to `R/analysis/02_fit_balanced_synthesis_agent_v1.R`, run it in `primary`/`all`, and retain earlier exploratory evidence. Preserve the five current graphics and document each figure's distinct analysis unit and inferential scope in `output/paper/README.md`; pooled inference belongs in Results/table alongside the descriptive Figure 2.


## October 22, 2026 — Figure 2 raincloud replaces boxplot

At author request, Figure 2 now displays balance-weighted half-violins, faint annual observations, and pooled diamonds with connected study/fire-component 95% CR2 intervals. Keep the complete range and existing colors; distinguish record/publication/component counts and density variation from pooled uncertainty in the caption. This supersedes the earlier plan to keep Figure 2 entirely descriptive. Preserve the exact former graphic, script and summary table under `archive/figure_2_boxplot_2026_10_22/`. The runner uses `R/figures/02_plot_response_raincloud_agent_v1.R` and requires saved synthesis weights/results; no model refit.


## October 22, 2026 — Author-directed nitrate outlier exclusion

Exclude exactly Crandall et al. 2021 / Crandall_et_al_2021_C2 / Site_2–Control_2 / NO3 / 2018 (lnRR −4.506808658089654) from all current manuscript analyses, sensitivities and figures. This is a post hoc exclusion based on extremeness, with no confirmed measurement/transcription error. Preserve chemistry and all 110 upstream annual effects; keep the complete prior 104-record package/model tables in `archive/pre_nitrate_exclusion_2026_10_22/`. Enforce the exact configured annual key in preparation script 05 and export the full row/reason. Recompute balance weights, validation, 1,000-attempt bootstrap stability, interpolation/weight sensitivities, CR2 synthesis, REML sensitivities and all figures. Retain the DOC counterpart; overall inventory remains 28 pairs/46 watersheds, nitrate now spans 27 pairs/44 watersheds. Primary inventory is 103 records (35 DOC, 68 nitrate); observed-only/matched inventory is 88 (32 DOC, 56 nitrate). Earlier keep-all-observations figure guidance is superseded for this record only.


## October 27, 2026 — REML moved to exploratory work

At author request, conditional working-covariance REML is exploratory, not a manuscript sensitivity or readiness requirement. Its standalone script is `exploratory/R/02_fit_working_covariance_inference_agent_v1.R`; saved tables/model objects and future outputs live in `exploratory/output/working_covariance/`. All paper runner modes and the manuscript exclusion check exclude REML. Balanced CR2 synthesis, publication-cluster/interpolation/deletion comparisons, and LASSO sensitivities remain unchanged. Prior October 22 descriptions of REML as a supporting manuscript sensitivity are superseded. Historical logs and archives remain intact.


## October 27, 2026 — Paired predictor-omission LASSO and SI Figure 3

At author request, add focused individual/joint burn and hydrology omissions (seven specifications per analyte). Retain primary balance weights, preprocessing, penalty grid and one-SE rule. Use identical publication-bootstrap training draws, inner folds and OOB test publications across specifications, plus nested LOSO predictions. Run 1,000 attempts/analyte; selection summaries use successful refits including no-OOB draws, paired error differences require OOB publications. Empirical OOB ranges are descriptive, not formal confidence intervals. Publication grouping remains unchanged. Preserve Figure 3 and primary outputs; add SI Figure 3 and saved omission outputs to paper sensitivity/figure/all modes. Reproduction of full-set primary predictions and bootstrap coefficient/selection summaries is required. The primary model is not replaced by an omission model selected after viewing these results.


## October 27, 2026 — Extended to hypothesis-oriented predictor groups

The author selected structured predictor-subset bootstrapping augmented with complete group exclusions as the primary predictor-stability framework, with nested publication-level CV as the primary measure of predictive contribution. Extend the initial seven specifications to 12: full set, eight individual omissions, complete wildfire group, complete watershed group, and joint hydrology omission. Watershed = area/runoff/baseflow/forest/soil; wildfire = burned area/high-severity burn; time remains separate in both group exclusions. Use primary nested-LOSO contrasts and secondary paired OOB stability summaries; preserve original fits/Figure 3. Sparse folds fit one available predictor or a weighted intercept if none vary; internal zero padding is not a candidate. Across-analyte comparisons are descriptive because inventories and soil variables differ; no causal ranking or independent-wildfire validation is claimed. This supersedes the initial seven-specification scope.


## October 27, 2026 — Figure 3 time-baseline comparisons; selection becomes Figure 4

Author requested Figure 3 for cross-validated predictor-group importance and renumbered the original selection graphic to Figure 4. Final Figure 3 rows are time + watershed, time + wildfire, time + watershed + wildfire, and intercept-only; the common time-only model is the zero reference. Fit dedicated time-adjusted group models with time unpenalized (factor zero), group predictors penalized (factor one), identical publication LOSO and inner study folds, training-fold preprocessing, hierarchical fitting weights and one-SE tuning. The intercept-only and time-only predictions reproduce the established benchmarks. Report percent RMSE reduction relative to time-only and retain actual RMSE/differences in caption/source tables, including negative values. These dedicated models differ from Figure 4/full-set omission models that penalize time; preserve those original fits. Renumber plotting scripts sequentially 01–07; SI numbering unchanged. Add group CV to primary/all and the new main figure to figures/all. This supersedes older references calling the selection graphic Figure 3.


## October 27, 2026 — Final Figure 3 reference and model order

Author revised the comparison basis to intercept-only and explicitly requested the row order intercept only, time, time + wildfire, time + watershed, time + watershed + wildfire. Use all five fitted models in the comparison table, with intercept-only at zero. Compute positive improvement as (intercept RMSE − model RMSE) / intercept RMSE; keep negative results visible. Reuse existing CV predictions; no refit is needed for this reference change. Time remains unpenalized in all three time-plus-group models. This supersedes the time-only zero-reference choice above.


## October 27, 2026 — Paired CV error resampling

At author request, calculate all publication-level paired model RMSE differences, improved/tied/worsened counts and uncertainty in overall improvement from fixed Figure 3 held-out errors. Resample connected publication/fire components jointly (Rhea/Writer linked), preserving publication weight in the mean MSE before square root. Use 100,000 paired draws, seed 20261027, default `N_ERROR_BOOTSTRAP=100000`, 95% percentile intervals and tie tolerance 1e-10 lnRR units. These conditional error-composition intervals do not refit models, fully account for overlapping CV training sets or remove shared-fire information across original validation folds. Positive improvement means reference minus candidate; standard difference has opposite sign. Add analysis 04 after group CV in primary/all and save detailed provenance/draws. Leave Figure 3 unchanged.


## October 27, 2026 — Statistical SI packet

At author request, maintain editable statistical SI for Results 3.1–3.3 under `output/paper/supporting_information/`. Three sections cover pooled synthesis sensitivities, validation/predictor-group comparisons, and selection/weighting/omission stability. Tables S1–S14 are numbered within this packet; preserve existing SI Figures 1–3 and main Figures 1–4. Generate text/tables from saved results with the Python manuscript builder through `si` mode or after figures in `all`, without refitting. Retain exact main-text citation replacements and provenance hashes. Explicitly distinguish time-unpenalized group models from time-penalized selection models, publication folds from dependence-group error resampling, and conditional/empirical intervals from full refitting uncertainty. Final journal numbering and predictor units/provenance remain authoring checks.
