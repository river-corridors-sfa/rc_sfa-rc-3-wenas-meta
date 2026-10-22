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
