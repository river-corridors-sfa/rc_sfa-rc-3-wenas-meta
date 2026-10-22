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
