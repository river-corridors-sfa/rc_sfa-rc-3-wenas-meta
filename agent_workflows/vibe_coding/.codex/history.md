# Project history

## 2026-08-20 — Repository and analysis assessment

- Task: Assess the current repository against the proposed LASSO-centered manuscript.
- Files changed: None.
- Decisions made: The paired annual wildfire effect size, rather than burned or unburned concentration alone, should be the primary LASSO response. Multiple watershed pairs provide ecological replication, while study, comparison, shared-control, and repeated-year dependence must be preserved.
- Unresolved issues: Pairing structure, interpolation effects, effect-size variance, and the limited number of independent DOC studies require explicit treatment.

## 2026-08-20 — Agent workflow organization

- Task: Propose a clean workflow under the new `agents` branch.
- Files changed: None.
- Decisions made: Keep a small linear sequence of new agent-versioned scripts; retain upstream paired chemistry, geospatial attributes, and site metadata; exclude normalization and historical exploratory analyses from the active workflow.
- Unresolved issues: The `vibe_coding/` directory and project memory files had not yet been initialized.

## 2026-08-20 — Source manifest

- Task: Create the source manifest for the new agent analysis workflow.
- Files changed: `agent_workflows/vibe_coding/config/source_manifest.csv`; initialized `agent_workflows/vibe_coding/.codex/history.md`.
- Decisions made: Pin retained inputs to the pre-workflow branch commit and individual blob SHAs; treat the existing annual effect-size table as a QC reference rather than an authoritative modeling input; document interpolation, shared-control, Gaviota COMID, and area-normalization caveats.
- Unresolved issues: Source datasets have not yet been copied into `vibe_coding/data/source/`; pairing decisions still require author review.

## 2026-08-20 — Project decisions and history

- Task: Initialize durable project decisions and expand the session history.
- Files changed: `agent_workflows/vibe_coding/.codex/decisions.md` and `agent_workflows/vibe_coding/.codex/history.md`.
- Decisions made: Recorded current workflow organization, scientific scope, observational hierarchy, model roles, validation requirements, predictor-selection principles, and inactive historical analyses.
- Unresolved issues: Pair definitions, shared-control treatment, effect-size weighting, temporal aggregation, the final predictor set, and the strength of inference supported by the DOC sample size remain provisional.
- Next steps: Build and review `pairing_decisions.csv`, then construct and audit the annual pair-level modeling table.

## 2026-08-20 — Candidate pairing decisions table

- Task: Generate an author-review table of candidate burned-reference watershed contrasts and document its construction in R.
- Files changed: `agent_workflows/vibe_coding/config/pairing_decisions.csv`, `agent_workflows/vibe_coding/R/00_build_pairing_decisions_agent_v1.R`, and this history file.
- Decisions made: Inventory distinct pairings from the existing annual effect-size table; enrich them with study-level fire metadata; flag shared references and multi-fire studies; leave final fire assignment, pairing type, inclusion, and decision fields blank for author review.
- Unresolved issues: The 36 candidate contrasts have not been validated against the original study designs. Shared-control handling and comparison-specific fire assignments remain unresolved.
- Next steps: Authors should complete `Fire_ID_Final`, `Pairing_Type_Final`, `Include`, `Decision_Status`, and `Decision_Notes` before the final effect-size table is rebuilt.

## 2026-08-20 — Pairing review workbook workflow

- Task: Create an R workflow to support structured author review of candidate watershed pairings.
- Files changed: `agent_workflows/vibe_coding/R/01_pairing_review_workbook_agent_v1.R` and this history file.
- Decisions made: Use one linear script with a create mode and an import mode. The workbook includes instructions, study summaries, actual watershed names, review priorities, controlled decision values, dropdowns, and highlighted editable fields. Import writes a new reviewed CSV and a validation report; it does not overwrite the original candidate table.
- Unresolved issues: The workbook has not yet been completed by reviewers. Final fire IDs, pairing types, inclusion decisions, evidence citations, and shared-reference treatment remain pending.
- Next steps: Run the script locally with `review_action <- "create"`, review the workbook by Study_ID, then rerun with `review_action <- "import"` and `allow_partial_import <- FALSE` for the final export.

## 2026-08-20 — Remaining analysis-script placeholders

- Task: Create the remaining proposed analysis scripts while authors review watershed pairings.
- Files changed: `agent_workflows/vibe_coding/R/02_prepare_analysis_data_agent_v1.R`, `03_audit_pairs_and_predictors_agent_v1.R`, `04_fit_meta_analysis_agent_v1.R`, `05_fit_grouped_lasso_agent_v1.R`, `06_run_stability_sensitivity_agent_v1.R`, `07_make_results_agent_v1.R`, and this history file.
- Decisions made: Continue the linear numbered workflow after scripts 00–01; cite rather than overwrite the human scripts; use annual non-area-normalized lnRR; reserve study-grouped validation as primary; and make every placeholder stop safely before incomplete analysis can produce results.
- Unresolved issues: Pair approval, shared-control treatment, effect-size variance, post-fire-year definition, final predictor dictionary, weighting, and the strength of DOC inference remain open.
- Next steps: Finish and import the pairing review, then implement script 02 and use its audit outputs to finalize the remaining modeling choices.

## 2026-08-20 — Provisional adoption of established lnRR pairings

- Task: Continue workflow development under the assumption that pairs already used by the lnRR script were manually reviewed, while preserving formal co-author review.
- Files changed: `agent_workflows/vibe_coding/R/01a_create_provisional_pairings_agent_v1.R`, `R/02_prepare_analysis_data_agent_v1.R`, `config/pairing_decisions_analysis.csv`, `.codex/decisions.md`, and this history file.
- Decisions made: Provisionally include all 36 established pairs; classify 24 as shared-reference comparisons; keep every confirmation status pending; use `Comparison_ID` as the temporary fire/comparison identifier; and leave the review workbook and original decision fields unchanged.
- Unresolved issues: A co-author must confirm that the established lnRR pairs were manually vetted. Shared/composite reference interpretation and final multi-fire assignments still require confirmation.
- Next steps: Implement the annual model-table construction using `pairing_decisions_analysis.csv`, then replace provisional fields with the completed review export before final modeling.
