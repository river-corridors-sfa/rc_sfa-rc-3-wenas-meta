# Active paper workflow

This is the current workflow and status index, updated October 22, 2026. Paths below are relative to `agent_workflows/vibe_coding/` unless stated otherwise. The repository-root README describes the original published data package. Historical analysis scripts and decisions remain preserved; use this index for the active paper analysis.

## Analysis, exploration, and history

| Location | Purpose | Status |
|---|---|---|
| `R/preparation/` | Reviewed chemistry, pair/site selection, annual effects, model-table preparation | Active analysis inputs |
| `R/analysis/` | Selected primary DOC/nitrate LASSOs | Active paper analysis |
| `R/sensitivities/` | Prespecified weighting/interpolation comparisons and conditional covariance fits | Active supporting analysis; conditional inference is not finalized |
| `R/figures/` | Five manuscript figures from saved inputs | Active paper products |
| `R/checks/` | Inventory, sampling-support audits, and regression checks | Analysis validation |
| [`exploratory/`](exploratory/README.md) | Unselected annual-uncertainty synthesis candidate, review, and saved results | Exploratory; excluded from every runner mode |
| `output/paper/` | Active paper figures, tables, model objects, and supporting reports | Manuscript candidates; outstanding methods decisions still apply |
| `output/paper/sensitivity/` | The four supporting LASSO scenarios | Sensitivity results, separate from primary tables |
| `output/logs/` | Preserved execution, installation, and validation logs | Run records, not paper products |
| [`archive/`](archive/README.md) | Superseded agent workflows and preliminary results | Historical; do not run as the current workflow |

The repository's original `R_scripts/`, `Output_for_analysis/`, and root-level manuscript artifacts belong to the earlier human-authored/published workflow. Root `archive/` contains older reviews and plots. These are preserved in place for provenance; the active runner does not execute them. Inputs and GIS assets outside this folder remain shared source material where explicitly referenced. All new analysis and exploration described here lives under `agent_workflows/vibe_coding/`.

## Run only the work you need

Run from the repository root:

```sh
Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R figures
```

Replace `figures` with the mode you need:

| Mode | Work performed | Prerequisites |
|---|---|---|
| `prepare` | Rebuild corrected chemistry, import and promote reviewed pairings, apply site selections, build the annual model table, and run preparation/inventory/sampling audits | Source chemistry and attributes, completed review workbook, source snapshots |
| `primary` | Inventory/sampling audits and primary DOC/nitrate LASSOs, including held-out predictions and bootstrap stability | Completed preparation |
| `sensitivities` | Inventory/sampling audits, four LASSO sensitivity scenarios, and conditional working-covariance inference (script 19) | Completed preparation and primary results, including `tables/performance_equal_study.csv` |
| `figures` | Regenerate the five figures and their supporting figure tables; no model fitting | Prepared model table, primary selection-stability table and generated predictor dictionary, local map inputs |
| `all` | Audits, primary fits, sensitivities, and figures | Completed preparation; set `PAPER_FROM_SOURCE=1` to prepare first |

No mode argument defaults to `all`, preserving previous behavior. `prepare` always rebuilds from source. `PAPER_FROM_SOURCE=1` is accepted only with `prepare` or `all`, so it cannot unexpectedly expand a figure-only run. Examples:

```sh
# Inspect the planned sequence without executing analysis scripts.
Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R prepare --dry-run

# Full manuscript rebuild, including source preparation.
PAPER_FROM_SOURCE=1 N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R all
```

`N_BOOTSTRAP` defaults to 1000 per analyte/scenario. Reduced values are for development; they overwrite selected-mode outputs and must not be presented as manuscript stability results. Unset `PAPER_SCENARIO`, `PAPER_WEIGHT_METHOD`, and `PAPER_MODEL_TABLE` before using the runner; those overrides are for direct custom calls to script 11.

Modes use saved inputs without detecting whether they are stale. After changing source data or reviewed decisions, run `prepare`, then rerun the downstream analyses and figures you need. After changing the primary predictors, run `primary`, `sensitivities`, and `figures`. `all` is the complete production check. Individual scripts still validate their detailed inputs. R dependencies include tidyverse, here, lubridate, openxlsx, glmnet, metafor, sf, and clubSandwich; the latter may be supplied by the workflow-local `.R-library/` or the normal R library.

## Authoritative inputs and execution order

- Reviewed pair decisions: [`config/pairing_review_workbook.xlsx`](config/pairing_review_workbook.xlsx). This is the authority for inclusion, fire identifiers, pairing types, and review evidence. The original candidate inventory is `config/pairing_decisions.csv`; reviewed and analysis CSVs are generated exports.
- Chemistry and watershed attributes: immutable snapshots in `data/source/`, documented in [`config/source_manifest.csv`](config/source_manifest.csv). Corrected chemistry is generated under `data/derived/gapfill_corrected_agent_v1/`; reviewed daily and annual effects are generated under `data/derived/reviewed_sites_agent_v1/`.
- Primary ordered predictor sets: [`config/issue6_revised_primary_predictors.csv`](config/issue6_revised_primary_predictors.csv). Labels and transformations come from `config/predictor_dictionary.csv`; script 11 generates `config/issue6_revised_primary_dictionary.csv`.
- Shared analysis input: [`data/derived/lasso_model_table.csv`](data/derived/lasso_model_table.csv). Observed-only and matched-interpolated sensitivity tables are generated separately.

The [paper runner](R/run_paper_workflow_agent_v1.R) owns preparation order: corrected gap-fill → strict workbook import → pairing promotion → reviewed site selection/annual effects → model table → preparation audit → inventory and sampling audits. Scripts 01b and 01c consume upstream files; they do not rerun earlier scripts. Script 18 deliberately calls script 11 once for each sensitivity scenario. `--dry-run` lists the exact scripts for any mode.

## Current analysis choices

- Response: `log(annual_mean_burn / annual_mean_reference)` over the same matched positive-concentration dates within calendar year, retained downstream as `lnRR_mean`. The selected analysis axis is time since fire; calendar-year aggregation remains unchanged.
- Inventory: 104 annual responses (35 DOC, 69 nitrate), 12 publications, 28 approved pairs, 14 unique fires, and 46 study-specific physical watersheds. Six singleton annual records remain upstream but are excluded from paper analyses. Pair approval and control selection are settled; the observation-preserving gap-fill fix is active.
- Primary prediction: separate DOC/nitrate LASSOs using the requested eight-predictor sets, equal study → pair → annual-record weights, training-fold preprocessing, study-grouped tuning/validation, and the one-SE rule. Bootstrap copies of the same original study share tuning groups. Selection frequencies use successful refits only.
- Completed sensitivities: equal-row, provisional precision/family, observed-only, and matched-interpolated LASSOs. The latter two use the same 89-record inventory (32 DOC, 57 nitrate). Script 19 provides 36 conditional REML pooled/time fits across nine assumed reference/temporal covariance scenarios, with study-clustered CR2 inference.
- Annual paired delta variances include same-date covariance but remain provisional for temporal dependence and interpolation. Script 20 compares balanced CR2 synthesis candidates and connected study/fire clustering; it remains outside the runner, pending author selection of the primary synthesis.

## Outputs and remaining work

The [paper output index](output/paper/README.md) maps all five figures to their scripts and supporting tables. Primary results live in `output/paper/tables/`; LASSO sensitivities in `output/paper/sensitivity/`; candidate synthesis results in `exploratory/output/annual_uncertainty/`. Execution and validation logs are in `output/logs/`. See the [inventory report](output/paper/paper_inventory_report.md), [weighting report](output/paper/uncertainty_weighting_report.md), and [annual uncertainty review](exploratory/annual_uncertainty_review.md) for evidence. The last documented full bootstrap run completed 994 DOC and 1,000 nitrate refits per scenario out of 1,000 attempts; six DOC draws lacked enough distinct studies for tuning.

Remaining work is tracked in the [manuscript checklist](output/paper/manuscript_readiness_issue.md):

- **Author decisions:** select primary synthesis and annual uncertainty/dependence assumptions; review connected fire/study clustering, time attribution, and limits on DOC predictive claims.
- **Analysis and reproducibility:** complete promised elastic-net, isolated shared-reference weighting, influence, matched-analyte, and alternative predictor sensitivities; export reusable fold assignments; verify a clean full from-source manuscript run.
- **Manuscript verification:** reconcile Methods, predictor units/provenance, screening/biome claims, table/SI references, map positions, and final captions/exports.

The [working conventions](.codex/decisions.md) apply to future edits. The [historical decisions](.codex/decisions_history.md), [session history](.codex/history.md), and `archive/preliminary_2026_09_29/` preserve prior work, including superseded choices; they are not alternative instructions for the current run.
