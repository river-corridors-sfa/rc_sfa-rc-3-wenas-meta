# Next steps after site and watershed-pair confirmation

This guide describes the analysis sequence to follow after a co-author confirms the burned and reference sites, watershed pairings, fire assignments, and inclusion decisions in the review workbook.

Confirmation moves the workflow from **provisional development** to **final analysis preparation**. Historical lnRR pairs should no longer be treated as automatically included.

## 1. Complete the strict pairing-review import

Use `R/01_pairing_review_workbook_agent_v1.R`.

1. Confirm that the completed workbook is saved as `config/pairing_review_workbook.xlsx`.
2. In the script, set `review_action <- "import"` and `allow_partial_import <- FALSE`.
3. Run the script.
4. Review `config/pairing_decisions_reviewed.csv` and `data/audit/pairing_review_validation.csv`.

Do not proceed until every candidate pair has a final status and the validation file contains no unresolved errors. Each included pair needs a final fire ID, pairing type, inclusion decision, decision status, and evidence note. Legitimate shared-reference pairs should remain included and retain a shared-control identifier.

## 2. Promote confirmed decisions into the analysis configuration

The current modeling scripts read `config/pairing_decisions_analysis.csv`. Replace its provisional fields with confirmed values from `config/pairing_decisions_reviewed.csv`:

| Confirmed review field | Analysis field |
|---|---|
| `Fire_ID_Final` | `Fire_ID_Analysis` |
| `Pairing_Type_Final` | `Pairing_Type_Analysis` |
| `Include` | `Include_Analysis` |
| `Decision_Status` | `Analysis_Decision_Status` |
| confirmed evidence and notes | analysis evidence and notes |
| shared-control identifier | `shared_control_id` |
| co-author confirmation | `Coauthor_Confirmation = "confirmed"` |

Preserve excluded rows in the configuration for provenance, with `Include_Analysis = FALSE`.

### Important provisional-script safeguard

Do **not** rerun `R/01a_create_provisional_pairings_agent_v1.R` after confirmed decisions are promoted. It rebuilds the provisional table with all 36 historical pairs and `Coauthor_Confirmation = "pending"`.

The current `R/run_all_agent_v1.R` includes script 01a. Until that runner is revised to omit the provisional generator, run scripts 02–07 individually.

## 3. Build the confirmed annual model table

Run `R/02_prepare_analysis_data_agent_v1.R`.

Primary output: `data/derived/lasso_model_table.csv`.

Before modeling, verify that:

- only confirmed included pairs appear;
- excluded or unsupported cross-products are absent;
- no new burned × reference combinations were generated;
- each row is unique for pair × analyte × post-fire year;
- final fire IDs and post-fire years are plausible;
- shared-reference pairs retain `shared_control_id`;
- burned-watershed attributes joined to the intended site;
- response values agree with the established annual lnRR table where pairing and aggregation rules are unchanged.

Resolve mismatches in the pairing configuration or preparation script, not by editing the derived table.

## 4. Audit pairs, response coverage, and predictors

Run `R/03_audit_pairs_and_predictors_agent_v1.R`.

Review the files under `data/audit/`, including:

- study, comparison, pair, and year counts;
- shared-reference families and contrasts per reference;
- DOC and nitrate coverage by study and pair;
- missingness by predictor and analyte;
- predictor ranges, transformations, and low-variation flags;
- predictor correlations and redundancy;
- matched DOC–nitrate coverage.

Then review `config/predictor_dictionary.csv`. Finalize the primary predictor set before fitting final models. Do not include a variable only because it is available, or retain nearly redundant predictors without a scientific reason.

## 5. Resolve remaining analysis decisions

Record final choices in `.codex/decisions.md` before interpreting results:

1. **Shared references:** retain legitimate contrasts but account for dependence. The current family-adjusted weighting is a sensitivity analysis, not an exact covariance model.
2. **Effect-size variance:** decide whether annual variance is adequate given daily interpolation and temporal autocorrelation.
3. **Time since fire:** confirm fire year and post-fire-year assignments, especially for multi-fire studies.
4. **Predictors:** approve the primary list, transformations, and missing-data rules.
5. **Inference scope:** decide whether DOC supports a primary predictive analysis or should be described as exploratory because of its smaller study count.

If exact shared-reference covariance will be used, implement and verify it before the final meta-analysis. LASSO should still use grouped resampling and sensitivity analyses rather than treating shared-reference rows as independent evidence.

## 6. Fit the multilevel meta-analysis

Run `R/04_fit_meta_analysis_agent_v1.R`.

Confirm that DOC and nitrate are fit separately, random-effects IDs match the confirmed hierarchy, reported-variance and family-adjusted results are distinguished, failed or singular fits are investigated, and temporal coefficients use the confirmed post-fire-year definition.

These models estimate average effects, recovery, heterogeneity, and uncertainty. LASSO is not a substitute for this hierarchical inference.

## 7. Fit grouped benchmark and LASSO models

Run `R/05_fit_grouped_lasso_agent_v1.R`.

Confirm that each outer fold holds out an entire study and preprocessing is learned from training data only. Review held-out predictions, fold membership, RMSE, MAE, held-out R², benchmark comparisons, coefficients at `lambda.1se`, and failed folds.

Performance claims must use held-out predictions. If the full LASSO does not improve on simpler benchmarks, do not claim that watershed predictors generalize.

## 8. Run stability and sensitivity analyses

Run `R/06_run_stability_sensitivity_agent_v1.R`.

Use the default 100 bootstrap iterations while debugging. For the final run:

```r
Sys.setenv(N_BOOTSTRAP = 1000)
source("agent_workflows/vibe_coding/R/06_run_stability_sensitivity_agent_v1.R")
```

Review selection frequency, median standardized coefficients, sign consistency, and failed iterations for LASSO, elastic net, weighted/unweighted fits, and shared-reference sensitivity. Interpret selected variables as stable predictive information conditional on the candidate set, not as causal proof.

## 9. Generate manuscript-facing results

Run `R/07_make_results_agent_v1.R`.

Inspect every source table and figure under `output/`. Confirm that provisional labels are removed only after validation, counts match the audited dataset, predictive figures use held-out results, stability plots state the resampling unit and successful iterations, shared-reference sensitivity is reported, and DOC–nitrate differences are not overstated.

## 10. Reproducibility check

After decisions and scripts are final:

1. Start a clean R session.
2. Run scripts 02–07 in order.
3. Confirm every output is regenerated without hand edits.
4. Save package and session information under `output/logs/`.
5. Commit the confirmed pairing table, predictor dictionary, decisions, scripts, and results in traceable commits.
6. Update `.codex/history.md` with the final run, failures, deviations, and manuscript-results commit.

## Recommended final run order

```r
source("agent_workflows/vibe_coding/R/02_prepare_analysis_data_agent_v1.R")
source("agent_workflows/vibe_coding/R/03_audit_pairs_and_predictors_agent_v1.R")

# Stop here to review audits and finalize predictor/model decisions.

source("agent_workflows/vibe_coding/R/04_fit_meta_analysis_agent_v1.R")
source("agent_workflows/vibe_coding/R/05_fit_grouped_lasso_agent_v1.R")

Sys.setenv(N_BOOTSTRAP = 1000)
source("agent_workflows/vibe_coding/R/06_run_stability_sensitivity_agent_v1.R")
source("agent_workflows/vibe_coding/R/07_make_results_agent_v1.R")
```

## Completion criteria

The post-confirmation workflow is complete when:

- the strict pairing import passes validation;
- every analyzed pair is explicitly confirmed;
- the provisional pairing generator is absent from the final run;
- the model table and audits pass pair, fire, time, and shared-reference checks;
- the predictor dictionary and modeling choices are documented;
- meta-analysis, grouped prediction, and stability analyses run successfully;
- final results regenerate in a clean session without manual changes.
