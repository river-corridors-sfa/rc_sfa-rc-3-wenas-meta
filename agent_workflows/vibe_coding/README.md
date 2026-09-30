# Active paper workflow

The active workflow consists of reviewed-pair annual data preparation, the user-defined DOC and nitrate LASSOs, and five paper figures. Start with the [paper output index](output/paper/README.md).

From the repository root, run:

```sh
N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R
```

This uses the prepared `data/derived/lasso_model_table.csv`. Set `PAPER_FROM_SOURCE=1` to rebuild that table from the reviewed pairing decisions and source chemistry first. The exact active predictor lists are in `config/issue6_revised_primary_predictors.csv`. Older exploratory scripts and outputs are retained in `archive/preliminary_2026_09_29/` and are not called by the paper runner.

Active upstream steps are `01_read_and_gapfill_preserve_observations_agent_v1.R`, `01b_promote_reviewed_pairings_agent_v1.R`, `01c_apply_reviewed_sites_agent_v1.R`, `02_prepare_analysis_data_agent_v1.R`, and `03_audit_pairs_and_predictors_agent_v1.R`. The later active scripts generate the model tables and five figures listed in the paper output index.

Annual effect sizes use `log(annual_mean_burn / annual_mean_reference)`, with arithmetic means over the same matched positive-concentration dates within calendar year. `lnRR_mean` retains this response for compatibility. Paired delta variances include same-date covariance but remain provisional with respect to temporal dependence and interpolation; singleton variances remain missing. The comparison with mean daily log ratios is in `data/audit/annual_estimand_comparison.csv` (relative to the workflow root).

Paper eligibility excludes annual summaries with fewer than two matched dates. The final table has 104 rows (35 DOC, 69 nitrate). See `output/paper/paper_inventory_report.md` from the workflow root for the verified 12-study, 28-pair, 46-watershed inventory and exclusion details. Upstream annual tables retain all 110 records.
