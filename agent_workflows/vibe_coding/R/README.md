# Active analysis scripts

Use [the paper runner](run_paper_workflow_agent_v1.R) from the repository root. It owns execution order; [the workflow index](../README.md) explains modes, prerequisites, scientific status, and outputs.

- `preparation/`: source chemistry, strict workbook import, reviewed pairing promotion/site selection, annual effects, predictor joins, and preparation audit.
- `analysis/`: selected primary LASSOs (`R/analysis/01_fit_issue6_revised_primary_agent_v1.R`), balanced pooled synthesis (`R/analysis/02_fit_balanced_synthesis_agent_v1.R`), and time-adjusted predictor-group CV (`R/analysis/03_fit_predictor_group_cv_agent_v1.R`).
- `sensitivities/`: four weighting/interpolation LASSO scenarios (`R/sensitivities/01_run_weighting_sensitivities_agent_v1.R`) and 12 paired predictor-omission specifications (`R/sensitivities/02_run_predictor_omission_agent_v1.R`). The weighting sensitivity script reuses the primary fitting implementation.
- `figures/`: the seven manuscript/SI plotting scripts; these consume saved inputs.
- `manuscript/`: Python builder for the three-section statistical SI packet, full-precision tables, coefficient distributions and main-text reference replacements; consumes saved outputs through runner mode `si` or the final step of `all`.
- `checks/`: runner inventory/sampling audits (01–02) plus standalone annual-estimand, weighting/preprocessing and exclusion-propagation checks.

The standalone checks can be run from the repository root:

```sh
python3 agent_workflows/vibe_coding/R/checks/03_check_annual_estimand_agent_v1.py
Rscript agent_workflows/vibe_coding/R/checks/04_check_weighting_support_agent_v1.R
python3 agent_workflows/vibe_coding/R/checks/05_check_paper_exclusions_agent_v1.py
```

The weighting check loads the model's setup and writes validation metadata; it does not run bootstrap fitting. Both checks require prepared analysis inputs. The selected balanced synthesis runs in `primary` and `all`; the earlier candidate remains in [exploratory](../exploratory/README.md) as historical evidence. Script numbers are sequential within each folder; the runner defines order across folders.

Check 05 requires completed 1,000-attempt primary/sensitivity refits, balanced CR2 and the current Figure 2 exports; it verifies the author-directed exclusion across every active result inventory.

Check 06 audits predictor-omission grouping, paired held-out records and error calculations, and reproduction of primary predictions/coefficient stability. It requires the completed 1,000-attempt omission analysis:

```sh
python3 agent_workflows/vibe_coding/R/checks/06_check_predictor_omission_agent_v1.py
```

Check 07 exercises zero/one usable-predictor and sparse inner folds, then freshly refits every full-set outer fold to verify that the extended fitting code reproduces primary predictions:

```sh
Rscript agent_workflows/vibe_coding/R/checks/07_check_omission_training_edges_agent_v1.R
```

Check 08 validates the main Figure 3 time-adjusted group comparisons, matched baseline/splits, metric calculations, provenance and Figure 4 renumbering:

```sh
python3 agent_workflows/vibe_coding/R/checks/08_check_predictor_group_cv_agent_v1.py
```

`analysis/04_summarize_paired_cv_uncertainty_agent_v1.R` postprocesses fixed Figure 3 predictions into publication-level paired errors/counts and component-bootstrap uncertainty, without model fitting. Default `N_ERROR_BOOTSTRAP=100000` is separate from the fitting bootstrap count. Check 09 independently verifies every component draw, error target and percentile interval:

```sh
python3 agent_workflows/vibe_coding/R/checks/09_check_paired_cv_uncertainty_agent_v1.py
```
