# Active analysis scripts

Use [the paper runner](run_paper_workflow_agent_v1.R) from the repository root. It owns execution order; [the workflow index](../README.md) explains modes, prerequisites, scientific status, and outputs.

- `preparation/`: source chemistry, strict workbook import, reviewed pairing promotion/site selection, annual effects, predictor joins, and preparation audit.
- `analysis/`: selected primary LASSOs (`R/analysis/01_fit_issue6_revised_primary_agent_v1.R`).
- `sensitivities/`: four LASSO scenarios (`R/sensitivities/01_run_weighting_sensitivities_agent_v1.R`) and conditional covariance inference (`R/sensitivities/02_fit_working_covariance_inference_agent_v1.R`). The weighting sensitivity script reuses the primary fitting implementation.
- `figures/`: the five manuscript plotting scripts; these consume saved inputs.
- `checks/`: runner inventory/sampling audits (01–02) plus standalone annual-estimand and weighting/preprocessing regression checks.

The standalone checks can be run from the repository root:

```sh
python3 agent_workflows/vibe_coding/R/checks/03_check_annual_estimand_agent_v1.py
Rscript agent_workflows/vibe_coding/R/checks/04_check_weighting_support_agent_v1.R
```

The weighting check loads the model's setup and writes validation metadata; it does not run bootstrap fitting. Both checks require prepared analysis inputs. The annual-uncertainty candidate lives in [exploratory](../exploratory/README.md). Script numbers are sequential within each folder; the runner defines order across folders.
