# Active analysis scripts

Use [the paper runner](run_paper_workflow_agent_v1.R) from the repository root. It owns execution order; [the workflow index](../README.md) explains modes, prerequisites, scientific status, and outputs.

- `preparation/`: source chemistry, strict workbook import, reviewed pairing promotion/site selection, annual effects, predictor joins, and preparation audit.
- `analysis/`: selected primary LASSOs (script 11).
- `sensitivities/`: four LASSO scenarios (script 18) and conditional covariance inference (script 19). Script 18 reuses the primary fitting implementation.
- `figures/`: the five manuscript plotting scripts; these consume saved inputs.
- `checks/`: runner inventory/sampling audits (16–17) plus standalone annual-estimand and weighting/preprocessing regression checks.

The standalone checks can be run from the repository root:

```sh
python3 agent_workflows/vibe_coding/R/checks/check_annual_estimand_agent_v1.py
Rscript agent_workflows/vibe_coding/R/checks/check_weighting_support_agent_v1.R
```

The weighting check loads the model's setup and writes validation metadata; it does not run bootstrap fitting. Both checks require prepared analysis inputs. Exploratory script 20 moved to [exploratory](../exploratory/README.md). Filenames retain their original numbers for traceability; directory names identify their role.
