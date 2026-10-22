# Exploratory work

This folder contains methods under review that have not been selected for the paper analysis. Nothing here is called by `prepare`, `primary`, `sensitivities`, `figures`, or `all`. The [active workflow index](../README.md) records the selected analysis and outstanding decisions.

| Work | Script | Review | Saved outputs |
|---|---|---|---|
| Balanced annual synthesis with study/fire-component CR2 uncertainty | [The exploratory annual-uncertainty script](R/01_review_annual_uncertainty_agent_v1.R) | [Annual uncertainty review](annual_uncertainty_review.md) | `output/annual_uncertainty/` |

To rerun the candidate after preparing the primary and observed-only inputs, run from the repository root:

```sh
Rscript agent_workflows/vibe_coding/exploratory/R/01_review_annual_uncertainty_agent_v1.R
```

This writes only the candidate result set. It does not replace the selected primary fits, annual variances, or conditional inference results. Moving these results here does not reject the candidate; primary synthesis selection is still pending author review.

Keep future unselected methods and trial specifications here with their own output folders and brief status notes. When a method is selected, explicitly update the active workflow and Methods before promoting it. Superseded work belongs in [archive](../archive/README.md).
