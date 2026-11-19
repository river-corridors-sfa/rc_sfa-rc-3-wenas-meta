# Independent analysis of the 18-pair design

This folder holds the selected inputs and a standalone R rebuild of the paper’s first post-fire water-year dataset. From this folder, run `Rscript rebuild_inputs.R`. The script uses `dplyr`, `readr`, and `tidyr`. It reads only files in this folder and writes the regenerated inputs to `model_inputs/`.

## Analysis inputs

| File | Contents |
|---|---|
| `inputs/pairs.csv` | The 18 selected pairs, named sites, publication IDs, fire IDs, and fire-start water year |
| `inputs/daily_paired_chemistry.csv` | All reviewed daily DOC and nitrate rows for the 18 pairs, including flags for valid positive pairs and separately observed burn and reference concentrations |
| `inputs/watershed_predictors.csv` | One row of prepared static watershed and wildfire predictors per pair |
| `inputs/calendar_year_exclusions.csv` | The carried-forward Payson nitrate exclusion, applied before aggregating by water year |
| `inputs/lasso_candidate_predictors.csv` and `inputs/predictor_dictionary.csv` | Prior candidate lists and predictor transformations. Omit `post_fire_year` from a first-water-year fit because it is constant. |
| `source_extracts/` | The 12 original per-study chemistry extracts, reviewed site selections for the 18 pairs, and source GIS rows for the selected site labels |

The rebuild starts from the reviewed, harmonized daily chemistry and the prepared covariates. The original study extracts and GIS rows are available for auditing those upstream steps. Rebuilding the daily interpolation or GIS covariates would be a separate step.

In the parent project, the paired chemistry comes from `data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv` and the static covariates come from `data/derived/lasso_model_table.csv`. The files in `source_extracts/study_chemistry/` are copies of `inputs/Studies/meta_final/` in the repository root.

## Rebuilt model inputs

The target window is October 1 following the fire-start water year through September 30 of the next calendar year. For a September 2018 fire, the window is October 1, 2018 through September 30, 2019. The script requires at least two eligible paired dates and calculates `lnRR_mean` as the log of the mean burn concentration divided by the mean reference concentration.

- `model_inputs/first_water_year.csv` uses valid paired dates, including dates supported by interpolation. It contains 8 DOC and 13 nitrate effects across 15 pairs.
- `model_inputs/first_water_year_observed_only.csv` requires direct measurements at both sites on at least two paired dates. It contains 8 DOC and 10 nitrate effects.
- `model_inputs/coverage.csv` shows eligibility and paired-date counts for all 18 pairs and both analytes.

Fit DOC and nitrate separately. Use `Study_ID` to assign validation folds. In these inputs the publication and wildfire groups coincide, giving 6 DOC and 10 nitrate holdout groups, or 6 and 9 for the observed-only subset. The annual delta variance assumes independent days and is provisional. Keep that limitation in mind if weighting by its inverse.

The Hauer & Spencer nitrate effects each have one directly observed paired date and drop in the observed-only table. Oliver compares two locations on Angora Creek. Gluns & Toews has a source note describing upstream–downstream concentration differences, and its two sites map to the same reach in the source GIS file. The bundle carries those records forward under the existing inclusion decisions.
