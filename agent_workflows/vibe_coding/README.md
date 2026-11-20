# Independent analysis of the 18-pair design

This folder holds the 18-pair analysis and two ways to rebuild its first post-fire water-year inputs.

## Rebuild from the study source files

From the repository root, run:

```sh
Rscript agent_workflows/vibe_coding/R/rebuild_18_pairs_from_source.R
```

The [source runner](R/rebuild_18_pairs_from_source.R) uses `tidyverse`, `lubridate`, `here`, and `openxlsx`. It reads the full set of per-study chemistry CSVs from `inputs/Studies/meta_final/`, harmonizes units and interpolates daily chemistry while preserving the observed flags, imports the completed [pairing review workbook](config/pairing_review_workbook.xlsx), and combines the reviewed sites with the [saved GIS attributes](data/source/geospatial_variables_bp_severity_pull.csv). It then applies the [18-pair roster](config/paper_first_water_year_pairs.csv), [calendar-key exclusion](config/paper_record_exclusions.csv), and first-water-year rule.

The [source snapshots](config/source_manifest.csv) trace the GIS pull and the two historical comparison inputs. The historical daily table supplies site labels for the workbook import, and the historical annual effects support a before/after audit. Fresh effect sizes use chemistry rebuilt from the per-study extracts. The digitized study extracts, workbook decisions, and GIS pull are starting inputs.

Fresh outputs land under `data/derived/rebuild_18_pairs/`. Its `inputs/` contains the regenerated roster, predictors, daily paired chemistry, and exclusions. Its `model_inputs/` contains the primary and observed-only tables and the coverage table. The runner also creates local audits under `data/audit/` and exports the reviewed pair and site decisions under `config/`. The [paper inclusion and independence assessment](output/paper/first_water_year_inclusion.md) explains which pairs contribute data and which share a publication or fire.

## Rebuild from prepared inputs

From the directory containing this README, run `Rscript rebuild_inputs.R` with `dplyr`, `readr`, and `tidyr`. From an R session at the repository root, run `source("agent_workflows/vibe_coding/rebuild_inputs.R")`. Both commands read `agent_workflows/vibe_coding/inputs/` and write to `agent_workflows/vibe_coding/model_inputs/`.

## Analysis inputs

| File | Contents |
|---|---|
| `inputs/pairs.csv` | The 18 selected pairs, named sites, publication IDs, fire IDs, and fire-start water year |
| `inputs/daily_paired_chemistry.csv` | All reviewed daily DOC and nitrate rows for the 18 pairs, including flags for valid positive pairs and separately observed burn and reference concentrations |
| `inputs/watershed_predictors.csv` | One row of prepared static watershed and wildfire predictors per pair |
| `inputs/calendar_year_exclusions.csv` | The carried-forward Payson nitrate exclusion, applied before aggregating by water year |
| `inputs/lasso_candidate_predictors.csv` and `inputs/predictor_dictionary.csv` | Prior candidate lists and predictor transformations. Omit `post_fire_year` from a first-water-year fit because it is constant. |

The source runner prepares the paired chemistry in `data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv` and the static covariates in `data/derived/lasso_model_table.csv`.

## Rebuilt model inputs

The target window is October 1 following the fire-start water year through September 30 of the next calendar year. For a September 2018 fire, the window is October 1, 2018 through September 30, 2019. The script requires at least two eligible paired dates and calculates `lnRR_mean` as the log of the mean burn concentration divided by the mean reference concentration.

- `model_inputs/first_water_year.csv` uses valid paired dates, including dates supported by interpolation. It contains 8 DOC and 13 nitrate effects across 15 pairs.
- `model_inputs/first_water_year_observed_only.csv` requires direct measurements at both sites on at least two paired dates. It contains 8 DOC and 10 nitrate effects.
- `model_inputs/coverage.csv` shows eligibility and paired-date counts for all 18 pairs and both analytes.

Fit DOC and nitrate separately. Use `Study_ID` to assign validation folds. In these inputs the publication and wildfire groups coincide, giving 6 DOC and 10 nitrate holdout groups, or 6 and 9 for the observed-only subset. The annual delta variance assumes independent days and is provisional. Keep that limitation in mind if weighting by its inverse.

The Hauer & Spencer nitrate effects each have one directly observed paired date and drop in the observed-only table. Oliver compares two locations on Angora Creek. Gluns & Toews has a source note describing upstream–downstream concentration differences, and its two sites map to the same reach in the source GIS file. The bundle carries those records forward under the existing inclusion decisions.
