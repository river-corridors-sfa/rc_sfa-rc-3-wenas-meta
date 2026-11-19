# First post-fire water year inclusion and independence

## Inclusion criteria

The revised paper analysis uses the first full October–September water year after the water year in which the fire started. For example, a September 2018 fire contributes paired samples from October 1, 2018 through September 30, 2019. The [selection CSV](../../config/paper_first_water_year_pairs.csv) records the fire water year and the 18 eligible comparisons. It retains 11 pairs with a dedicated reference plus one burn from each of the seven shared-reference groups.

Use one effect size per selected pair and analyte. Require at least two dates with finite, positive paired concentrations within the target water year. The primary input allows the existing interpolation and calculates the natural log of the ratio of the mean burned concentration to the mean reference concentration. The existing [Crandall Payson nitrate exclusion](../../config/paper_record_exclusions.csv) applies to its original calendar dates before water-year aggregation. The observed-only eligibility check requires at least two dates with direct observations at both sites.

## Resulting inventory

| Analyte | Effect sizes | Publications | Wildfires or complexes | Effect sizes eligible with observed dates only |
|---|---:|---:|---:|---:|
| DOC | 8 | 6 | 6 | 8 |
| Nitrate | 13 | 10 | 10 | 10 |

The 21 effects come from 15 pairs, 11 publications, and 11 fires or complexes overall. Six pairs contribute both analytes. The Hickenbottom Mosquito pair and the two Rhea pairs contribute no eligible effect in this window. The [coverage table](../../model_inputs/coverage.csv) includes all 18 pairs and both analytes. The [model input](../../model_inputs/first_water_year.csv) contains the 21 eligible effects. The source rebuild also generates a local audit with matched date ranges at `data/audit/paper_first_water_year_coverage.csv`.

## Independence assessment

- The selection uses each reference once within a publication, based on reviewed named sites as well as control IDs. Each pair contributes at most one effect per analyte, which removes repeated-year rows and reference reuse within an analyte. DOC and nitrate from the same pair share sites and sampling and are analyzed as separate outcomes.
- Crandall contributes three DOC and two nitrate effects from the Pole Creek Fire Complex. They share a publication, fire complex and October storm. The paper also describes nested sampling in Spanish Fork River and Hobble Creek. Review kept Spanish Fork Lower and excluded Spanish Fork Upper, so the documented nested burned-site duplicate does not add a second retained row. Keep the entire publication together in validation. [Crandall et al., study design and Figure 1](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0257733)
- Hauer & Spencer contributes three nitrate effects from the Red Bench Fire with distinct reference sites. Each effect has just one directly observed paired date in the selected water year. Each passes with interpolation and drops under the observed-only rule. The observed-only nitrate subset therefore has 10 effects from nine publication/fire groups.
- Oliver’s Angora Creek contrast compares upstream and downstream sites on the same stream. It contributes one effect, but the reference and burned measurements are hydrologically linked. Flag it for sensitivity to the reference design. [Pairing review workbook](../../config/pairing_review_workbook.xlsx)
- In Gluns & Toews, the [source extraction](../../../../inputs/Studies/meta_final/Gluns_Toews_1989.csv) describes upstream–downstream concentration differences for each fork. The [GIS pull](../../data/source/geospatial_variables_bp_severity_pull.csv#L20) also assigns East and West Fork sampling locations to the same Kootenai River reach despite distinct coordinates. Verify the concentration basis and the burned-watershed GIS predictors before relying on that nitrate row for prediction. The review classification is carried into the input.
- Each retained publication is associated with one fire or complex, and no retained fire links different publications. Publication-grouped and publication–fire-grouped validation therefore form the same six DOC and ten nitrate holdout groups. These are grouping counts. Shared regional conditions and hydrologic links can remain even with distinct site labels. The review and GIS checks do not establish disjoint catchment boundaries across the full inventory.

The year-since-fire predictor is constant at 1 for these rows. The number of holdout groups limits how much can be learned from new variable selection, especially for DOC.

## Rebuild

```sh
Rscript agent_workflows/vibe_coding/R/rebuild_18_pairs_from_source.R
```

The source runner rebuilds the revised inputs and coverage audit from the study CSVs, pairing workbook, and saved GIS attributes. See the [workflow README](../../README.md) for input details. Updated results and graphics require fitting the revised model input.
