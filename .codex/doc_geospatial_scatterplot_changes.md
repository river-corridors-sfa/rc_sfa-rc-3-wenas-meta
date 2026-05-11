# DOC vs Geospatial Scatterplots

Date: 2026-05-11

## Request

Create scatter plots for geospatial variables in
`Output_for_analysis/03_merge_geospatial/03_master_merged.csv` with DOC on the
y axis, similar to the existing DOC vs watershed-area exploratory plot.

## Files Added

- `R_scripts/08_exploratory_doc_geospatial_scatterplots.R`
- `Output_for_analysis/08_doc_geospatial_scatterplots/doc_geospatial_scatterplot_summary.csv`
- `Output_for_analysis/08_doc_geospatial_scatterplots/figures/`
- `.codex/doc_geospatial_scatterplot_changes.md`

## Implementation Notes

- Read the merged master CSV and the geospatial source CSV header.
- Built the candidate geospatial variable list from:
  - `latitude`
  - `longitude`
  - `Area_watershed_km`
  - columns from `geospatial_variables_bp_severity_pull.csv` that are present in
    `03_master_merged.csv`
- Coerced DOC and candidate geospatial variables to numeric where possible.
- Filtered each plot to complete rows with positive DOC so the log-scaled DOC
  axis renders correctly.
- Created one PNG per eligible variable.
- Colored points by `Burn_Unburn`.
- Used a log10 x axis only when all x values were positive and the variable
  spanned at least two orders of magnitude.
- Wrote a summary CSV with plotted/skipped status, row counts, site/study
  counts, x ranges, x-axis scale, and Spearman correlation results.

## Verification

Ran:

```sh
Rscript R_scripts/08_exploratory_doc_geospatial_scatterplots.R
```

The script reported:

```text
Plotted 131 DOC-geospatial scatterplots. Skipped 21 variables.
```

Confirmed:

```text
131 PNG files in Output_for_analysis/08_doc_geospatial_scatterplots/figures/
Output_for_analysis/08_doc_geospatial_scatterplots/figures/scatter_DOC_vs_Area_watershed_km.png: PNG image data, 1200 x 900
Output_for_analysis/08_doc_geospatial_scatterplots/figures/scatter_DOC_vs_burn_percent_fire_year.png: PNG image data, 1200 x 900
```
