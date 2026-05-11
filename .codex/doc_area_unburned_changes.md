# DOC vs Watershed Area, Unburned Watersheds

Date: 2026-05-08

## Request

Create a new exploratory R script using `Output_for_analysis/03_merge_geospatial/03_master_merged.csv` to plot DOC on the y axis and watershed area on the x axis for unburned watersheds only.

## Files Added

- `R_scripts/05_exploratory_doc_area_unburned.R`
- `Output_for_analysis/05_doc_area_unburned/figures/scatter_DOC_vs_watershed_area_unburned.png`
- `.codex/doc_area_unburned_changes.md`

## Implementation Notes

- Read the merged master CSV with `readr::read_csv()`.
- Coerced `Area_watershed_km` and `DOC_Interp_mg_C_L` to numeric.
- Filtered rows to `Burn_Unburn == "Unburn"`.
- Dropped missing DOC and watershed area values.
- Filtered to positive DOC and watershed area values so log-scaled axes render correctly.
- Built a `ggplot2` scatter plot with:
  - `Area_watershed_km` on the x axis.
  - `DOC_Interp_mg_C_L` on the y axis.
  - Unburned observations only.
  - A linear model smooth.
  - Log10 x and y scales, following the existing exploratory plotting style for watershed area and DOC.

## Verification

Ran:

```sh
Rscript R_scripts/05_exploratory_doc_area_unburned.R
```

The script reported:

```text
Unburned DOC-area data: 6073 rows, 18 sites, 9 studies.
```

Confirmed the plot was written as a PNG:

```text
Output_for_analysis/05_doc_area_unburned/figures/scatter_DOC_vs_watershed_area_unburned.png
PNG image data, 1200 x 900, 8-bit/color RGB, non-interlaced
```

## Cleanup

- Removed the unwanted `Rplots.pdf` created by the first batch verification run.
- Updated the script so it only prints the ggplot object in interactive sessions.
