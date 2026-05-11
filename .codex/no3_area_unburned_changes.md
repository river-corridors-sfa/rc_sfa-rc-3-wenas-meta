# Nitrate vs Watershed Area, Unburned Watersheds

Date: 2026-05-08

## Request

Create the same exploratory plot workflow as the DOC script, but for nitrate: use `Output_for_analysis/03_merge_geospatial/03_master_merged.csv` to plot nitrate on the y axis and watershed area on the x axis for unburned watersheds only.

## Files Added

- `R_scripts/06_exploratory_no3_area_unburned.R`
- `Output_for_analysis/06_no3_area_unburned/figures/scatter_NO3_vs_watershed_area_unburned.png`
- `.codex/no3_area_unburned_changes.md`

## Implementation Notes

- Read the merged master CSV with `readr::read_csv()`.
- Coerced `Area_watershed_km` and `NO3_Interp_mg_N_L` to numeric.
- Filtered rows to `Burn_Unburn == "Unburn"`.
- Dropped missing nitrate and watershed area values.
- Filtered to positive nitrate and watershed area values so log-scaled axes render correctly.
- Built a `ggplot2` scatter plot with:
  - `Area_watershed_km` on the x axis.
  - `NO3_Interp_mg_N_L` on the y axis.
  - Unburned observations only.
  - A linear model smooth.
  - Log10 x and y scales, matching the DOC exploratory script.

## Verification

Initial data check found:

```text
n complete positive: 11660
```

Run for final verification:

```sh
Rscript R_scripts/06_exploratory_no3_area_unburned.R
```

The script reported:

```text
Unburned NO3-area data: 11660 rows, 25 sites, 14 studies.
```

Confirmed the plot was written as a PNG:

```text
Output_for_analysis/06_no3_area_unburned/figures/scatter_NO3_vs_watershed_area_unburned.png
```
