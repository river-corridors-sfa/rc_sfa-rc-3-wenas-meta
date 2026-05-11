# Concentration Ratio vs Watershed Area Ratio

Date: 2026-05-08

## Request

Plot concentration ratio versus watershed area ratio, with one plot per solute.

## Files Added

- `R_scripts/07_exploratory_concentration_area_ratios.R`
- `Output_for_analysis/07_concentration_area_ratios/concentration_area_ratio_data.csv`
- `Output_for_analysis/07_concentration_area_ratios/figures/scatter_DOC_concentration_ratio_vs_watershed_area_ratio.png`
- `Output_for_analysis/07_concentration_area_ratios/figures/scatter_NO3_concentration_ratio_vs_watershed_area_ratio.png`
- `.codex/concentration_area_ratio_changes.md`

## Implementation Notes

- Read the merged master CSV from `Output_for_analysis/03_merge_geospatial/03_master_merged.csv`.
- Used `Comparison_ID` to pair burned and unburned watersheds.
- Calculated ratios by `Study_ID`, `Comparison_ID`, and `Sampling_Date`.
- Calculated concentration ratio as:

```text
burned concentration / unburned concentration
```

- Calculated watershed area ratio as:

```text
burned watershed area / unburned watershed area
```

- Filtered out rows with missing, zero, negative, or non-finite concentrations or watershed areas so log-scaled ratio axes render correctly.
- Saved one plot for DOC and one plot for nitrate.
- Saved the underlying paired ratio data as `concentration_area_ratio_data.csv`.

## Verification

Run for final verification:

```sh
Rscript R_scripts/07_exploratory_concentration_area_ratios.R
```

The script reported:

```text
# A tibble: 2 x 4
  solute n_ratio_observations n_comparisons n_studies
  DOC                    5555            20         9
  NO3                    9587            33        14
```

Confirmed outputs:

```text
Output_for_analysis/07_concentration_area_ratios/concentration_area_ratio_data.csv
Output_for_analysis/07_concentration_area_ratios/figures/scatter_DOC_concentration_ratio_vs_watershed_area_ratio.png
Output_for_analysis/07_concentration_area_ratios/figures/scatter_NO3_concentration_ratio_vs_watershed_area_ratio.png
```

Both plots are PNG images at `1200 x 900`.
