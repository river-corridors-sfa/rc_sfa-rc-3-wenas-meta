# Preserve measured concentrations when blocking interpolation across long gaps

## Problem

`R_scripts/01_read_and_gapfill.R` removes actual DOC and nitrate observations when they follow a sampling gap longer than 40 days. This changes annual log response ratios and can create missing variances by reducing a summary to one contributing value.

The script assigns `Large_Gap_Flag = Days_Between_Samples > 40` to the later sampling date, propagates flags through the daily grid, and applies:

```r
ifelse(
  Large_Gap_Flag,
  NA_real_,
  zoo::na.approx(.data[[analyte]], na.rm = FALSE, maxgap = 40)
)
```

The outer `ifelse` masks measured endpoints as well as missing dates. The gap rule should restrict interpolation while preserving recorded observations.

## Reproducing the error

Compare `inputs/Studies/meta_final/Writer_et_al_2014.csv` with `agent_workflows/vibe_coding/data/source/01_daily_time_series_paired.csv`.

On February 4, 2013, the source contains nitrate measurements of 0.09 at PBR, 0.23 at PNF, and 0.16 at PSF (original mg/L units). The previous sampling date is November 5, 2012, a 91-day gap. All three measurements become missing in the processed nitrate column.

For PNF / Site_1 in 2013, removing February leaves only June 1. A diagnostic restoration of recorded observations, without additional interpolation, gives:

| Quantity | Current | Diagnostic restoration |
|---|---:|---:|
| Number of paired values | 1 | 2 |
| Annual mean lnRR | 2.159484 | 1.548877 |
| Variance, SD²/n | Missing | 0.372841 |

April 1 observations are also masked after a 56-day gap. The reference nitrate that day is zero, so restoring it does not create a valid log ratio. PSF 2013 is affected even though its existing variance is finite.

The missing PNF and PSF variances in 2012 have a different cause: each has only one positive same-date observed pair in the extracted source CSV. This fix should not manufacture extra observations for those summaries.

Local diagnostic artifacts from the investigation (include with the fix if not yet committed):

- `agent_workflows/vibe_coding/R/audit_writer_variance_agent_v1.py`
- `agent_workflows/vibe_coding/data/audit/writer_nitrate_observation_trace.csv`
- `agent_workflows/vibe_coding/data/audit/writer_nitrate_variance_diagnostic.csv`

The diagnosis concerns repository-extracted observations; the original paper has not been independently audited.

## Proposed correction

### Correction script created September 22, 2026

**Script: `agent_workflows/vibe_coding/R/01_read_and_gapfill_preserve_observations_agent_v1.R`**

Run from the repository root:

```sh
Rscript agent_workflows/vibe_coding/R/01_read_and_gapfill_preserve_observations_agent_v1.R
```

The script adapts the original upstream workflow, preserving its unit conversions,
study exclusion, and comparison mapping. It preserves observed endpoints, checks
the 40-day threshold per analyte, and exports observation flags for DOC and nitrate.
Outputs are isolated under
`agent_workflows/vibe_coding/data/derived/gapfill_corrected_agent_v1/`.
The primary output is `01_daily_time_series_paired.csv`; unit and missing-comparison
audits are saved alongside it. Source snapshots and active model inputs are unchanged.

Execution succeeded across the input study files. Checks passed for preserved
observations, 40/41-day boundaries, long gaps, singleton/all-missing series, and
restoration of all three Writer sites on February 4, 2013. The retained comparison
mapping reports three unlinked Mast & Clow burned sites (`Site_2_post`, `Site_3_post`,
`Site_4_post`); inspect the exported audit before downstream use. Annual effect
sizes and models have not yet been rebuilt from this output.

Use a new agent-versioned upstream script under `agent_workflows/vibe_coding/`, preserving the human-written script and source snapshots. Keep observed concentrations, including zero, and interpolate only between valid observations for the specific analyte. Do not extrapolate.

The following replacement retains the existing function name for call-site compatibility but does not use `Large_Gap_Flag`:

```r
interpolate_values_based_on_flag <- function(data, analyte,
                                            max_gap_days = 40) {
  data <- data %>% arrange(Sampling_Date)
  dates <- as.numeric(as.Date(data$Sampling_Date))
  values <- data[[analyte]]
  observed <- which(!is.na(values))
  result <- values

  if (anyNA(dates) || anyDuplicated(dates)) {
    stop("Each site/pair must have unique, nonmissing sampling dates.")
  }
  if (any(!is.finite(values[observed]))) {
    stop("Observed concentrations must be finite.")
  }

  if (length(observed) >= 2) {
    for (i in seq_len(length(observed) - 1L)) {
      left <- observed[i]
      right <- observed[i + 1L]
      gap_days <- dates[right] - dates[left]

      if (gap_days <= max_gap_days) {
        fill_rows <- which(
          is.na(values) &
            dates > dates[left] & dates < dates[right]
        )
        if (length(fill_rows) > 0) {
          result[fill_rows] <- approx(
            x = dates[c(left, right)],
            y = values[c(left, right)],
            xout = dates[fill_rows]
          )$y
        }
      }
    }
  }

  data[[paste0(analyte, "_Interp")]] <- result
  data
}
```

Apply within the existing study/site/pair groups after unit conversion and missing-sentinel handling. The proposed threshold means **at most 40 elapsed calendar days between observed endpoints**, which should be explicitly confirmed/documented; it differs from counting 40 missing daily rows. DOC and nitrate must have independent eligible intervals.

## Acceptance checks

- [ ] Recorded values are unchanged after processing, including observations after long gaps and zeros.
- [ ] Intervals of 40 days permit interpolation; intervals of 41 days do not.
- [ ] No extrapolation occurs; groups with zero or one observation retain their observations and otherwise remain missing.
- [ ] Missingness in one analyte does not borrow sampling coverage from another.
- [ ] Writer February 4 observations survive; PNF 2013 regains the second valid pair.
- [ ] The two 2012 Writer singleton summaries remain correctly identified as insufficient for variance estimation unless additional source data are recovered.
- [ ] Audit all studies and both analytes for removed endpoints, not just Writer.
- [ ] Regenerate daily chemistry and downstream annual effect sizes into versioned agent outputs, then rebuild the reviewed model table and audits.
- [ ] Report before/after counts, effect sizes, variances, and model eligibility while preserving reviewed pairing exclusions.

This correction restores observations. It does not resolve temporal autocorrelation, the effective sample size of interpolated data, or shared-reference covariance; those remain separate analysis decisions.
