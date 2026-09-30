# Reviewed pairing preparation — September 18, 2026

Strict workbook import, analysis promotion, and scripts 02–03 completed successfully.
The Excel workbook was read without modification. Validation found zero issues;
36 original candidate identities were preserved: 28 included and 8 excluded.

| Analyte | Annual rows | Studies | Pairs | Usable variances |
|---|---:|---:|---:|---:|
| DOC | 35 | 7 | 16 | 35 |
| NO3 | 71 | 12 | 28 | 68 |

Checks passed for complete included-pair coverage, absence of excluded pairs,
confirmed status on every model row, unique pair/analyte/calendar-year and
pair/analyte/post-fire-year rows, nonmissing burned-site joins, positive family
weights, and exact preservation of annual lnRR values from the established source.
No annual calendar years precede the fire year encoded in the confirmed fire ID.
Post-fire years range from 0 to 15; this plausibility check does not validate their
scientific definition or the upstream burn predictors against the reviewed fires.

Shared-control IDs are preserved. Family counts and weights now reflect included
pairs; original inventory counts remain in the analysis configuration for provenance.

## What “usable variance” means

The preparation script labels `lnRR_var` usable when it is **finite and greater
than zero**. Missing values, infinite values, zero, and negative values fail this
numerical check. A usable value is not proof that the uncertainty estimate is
scientifically adequate: interpolation, repeated observations, and shared
references still need to be addressed.

The established calculation in `R_scripts/04_calculate_effect_sizes.R` first
computes each paired daily log response ratio, then summarizes it by year:

```text
daily lnRR = log(burned concentration / reference concentration)
lnRR_mean = mean of the available daily lnRR values
lnRR_sd   = sample standard deviation of those values
lnRR_n    = number of nonmissing daily lnRR values
lnRR_var  = lnRR_sd² / lnRR_n
```

Thus, `lnRR_var` is the estimated variance of the annual mean log response ratio,
not the variance of raw concentrations or the between-study heterogeneity.
Its corresponding standard error is `sqrt(lnRR_var)`. Script 02 imports this
quantity from the established annual table; the reviewed-pair rebuild did not
recalculate it from daily chemistry.

### Why three nitrate rows fail

All three failures are **missing variances**, rather than zero or negative
variances. Each has `lnRR_n = 1`, so the sample standard deviation and therefore
`lnRR_var` are undefined. The annual response itself is present and finite.

| Study | Burned pair / site | Reference ID | Year | Annual lnRR | n | SD / variance |
|---|---|---|---:|---:|---:|---|
| Writer et al. 2014 | Site_1 / PNF | Control_1_2 | 2012 | -0.318454 | 1 | Missing / missing |
| Writer et al. 2014 | Site_1 / PNF | Control_1_2 | 2013 | 2.159484 | 1 | Missing / missing |
| Writer et al. 2014 | Site_2 / PSF | Control_1_2 | 2012 | 0.000000 | 1 | Missing / missing |

The zero lnRR in the third row means the paired concentrations were equal; it
does not mean that the response was measured without uncertainty. These counts
describe the retained source table, not necessarily the total observations in
the publication. Establishing why only one usable paired value survived requires
tracing the original observations, matching, and filtering.

Writer Site_2 in 2013 remains eligible: it has 61 contributing values and
`lnRR_var = 0.000395169`. All four retained Writer DOC rows have usable variances.

### How the current scripts handle these rows

| Analysis | Current behavior | Consequence for this dataset |
|---|---|---|
| Multilevel meta-analysis (04) | Requires finite lnRR, finite positive variance, and `lnRR_n >= 2`. | 35 DOC rows across 7 studies / 16 pairs; 68 nitrate rows across 12 studies / 27 pairs are eligible before fitting. Writer remains represented, but its nitrate Site_1 pair is absent. |
| Grouped LASSO prediction (05) | Retains finite responses. During outer-fold training, missing precision is replaced with the median available training precision; precision is inverse variance, capped at the training 95th percentile, then multiplied by the reference-family weight and normalized. If all training precisions are missing, equal precision is used. | All 71 nitrate responses remain eligible for prediction. The three rows can receive assumed training weights; this does not estimate or fill their missing variances in the data table. |
| Bootstrap stability (06) | Uses reference-family weights or equal weights, depending on scenario; it does not use inverse-variance weights. | Missing variance alone does not remove the three rows. Stability results therefore use a different weighting rule from script 05. |

These are code-defined eligibility counts and behaviors, not new model results.
The differing row sets and weighting assumptions should be explicit when comparing
meta-analysis, predictive performance, and stability results.

### Source-data investigation: two sparse summaries and one processing error

Tracing `inputs/Studies/meta_final/Writer_et_al_2014.csv` against the daily source
snapshot reproduces all four current Writer nitrate annual summaries, including
their counts, means, and available variance. The missing variances have different causes:

| Annual summary | Positive same-date observed pairs in the source CSV | Finding |
|---|---|---|
| PNF / Site_1, 2012 | 1: November 5 | Insufficient paired source data. Earlier PNF measurements precede reference coverage; PNF nitrate is missing on August 15 and September 10, and October 15 is missing at both sites. |
| PSF / Site_2, 2012 | 1: November 5 | Insufficient paired source data. PSF nitrate is missing on August 15, September 10, and October 15. |
| PNF / Site_1, 2013 | 2: February 4 and June 1 | A valid February pair was removed by gap masking, creating the singleton summary. |

The source CSV has PNF nitrate = 0.23 and reference PBR nitrate = 0.09 on
February 4, 2013 (original `mg_L` units). Both values become missing in the
processed daily snapshot. The human-written `01_read_and_gapfill.R` sets
`Large_Gap_Flag` when the previous sampling date is more than 40 days earlier,
then applies `ifelse(Large_Gap_Flag, NA_real_, interpolated_value)` to every row.
This masks measured endpoints as well as dates that would require interpolation.
February 4 follows a 91-day gap; the code therefore removes actual observations
that should remain available even when interpolation across that gap is prohibited.

The same mechanism removes April 1 measurements after a 56-day gap at PNF, PSF,
and PBR. However, PBR's original April nitrate is zero, so that date cannot yield
a finite log response ratio even after restoring the measurements. PNF has no
May 4 nitrate measurement. These details explain why restoring observations
recovers exactly one additional valid PNF pair in 2013.

In a diagnostic copy that preserves recorded observations without filling any
additional gaps, PNF 2013 changes from `n = 1` to `n = 2`, annual lnRR changes
from **2.159484 to 1.548877**, and variance becomes **0.372841** using the existing
`SD² / n` formula. This is numerically estimable but supported by only two dates.
The two 2012 variances remain missing.

The error also affects PSF 2013, whose variance was already numerically usable:
restoring February changes its daily count from 61 to 62. Its source contains
only **three positive observed pairs**, illustrating why daily interpolated counts
must not be interpreted as independent sample counts. An observed-only calculation
and an interpolated daily calculation target different temporal summaries; the
diagnostic values are not approved replacement estimates.

**Conclusion:** two missing variances reflect sparse paired data in the repository's
extracted source; the third is caused by an upstream processing error. This audit
does not establish whether the original publication contains additional data
absent from the extracted CSV. The source notes say values were manually extracted
from Table 1; the publication itself was not reviewed in this audit.

Reproduce with `python3 agent_workflows/vibe_coding/R/audit_writer_variance_agent_v1.py`.
Evidence is saved in `data/audit/writer_nitrate_observation_trace.csv` and
`data/audit/writer_nitrate_variance_diagnostic.csv`. Only diagnostic files were
created; human scripts, immutable snapshots, and model inputs were not changed.

The next corrective step is an agent-versioned upstream processing script that
preserves observed values while restricting gap filling to eligible missing
values, followed by a cross-study audit and rebuild of downstream effect sizes.
Because the masking function applies across sites and analytes, its scope may
extend beyond these three summaries.

### Remaining variance decisions

- Trace the three singleton summaries back to the paired chemistry to distinguish
  genuinely sparse sampling from missing matches or filtering losses. Do not
  substitute zero variance or an arbitrary small number.
- Decide whether to retain the current meta-analysis exclusion and predictive
  weight fallback, or implement a justified alternative with sensitivity checks.
  No variance imputation or new exclusion decision was made in this preparation run.
- Review whether `lnRR_n` reflects independent information. The `SD² / n` formula
  does not itself adjust for daily interpolation or temporal autocorrelation;
  many contributing daily values need not imply many independent samples.
- Shared-reference dependence is a separate issue. Script 04's family sensitivity
  multiplies variance by the retained reference-family pair count; it does not
  recover missing variances or construct an exact covariance matrix.

## Pre-model review checkpoint

- Three nitrate rows lack usable variances: Writer et al. 2014, Site_1 in 2012 and
  2013, and Site_2 in 2012. Resolve variance/weighting treatment before final inference.
- Burned watershed area and high-severity burn have Spearman rho = 0.932. Both
  remain in the provisional candidate set; finalize redundancy handling before models.
- Decide shared-reference dependence treatment, interpolation/autocorrelation
  handling, final post-fire-year definition, and the inference scope of seven DOC studies.
- Review workbook evidence against upstream site/reference aggregation and fire
  predictors where the reviewed interpretation changes; this run filters established
  annual responses and does not rebuild chemistry or extract new GIS attributes.
- The August 20 nitrate time-model convergence failure still requires investigation
  during the next model run.

Models, model-result tables, and figures were not regenerated. Those saved outputs
still describe the August 20 provisional dataset and must not be combined with
the refreshed audits as final results. The 1000-iteration stability run remains pending.

## Reproduction

From the repository root, source these R scripts in order:

1. `agent_workflows/vibe_coding/R/01b_promote_reviewed_pairings_agent_v1.R`
2. `agent_workflows/vibe_coding/R/02_prepare_analysis_data_agent_v1.R`
3. `agent_workflows/vibe_coding/R/03_audit_pairs_and_predictors_agent_v1.R`

The promotion script performs the strict import itself. Both full runners now
start with promotion, not provisional regeneration. Review the checkpoint before
using either full runner to fit models.

Console output and R/package information are in `output/logs/reviewed_preparation_2026-09-18.log`
and `output/logs/reviewed_preparation_session_info_2026-09-18.txt`.
