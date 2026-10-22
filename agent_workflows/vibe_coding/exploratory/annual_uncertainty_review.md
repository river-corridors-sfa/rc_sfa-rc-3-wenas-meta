# Annual uncertainty and dependence review

October 21, 2026. Primary manuscript selection is pending author review. The exploratory annual-uncertainty script adds review candidates without replacing annual responses, variances, prediction models, or conditional REML outputs.

The sampling audit does not support a validated annual sampling variance for every record. A defensible alternative for the main synthesis is equal-study, equal-pair, equal-available-year estimation with cluster-adjusted uncertainty, retaining variance-based multilevel models as conditional sensitivities. This changes the synthesis target and Methods specification; it does not validate individual annual variances.

## Annual response and sampling uncertainty

Retain the selected log ratio of arithmetic concentration means on eligible matched positive dates in each calendar year, the 104 paper records, and the documented six singleton exclusions. These are available-coverage annual summaries, not necessarily complete-year or flow-weighted means. The median matched observed-date count is six for DOC and five for nitrate. Only 18 records, all Murphy, pass the exploratory temporal-resampling screen, still with maximum gaps of 42–57 days.

Do not treat interpolated dates as independent measurements, assign automatic block-bootstrap variances, or replace daily n with observed n without a sampling model. The daily paired delta variances remain working quantities. Observed-only delta variances also assume date independence; removing interpolation does not resolve temporal dependence. No universal effective sample size or correlation correction is supported by this audit.

## Candidate synthesis and dependence assumptions

Assign each record weight `1 / (retained pairs in study × retained years in pair)`. Each study receives equal total weight, each pair equal weight within study, and each available year equal weight within pair. The pooled intercept equals the average of study means of pair means of annual log ratios. Its exponent is a geometric synthesis of annual ratios, not a ratio of globally pooled concentrations. The time regression describes a weighted association across included records, not an identified within-pair recovery trajectory.

Fit weighted least squares with CR2 coefficient uncertainty and Satterthwaite degrees of freedom. Specify `inverse_var=FALSE` because these are balancing weights, and an identity working target for the CR2 adjustment. This target is not a claim of within-cluster independence. Cluster-robust uncertainty permits within-cluster dependence but still relies on independent clusters, adequate information, and a suitable mean model. It does not reconstruct unsampled periods or remove response bias. See the [clubSandwich weighted model documentation](https://jepusto.github.io/clubSandwich/reference/vcovCR.lm.html) and [interval documentation](https://jepusto.github.io/clubSandwich/reference/conf_int.html).

Compare publication clusters with connected study/fire clusters: join publications sharing any retained fire, keeping all fires within each publication together. Rhea and Writer connect through High Park, giving six DOC and eleven nitrate components versus seven and twelve publications. This allows cross-publication dependence without asserting duplicate observations. Fire-only grouping would split multi-fire studies. Component independence remains an assumption: review shared physical sites, nested watersheds, overlapping measurements, and other cross-publication links before freezing it. Fire IDs inherit the remaining attribution checks.

Shared references and repeated years stay together within components. Reference-family weighting is a separate representation choice; it is not a covariance correction. The candidate target lets a family with more approved burned pairs contribute more within its study. A controlled family-balancing sensitivity remains outstanding. LASSO validation and bootstrap groups remain publication-based; this review has not changed them.

## Review results

These are candidate 95% CR2/Satterthwaite intervals using connected study/fire clusters, conditional on the stated target and grouping.

| Inventory and response | DOC pooled log ratio | Nitrate pooled log ratio |
|---|---:|---:|
| Primary, 104 records | 0.295 [−0.098, 0.689] | 0.978 [0.311, 1.645] |
| Observed only, 89 records | 0.291 [−0.137, 0.718] | 1.037 [0.337, 1.736] |
| Interpolated, same 89 records | 0.265 [−0.115, 0.646] | 1.067 [0.426, 1.709] |

Observed-only versus matched-interpolated compares response construction on identical records. Comparisons with the full inventory also change coverage and balancing weights. Nitrate's interval stays above zero in these three pooled summaries; DOC's includes zero. This supports that limited synthesis interpretation, not validated annual precision.

Primary-inventory time slopes are −0.025 log units/year for DOC [−0.193, 0.143], with 1.30 effective degrees of freedom, and 0.125 for nitrate [−0.386, 0.636], with 1.36 degrees of freedom. Existing precision-based conditional REML slopes span 0.005–0.017 for DOC and −0.051 to −0.020 for nitrate across nine covariance scenarios; their intervals also include zero. The signs reverse relative to the balanced candidates. Different weighting and mean-model assumptions matter more than the covariance grid alone reveals; do not claim an established recovery trend.

Deleting one study/fire component at a time gives pooled point estimates of 0.206–0.399 for DOC and 0.809–1.170 for nitrate. Time slopes span −0.082 to −0.014 and 0.088–0.250, respectively. High Park's component has the largest slope influence for both analytes. These are point-estimate deletion checks, not deletion-specific confidence intervals or influence validation for REML or LASSO.

## Recommended choice pending author selection

Use balanced pooled summaries with study/fire-component CR2 intervals as the main synthesis; retain publication-cluster intervals as sensitivity results. Treat DOC and all temporal associations cautiously given the effective degrees of freedom. Keep REML study/pair random-intercept models and their nine provisional annual covariance scenarios as conditional sensitivities. Do not attribute their heterogeneity components to the balanced analysis. Retain observed-only and matched-interpolated comparisons and revise Methods to distinguish balancing, coefficient uncertainty, and annual precision.

This route specifies synthesis uncertainty without assigning unsupported annual variances. If REML remains primary, its inference must explicitly remain conditional on the selected variance/covariance model. Neither route supplies exact annual sampling uncertainty or guarantees small-sample interval coverage. Final selection changes the Methods' promised primary multilevel inference and requires an explicit author choice.

Before manuscript freeze, confirm the inference target, review cross-publication links and time attribution, and decide whether prediction also needs study/fire-component validation. Do not mark annual-variance validation complete because a candidate synthesis runs.

## Reproduction

Run `Rscript agent_workflows/vibe_coding/exploratory/R/01_review_annual_uncertainty_agent_v1.R` from the repository root after model-table preparation and sampling-support audit produce the primary and observed-only inputs. This review remains separate from the paper runner pending method selection.

`exploratory/output/annual_uncertainty/` contains coefficients, intervals, effective degrees of freedom, study/fire membership, weights, leave-component-out estimates, and session information. Checks verify equal study totals and pair totals within study, unique primary annual keys, finite inputs/uncertainty, positive degrees of freedom, and pooled-intercept agreement with independently aggregated study/pair means.
