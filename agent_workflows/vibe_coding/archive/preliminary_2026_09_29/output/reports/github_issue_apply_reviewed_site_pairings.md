# Apply workbook decisions to named sites before rebuilding paired effect sizes

## Problem

The reviewed pairing workbook specifies which physical sites belong in each comparison. Filtering on `Pair_Burn` and `Pair_Unburn` alone does not fully implement those decisions: several reference IDs still contain multiple named sites, although reviewers selected one reference.

The corrected gap-filling output also retains three unlinked Mast & Clow sites that the workbook notes place outside the selected comparison. These records should remain available for provenance but must be explicitly excluded from the paired analysis dataset.

Source of decisions: `agent_workflows/vibe_coding/config/pairing_review_workbook.xlsx`, **Pairing Review** sheet, including `Decision_Notes`, `Include`, and `Decision_Status`.

## Required named-reference corrections

| Study | Burned site / pair | Reference pair ID | Workbook-selected reference | Other sites currently under that reference ID |
|---|---|---|---|---|
| Rhea et al. 2021 | B1 / `Site_1` | `Control_1` | U1 | U2 |
| Crandall et al. 2021 | Benjamin Slough / `Site_1` | `Control_1` | Hobble Creek Lower | Hobble Creek Upper |
| Crandall et al. 2021 | Payson / `Site_2` | `Control_2` | Mill Race | Dry Creek; Mitsubishi Race |

Restrict each comparison to its selected named reference before daily pairing or aggregation. Preserve the other site records in upstream/provenance data. Do not average or create additional contrasts from references that the workbook did not select.

Other retained comparisons in these studies remain as reviewed: Rhea B2 and B3 each compare with U3; Crandall Spanish Fork Lower compares with Provo River.

## Mast & Clow: explicitly classify three sites outside the selected analysis

The workbook decision for Mast & Clow; 2008 states that only **Coal Creek (burned) versus Pinchot Creek (unburned)** is selected, associated with the Rampage Fire of 2003. The source CSV uses `Coal` and `Pinchot` as the site names and `Site_1` and `Control_1` as their pair labels.

These additional source sites are outside that selected comparison:

| Site | Source pair label | Extracted source rows | Required treatment |
|---|---|---:|---|
| Fish Creek | `Site_2_post` | 43 | Retain upstream records; exclude from paired effect-size/model inputs |
| Lower MacDonald Creek | `Site_3_post` | 44 | Retain upstream records; exclude from paired effect-size/model inputs |
| Upper MacDonald Creek | `Site_4_post` | 41 | Retain upstream records; exclude from paired effect-size/model inputs |

These are not three explicitly excluded rows in the 36-row workbook inventory. Their treatment follows the approved Mast & Clow row's notes selecting only Coal–Pinchot. Record that provenance accurately rather than claiming that the workbook contains individual exclusion decisions for them.

The inherited comparison mapping leaves their `Comparison_ID` missing because no matching `Control_2`, `Control_3`, or `Control_4` exists. Removing `_post` would not supply a valid reference. Do not assign them to Pinchot automatically. Classify these known records as outside the reviewed analysis, while continuing to flag any unexpected unlinked sites separately.

## Preserve the eight explicit workbook exclusions

- Crandall et al. 2021: `Site_4` (Spanish Fork Upper).
- Oliver et al. 2012: `Site_1` and `Site_2` (source site names `Site_2` and `Site_3`); retain reviewed pair `Site_3`, whose physical site is `Site_4`.
- Wagner et al. 2015: `Site_1` and `Site_2`.
- Burd et al 2018: `Site_1`.
- Neary & Currier; 1982: `Site_1`.
- Tiedemann; 1973: `Site_1`.

Preserve legitimate shared-reference comparisons and their dependence identifiers. Do not remove an entire reference site globally if it serves another approved comparison.

## Implementation completed September 22, 2026

**Main script: `agent_workflows/vibe_coding/R/01c_apply_reviewed_sites_agent_v1.R`**

Run from the repository root:

```r
source("agent_workflows/vibe_coding/R/01_read_and_gapfill_preserve_observations_agent_v1.R")
source("agent_workflows/vibe_coding/R/01c_apply_reviewed_sites_agent_v1.R")
source("agent_workflows/vibe_coding/R/02_prepare_analysis_data_agent_v1.R")
source("agent_workflows/vibe_coding/R/03_audit_pairs_and_predictors_agent_v1.R")
source("agent_workflows/vibe_coding/R/check_reviewed_sites_agent_v1.R")
```

Script 01c refreshes the strict workbook import, writes `config/reviewed_site_selection.csv`,
filters named-site memberships, and rebuilds daily and annual effects under
`data/derived/reviewed_sites_agent_v1/`. The generated configuration records workbook
notes/evidence and explicit author exclusions; edit the documented rules in script
01c when decisions change rather than editing its generated CSV. Unexpected unmapped
sites and ambiguous retained roles stop processing. Uzun's three seasonal labels per
role are retained; date-level one-to-one checks prevent additional contrasts.

Both full runners now regenerate corrected chemistry and reviewed-site effects before
script 02. Script 02 uses these corrected outputs and retains observed/interpolated
counts. The original source snapshots and human scripts remain unchanged.

The author confirmed on September 22 that the other Mast & Clow reference sites are
fire-impacted and other burned sites involve multiple fires and/or a large upstream
lake. The three named exclusions record this collective rationale without assigning
an unverified individual mechanism to each site.

Validation passed: all 28 approved pairs remain; the eight excluded pairs and three
Mast & Clow sites contribute no paired effects; the three reference restrictions hold.
The rebuilt model table has 37 DOC rows (35 usable variances) and 73 nitrate rows
(69 usable variances). Four newly recovered Murphy 2010 summaries are singletons.
Writer PNF 2013 now has n=2 and variance 0.372841. Model fits and figures were regenerated in the full September 22 run with 1000 bootstrap iterations per analyte/scenario; see `corrected_run_2026-09-22.md`.

Audits: `reviewed_site_selection_summary.csv`, `reviewed_site_role_coverage.csv`,
`reviewed_chemistry_coverage.csv`, `reviewed_pairs_without_valid_chemistry.csv`, and
`reviewed_effect_size_changes.csv` under `data/audit/`. The last file records every
before/after annual mean, count, variance, and numerical meta-analysis eligibility flag.

## Implementation requirements

1. Create a machine-readable named-site selection configuration under `agent_workflows/vibe_coding/config/`, keyed by study, comparison, pair, role, and physical site, with workbook evidence and exclusion reasons.
2. Apply that configuration to the corrected daily chemistry before building burned/reference contrasts. Keep a separate audit of retained, excluded, and unexpected/unresolved site memberships.
3. Preserve immutable inputs and human-written scripts. Implement changes in agent-versioned scripts and write regenerated data under `agent_workflows/vibe_coding/`.
4. Rebuild annual effect sizes, model inputs, and audits from the corrected and site-filtered daily data. Report changes relative to the current provisional annual source.

Related interpolation correction script:
`agent_workflows/vibe_coding/R/01_read_and_gapfill_preserve_observations_agent_v1.R`.

Its current output is:
`agent_workflows/vibe_coding/data/derived/gapfill_corrected_agent_v1/01_daily_time_series_paired.csv`.

That script preserves the inherited comparison mapping; running it alone does not implement the named-reference decisions described here.

## Acceptance criteria

- [x] Rhea B1 uses U1 only; U2 contributes no values to that contrast.
- [x] Crandall Benjamin Slough uses Hobble Creek Lower only.
- [x] Crandall Payson uses Mill Race only.
- [x] Mast & Clow Coal–Pinchot remains included, while Fish Creek and both MacDonald sites are explicitly documented outside the reviewed analysis.
- [x] Known Mast & Clow omissions are distinguished from unexpected missing comparison links.
- [x] All eight explicit workbook exclusions remain absent from paired analysis outputs.
- [x] Every analyzed contrast has the reviewed named sites; no additional cross-products or unintended reference pooling are created.
- [x] Coverage is checked for all 28 approved pairs; any absence of valid positive matched chemistry is reported rather than concealed.
- [x] Before/after counts, annual means, variances, and model eligibility are audited, retaining observed/interpolated flags and shared-reference IDs.

The completed named-site and chemistry checks verify all 28 approved pairs, the selected physical references, and valid paired chemistry for each included pair. No additional pair-ID combinations were introduced.
