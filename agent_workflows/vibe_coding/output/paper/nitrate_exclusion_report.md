# Nitrate annual-record exclusion — October 22, 2026

The author requested exclusion of the very low nitrate response after inspecting the response distribution. Applied to the exact annual key below across primary synthesis, LASSO, all four weighting/interpolation sensitivity scenarios, conditional REML inference and all manuscript figures. No general lnRR cutoff was introduced.

- Publication: Crandall et al. 2021.
- Comparison: Crandall_et_al_2021_C2; burned Site_2, reference Control_2.
- Analyte/year: nitrate (NO3), 2018.
- Excluded lnRR: −4.506808658089654, based on 18 matched eligible dates.
- Annual burned/reference arithmetic means: 0.10410301875 / 9.435077122444444 in the source concentration units.
- Basis: author-directed post hoc exclusion of an extreme response; no measurement or transcription error has been confirmed. State the outcome-based selection transparently in Methods and discuss the inclusion comparison when interpreting the result.

The rule is in `config/paper_record_exclusions.csv` and the complete excluded annual record in `data/audit/paper_record_exclusions.csv` (paths relative to the workflow root). All 110 upstream annual effects and original chemistry remain intact. The full prior 104-record manuscript package and its primary/observed-only/matched model tables are preserved in `archive/pre_nitrate_exclusion_2026_10_22/`, with a verified SHA-256 manifest.

## Inventory

The current analysis has 103 records (35 DOC, 68 nitrate) after six singleton exclusions and this one author-selected outlier exclusion. The observed-only/matched-interpolated inventories have 88 records (32 DOC, 56 nitrate). Overall coverage remains 12 publications, 14 fires, 28 pairs and 46 watersheds because the pair's DOC counterpart remains. Nitrate coverage is now 27 pairs/44 watersheds (27 burned, 17 reference). Study/fire inference retains six DOC and eleven nitrate components.

## Primary pooled synthesis: before and after

Equal-study → equal-pair → equal-available-year weights are recalculated after exclusion. In Crandall's nitrate study, the two remaining pairs now split its total study weight equally. CR2 intervals use connected study/fire components and Satterthwaite degrees of freedom.

| Analyte | Before: pooled lnRR [95% CR2 interval] | After: pooled lnRR [95% CR2 interval] |
|---|---:|---:|
| Nitrate | 0.978 [0.311, 1.645] | 1.119 [0.653, 1.585] |
| DOC | 0.295 [-0.098, 0.689] | 0.295 [-0.098, 0.689] |

The nitrate interval is above zero with and without exclusion. Its estimate increases and interval narrows; DOC pooled results are unchanged. Time-association intervals still include zero, with primary slope effective df about 1.3–1.4. The exclusion does not establish a recovery trajectory.

## Held-out LASSO performance

Equal-study RMSE, comparing the archived inclusion analysis with the rerun exclusion analysis:

| Scenario | DOC before | DOC after | Nitrate before | Nitrate after |
|---|---:|---:|---:|---:|
| Primary | 0.279 | 0.279 | 1.293 | 0.867 |
| Equal row | 0.259 | 0.259 | 1.287 | 0.863 |
| Precision/family | 0.418 | 0.418 | 1.244 | 0.838 |
| Observed only | 0.330 | 0.330 | 1.350 | 0.921 |
| Matched interpolated | 0.191 | 0.191 | 1.273 | 0.820 |

These before/after scores use different retained validation responses; the lower errors partly reflect removal of the difficult observation. They do not establish improved prediction for the original full-inventory target. Primary nitrate LASSO held-out predictions still match the intercept-only benchmark. Primary nitrate predictor selection frequencies remain low (all below 4%); do not infer predictive skill from them.

## Execution and verification

Rebuilt preparation scripts 05–06 from retained corrected chemistry and annual effects, then ran `N_BOOTSTRAP=1000 Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R all`. This did not repeat upstream interpolation/import. All scenarios completed 994 DOC and 1,000 nitrate bootstrap refits; six DOC draws failed because fewer than three distinct studies were available. All 36 conditional REML fits completed. The annual-estimand, weighting/support and new exclusion-propagation checks passed; all 104 pre-exclusion archived artifacts match their saved hashes. All five figures were regenerated and Figures 2 and 3 visually inspected. The SI correlation caption now takes its row count from the data.

Logs: `output/logs/nitrate_exclusion_preparation_2026_10_22.log`, `output/logs/nitrate_exclusion_full_run_2026_10_22.log` and `output/logs/nitrate_exclusion_weighting_checks_2026_10_22.log`. Re-run `R/checks/05_check_paper_exclusions_agent_v1.py` after a manuscript build to verify that scenario predictions, synthesis, weights and Figure 2 share the correct excluded-record-free inventories.
