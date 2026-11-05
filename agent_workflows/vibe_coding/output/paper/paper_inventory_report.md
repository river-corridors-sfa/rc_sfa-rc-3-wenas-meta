# Verified paper study inventory

Updated October 22, 2026 after the author-directed nitrate exclusion. The paper analysis includes **12 publications, 14 unique fires, 28 burned–reference pairs and 46 study-specific physical watersheds (28 burned, 18 reference)**. It contains **103 annual responses: 35 DOC from seven studies and 68 nitrate from 12 studies**. DOC spans 16 pairs, 27 watersheds (16 burned, 11 reference) and nine fires. Nitrate spans 27 pairs, 44 watersheds (27 burned, 17 reference) and 14 fires. The excluded nitrate pair remains represented for DOC.

Suggested Methods wording:

> The paired analysis included 12 publications representing 14 unique wildfires and 28 burned–reference watershed pairs. Six annual pair–analyte records were excluded for having only one eligible matched date. Following inspection of the response distribution, one additional nitrate record (Crandall et al. 2021, Site_2–Control_2, 2018; lnRR −4.507) was excluded by author decision as an extreme response. This post hoc exclusion was not based on a confirmed measurement error. The retained analysis comprised 35 annual DOC responses and 68 annual nitrate responses.

## Study counts

| Publication | DOC rows | Nitrate rows | Pairs | Burned watersheds | Reference watersheds |
|---|---:|---:|---:|---:|---:|
| Coombs & Melack; 2013 | 0 | 6 | 3 | 3 | 1 |
| Crandall et al. 2021 | 3 | 2 | 3 | 3 | 3 |
| Gerla & Galloway; 1998 | 0 | 5 | 1 | 1 | 1 |
| Gluns & Toews; 1989 | 0 | 8 | 2 | 2 | 1 |
| Hauer & Spencer 1998 | 0 | 15 | 5 | 5 | 3 |
| Hickenbottom et al. 2023 | 2 | 2 | 2 | 2 | 2 |
| Mast & Clow; 2008 | 4 | 4 | 1 | 1 | 1 |
| Murphy et al. 2018 | 15 | 15 | 3 | 3 | 1 |
| Oliver et al. 2012 | 0 | 2 | 1 | 1 | 1 |
| Rhea et al. 2021 | 3 | 3 | 3 | 3 | 2 |
| Uzun et al. 2020 | 4 | 4 | 2 | 2 | 1 |
| Writer et al. 2014 | 4 | 2 | 2 | 2 | 1 |

## Exclusion and counting rules

- Exclude four Murphy 2010 singleton records (Site_1/Site_2 × DOC/nitrate) and two Writer 2012 nitrate singleton records; preserve them upstream and in `data/audit/paper_singleton_exclusions.csv`.
- Exclude only the configured Crandall Site_2–Control_2 nitrate 2018 key; retain its DOC record and all other Crandall records. Preserve full exclusion evidence in `data/audit/paper_record_exclusions.csv` and rationale in `config/paper_record_exclusions.csv`. No general low-response threshold is applied.
- All 110 upstream annual records remain intact: 110 − 6 singletons − 1 author-selected outlier = 103 retained records.
- Count physical watersheds once within study/role; deduplicate shared references and Uzun seasonal aliases. The 28-pair overall inventory remains, although nitrate loses one pair and its unique reference.
- High Park is shared across Rhea and Writer; 15 study/fire records represent 14 unique fires. Connected study/fire inference uses six DOC and eleven nitrate components.
- These retained data do not independently verify the original 226-screened-paper count or the proposed three biome categories. The earlier 52-watershed manuscript total remains unsupported.

## Reproducible evidence

`R/checks/01_verify_paper_inventory_agent_v1.R` exports `tables/paper_inventory_summary.csv`, `tables/paper_study_inventory.csv`, `tables/paper_pair_inventory.csv` and `tables/paper_watershed_inventory.csv`. Source annual effects remain in `data/derived/reviewed_sites_agent_v1/effect_sizes_yearly.csv`. The complete prior 104-record package/model tables are preserved in `archive/pre_nitrate_exclusion_2026_10_22/`; paths outside `tables/` are relative to the workflow root.
