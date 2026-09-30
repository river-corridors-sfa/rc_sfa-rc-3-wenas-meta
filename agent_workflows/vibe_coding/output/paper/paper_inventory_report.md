# Verified paper study inventory

After excluding six singleton annual records, the paper analysis includes **12 publications, 14 unique wildfires, 28 burned–reference pairs, and 46 watersheds (28 burned and 18 reference)**. These contribute **104 annual responses: 35 DOC from 7 studies and 69 nitrate from 12 studies**. DOC includes 16 pairs, 16 burned and 11 reference watersheds, and 9 unique fires. Nitrate includes the entire retained inventory.

Suggested Methods wording:

> The final paired analysis included 12 publications representing 14 unique wildfires and 28 burned–reference watershed pairs, comprising 28 burned and 18 reference watersheds. After excluding six pair–analyte–year records with only one eligible matched concentration date, the dataset contained 35 annual DOC effect sizes from seven publications and 69 annual nitrate effect sizes from 12 publications.

## Study counts

| Publication | DOC rows | Nitrate rows | Pairs | Burned watersheds | Reference watersheds |
|---|---:|---:|---:|---:|---:|
| Coombs & Melack; 2013 | 0 | 6 | 3 | 3 | 1 |
| Crandall et al. 2021 | 3 | 3 | 3 | 3 | 3 |
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

- Murphy et al. 2018: remove 2010 DOC and nitrate records for Site_1 and Site_2 (four rows). Its later records remain.
- Writer et al. 2014: remove 2012 nitrate records for Site_1 and Site_2 (two rows). DOC and later nitrate records remain.
- Count a watershed once per study and approved role/site ID, regardless of analyte, year, or reuse of the reference. The reviewed physical-site selection maps reference IDs to actual retained sites; composite-looking ID names do not establish multiple reference watersheds.
- Uzun's three seasonal labels per site are aliases of two burned watersheds and one reference watershed, not nine watersheds.
- High Park occurs in Rhea and Writer with distinct retained watersheds: 15 study/fire records represent 14 unique fires. Do not sum per-study fire counts as unique fires.
- All 28 approved pairs still have retained responses. The six-row exclusion does not remove either affected publication, any entire pair, or any fire.

The manuscript's 52-watershed total (32 burned, 20 reference) is not supported by the retained reviewed analysis. The reported count above is based on reviewed study-specific physical-site identities; no new field-site or original-paper review was performed. The metadata ecosystem labels do not establish the claimed three climate/biome categories. The original 226-paper search inventory and three-category classification remain separate verification tasks.

## Reproducible evidence

`R/16_verify_paper_inventory_agent_v1.R` runs in the paper workflow. Its tables are `tables/paper_inventory_summary.csv`, `tables/paper_study_inventory.csv`, `tables/paper_pair_inventory.csv`, and `tables/paper_watershed_inventory.csv`. The physical-site table preserves original seasonal aliases. Full excluded records are in `data/audit/paper_singleton_exclusions.csv` relative to the workflow root; all annual records remain in `data/derived/reviewed_sites_agent_v1/effect_sizes_yearly.csv`.
