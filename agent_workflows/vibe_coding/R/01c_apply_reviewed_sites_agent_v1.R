# Apply reviewed pair and physical-site decisions; rebuild annual nonnormalized lnRR.
# Uses corrected chemistry without changing source snapshots or human scripts.
library(tidyverse)
library(here)
library(lubridate)
b <- here('agent_workflows', 'vibe_coding')
source(file.path(b, 'R', '01b_promote_reviewed_pairings_agent_v1.R'), local = new.env(parent = globalenv()))
d <- read_csv(file.path(b,'data/derived/gapfill_corrected_agent_v1/01_daily_time_series_paired.csv'), show_col_types=FALSE)
p <- read_csv(file.path(b,'config/pairing_decisions_analysis.csv'), show_col_types=FALSE)
audit <- file.path(b,'data/audit')
out <- file.path(b,'data/derived/reviewed_sites_agent_v1')
dir.create(out,recursive=TRUE,showWarnings=FALSE)

# Expand only reviewed pair roles, not burned x reference cross-products.
roles <- bind_rows(
  p %>% mutate(Pair=Pair_Burn, Burn_Unburn='Burn'),
  p %>% mutate(Pair=Pair_Unburn, Burn_Unburn='Unburn')
)
site_keys <- c('Study_ID','Comparison_ID','Pair','Burn_Unburn')
inventory <- d %>% distinct(across(all_of(site_keys)),Site)
selection <- roles %>% inner_join(inventory, by=site_keys, relationship='many-to-many') %>%
  mutate(
    selected_reference = case_when(
      Study_ID=='Rhea et al. 2021' & Pair=='Control_1' ~ 'U1',
      Study_ID=='Crandall et al. 2021' & Pair=='Control_1' ~ 'Hobble Creek Lower',
      Study_ID=='Crandall et al. 2021' & Pair=='Control_2' ~ 'Mill Race',
      TRUE ~ NA_character_
    ),
    Include_Site = Include_Analysis & (is.na(selected_reference) | Site==selected_reference),
    Site_Decision = case_when(
      !Include_Analysis ~ 'excluded_pair',
      !Include_Site ~ 'reference_not_selected',
      TRUE ~ 'retained'
    ),
    Site_Decision_Source='Pairing Review workbook: inclusion, evidence, and decision notes',
    Site_Decision_Reason=if_else(Site_Decision=='reference_not_selected',
      paste('Workbook selects',selected_reference,'only for this comparison.'),Decision_Notes)
  )
# Unmapped records are never silently added to the reviewed inventory.
unmapped <- inventory %>% anti_join(roles,by=site_keys) %>% mutate(
  Include_Site=FALSE,
  Site_Decision=if_else(Study_ID=='Mast & Clow; 2008' &
    Pair %in% c('Site_2_post','Site_3_post','Site_4_post') &
    Site %in% c('Fish Creek','Lower MacDonald Creek','Upper MacDonald Creek'),
    'excluded_author_confirmed','unexpected_unmapped'),
  Site_Decision_Source='Author instruction, 2026-09-22, following paper review',
  Site_Decision_Reason=if_else(Site_Decision=='excluded_author_confirmed',
    'Retain only Coal–Pinchot. Other references are fire-impacted; other fire sites have multiple fires and/or are downstream of a very large lake. Collective author rationale; no site-specific mechanism inferred.',
    'No reviewed pair role matches this site; investigate before use.')
)
config <- bind_rows(selection,unmapped)
write_csv(config,file.path(b,'config/reviewed_site_selection.csv'),na='')
write_csv(config %>% count(Study_ID,Site_Decision),file.path(audit,'reviewed_site_selection_summary.csv'))
if(any(config$Site_Decision=='unexpected_unmapped')) stop('Unexpected unmapped sites; inspect reviewed_site_selection.csv')
selected <- selection %>% filter(Include_Site)
coverage <- roles %>% filter(Include_Analysis) %>% select(Study_ID,candidate_pair_id,Burn_Unburn) %>%
  left_join(selected %>% count(candidate_pair_id,Burn_Unburn,name='n_selected_sites'),by=c('candidate_pair_id','Burn_Unburn'))
write_csv(coverage,file.path(audit,'reviewed_site_role_coverage.csv'))
if(any(is.na(coverage$n_selected_sites) | (coverage$n_selected_sites!=1 & !(coverage$Study_ID=='Uzun et al. 2020' & coverage$n_selected_sites==3)))) stop('Each included pair must have one explicitly selected physical site per role.')

membership_keys <- c(site_keys,'Site')
filtered <- d %>% semi_join(selected,by=membership_keys)
stopifnot(!anyDuplicated(filtered[c(membership_keys,'Sampling_Date')]))
write_csv(filtered,file.path(out,'01_daily_time_series_paired.csv'),na='')

# Retain analyte-specific observed flags for sensitivity analyses.
chem <- filtered %>% pivot_longer(c(DOC_Interp_mg_C_L,NO3_Interp_mg_N_L),names_to='response_var',values_to='concentration') %>%
  mutate(observed=if_else(response_var=='DOC_Interp_mg_C_L',DOC_mg_C_L_Observed,NO3_mg_N_L_Observed),
         response_var=if_else(response_var=='DOC_Interp_mg_C_L','DOC','NO3'))
linked <- selected %>% select(candidate_pair_id,all_of(membership_keys)) %>%
  inner_join(chem,by=membership_keys,relationship='many-to-many')
burn <- linked %>% filter(Burn_Unburn=='Burn') %>% select(candidate_pair_id,Sampling_Date,response_var,Site_Burn=Site,burn=concentration,burn_observed=observed)
ref <- linked %>% filter(Burn_Unburn=='Unburn') %>% select(candidate_pair_id,Sampling_Date,response_var,Site_Unburn=Site,reference=concentration,reference_observed=observed)
paired <- inner_join(burn,ref,by=c('candidate_pair_id','Sampling_Date','response_var'),relationship='one-to-one') %>%
  mutate(valid=is.finite(burn)&is.finite(reference)&burn>0&reference>0,
         lnRR=log(if_else(valid,burn/reference,NA_real_)),year=year(Sampling_Date)) %>%
  left_join(p %>% select(candidate_pair_id,Study_ID,Comparison_ID,Pair_Burn,Pair_Unburn,shared_control_id),by='candidate_pair_id')
write_csv(paired,file.path(out,'effect_sizes_daily.csv'),na='')
write_csv(paired %>% group_by(candidate_pair_id,response_var,year) %>%
  summarise(n_matched_dates=n(),n_valid=sum(valid),n_both_observed=sum(valid & burn_observed & reference_observed),.groups='drop'),
  file.path(audit,'reviewed_chemistry_coverage.csv'))
annual <- paired %>% filter(valid) %>%
  group_by(Study_ID,Comparison_ID,Pair_Burn,Pair_Unburn,candidate_pair_id,shared_control_id,response_var,year) %>%
  summarise(lnRR_mean=mean(lnRR),lnRR_sd=sd(lnRR),lnRR_n=n(),
    n_both_observed=sum(burn_observed & reference_observed),
    n_with_interpolation=sum(!burn_observed | !reference_observed),.groups='drop') %>%
  mutate(lnRR_var=lnRR_sd^2/lnRR_n,pair_key=candidate_pair_id)
write_csv(annual,file.path(out,'effect_sizes_yearly.csv'),na='')
write_csv(p %>% filter(Include_Analysis) %>% anti_join(annual,by='candidate_pair_id'),file.path(audit,'reviewed_pairs_without_valid_chemistry.csv'),na='')
old <- read_csv(file.path(b,'data/source/effect_sizes_yearly.csv'),show_col_types=FALSE) %>%
  semi_join(p %>% filter(Include_Analysis),by=c('Study_ID','Comparison_ID','Pair_Burn','Pair_Unburn'))
comparison <- full_join(old %>% select(Study_ID,Comparison_ID,Pair_Burn,Pair_Unburn,response_var,year,lnRR_mean,lnRR_var,lnRR_n),
  annual %>% select(Study_ID,Comparison_ID,Pair_Burn,Pair_Unburn,response_var,year,lnRR_mean,lnRR_var,lnRR_n),
  by=c('Study_ID','Comparison_ID','Pair_Burn','Pair_Unburn','response_var','year'),suffix=c('_before','_after')) %>%
  mutate(mean_change=lnRR_mean_after-lnRR_mean_before,
         eligible_before=is.finite(lnRR_var_before)&lnRR_var_before>0&lnRR_n_before>=2,
         eligible_after=is.finite(lnRR_var_after)&lnRR_var_after>0&lnRR_n_after>=2)
write_csv(comparison,file.path(audit,'reviewed_effect_size_changes.csv'),na='')
stopifnot(all(annual$candidate_pair_id %in% p$candidate_pair_id[p$Include_Analysis]),
          !anyDuplicated(annual[c('candidate_pair_id','response_var','year')]))
print(annual %>% group_by(response_var) %>% summarise(rows=n(),pairs=n_distinct(candidate_pair_id),studies=n_distinct(Study_ID),usable_variances=sum(is.finite(lnRR_var)&lnRR_var>0),.groups='drop'))
