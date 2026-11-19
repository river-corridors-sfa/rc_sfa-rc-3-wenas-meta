# Quantify actual sampling information separately from interpolated daily coverage.
library(tidyverse)
library(here)
b <- here('agent_workflows','vibe_coding')
m <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
d <- read_csv(file.path(b,'data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv'),show_col_types=FALSE) %>%
  semi_join(m,by=c('candidate_pair_id','response_var','year')) %>% filter(valid) %>%
  arrange(candidate_pair_id,response_var,year,Sampling_Date)
support <- d %>% group_by(candidate_pair_id,response_var,year) %>% summarise(
  Study_ID=first(Study_ID),n_daily=n(),n_observed=sum(burn_observed & reference_observed),
  n_burn_observed=sum(burn_observed),n_reference_observed=sum(reference_observed),
  interpolated_fraction=mean(!burn_observed | !reference_observed),
  coverage_days=as.numeric(max(Sampling_Date)-min(Sampling_Date))+1,
  observed_span_days=if(sum(burn_observed & reference_observed)>1)
    as.numeric(diff(range(Sampling_Date[burn_observed & reference_observed]))) else NA_real_,
  median_observed_gap_days=if(sum(burn_observed & reference_observed)>1)
    median(as.numeric(diff(Sampling_Date[burn_observed & reference_observed]))) else NA_real_,
  max_observed_gap_days=if(sum(burn_observed & reference_observed)>1)
    max(as.numeric(diff(Sampling_Date[burn_observed & reference_observed]))) else NA_real_,
  .groups='drop') %>% mutate(
    observed_only_eligible=n_observed>=2,
    # A transparent screening flag, not a claim of bootstrap validity.
    temporal_resampling_screen=if_else(n_observed>=20 & observed_span_days>=90,
      'candidate_requires_dependence_and_seasonality_review','insufficient_support_for_automatic_block_bootstrap'))
write_csv(support,file.path(b,'data/audit/paper_sampling_support.csv'))
observed <- d %>% filter(burn_observed,reference_observed) %>%
  group_by(candidate_pair_id,response_var,year) %>% summarise(
    observed_n=n(),observed_mean_burn=mean(burn),observed_mean_reference=mean(reference),
    observed_lnRR=log(mean(burn)/mean(reference)),
    observed_variance=var(burn/mean(burn)-reference/mean(reference))/n(),.groups='drop')
comparison <- m %>% select(candidate_pair_id,response_var,year,Study_ID,lnRR_mean,lnRR_n) %>%
  left_join(observed,by=c('candidate_pair_id','response_var','year')) %>%
  mutate(observed_n=replace_na(observed_n,0L),eligible=observed_n>=2,
         response_change=observed_lnRR-lnRR_mean)
write_csv(comparison,file.path(b,'output/paper/tables/observed_only_response_comparison.csv'))
write_csv(filter(comparison,!eligible),file.path(b,'data/audit/observed_only_exclusions.csv'))
obs_model <- m %>% inner_join(filter(observed,observed_n>=2),by=c('candidate_pair_id','response_var','year')) %>%
  mutate(lnRR_mean=observed_lnRR,lnRR_var=observed_variance,lnRR_n=observed_n,
    annual_mean_burn=observed_mean_burn,annual_mean_reference=observed_mean_reference,
    n_both_observed=observed_n,n_with_interpolation=0,
    variance_method='paired_delta_observed_daily_independence_provisional') %>%
  group_by(candidate_pair_id,year) %>% mutate(matched_doc_no3=all(c('DOC','NO3') %in% response_var)) %>% ungroup()
write_csv(obs_model,file.path(b,'data/derived/paper_observed_only_model_table.csv'),na='')
print(support %>% group_by(response_var) %>% summarise(rows=n(),observed_only_eligible=sum(observed_only_eligible),
  minimum_observed=min(n_observed),median_observed=median(n_observed),maximum_observed=max(n_observed),
  bootstrap_candidates=sum(n_observed>=20 & observed_span_days>=90),.groups='drop'))
