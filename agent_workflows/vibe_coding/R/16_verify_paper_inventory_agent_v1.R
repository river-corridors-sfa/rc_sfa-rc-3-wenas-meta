# Inventory of retained paper records, with shared references and seasonal labels deduplicated.
library(tidyverse)
library(here)
b <- here('agent_workflows','vibe_coding')
m <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
s <- read_csv(file.path(b,'config/reviewed_site_selection.csv'),show_col_types=FALSE)
out <- file.path(b,'output/paper/tables')
stopifnot(all(m$lnRR_n >= 2), !anyDuplicated(m[c('candidate_pair_id','response_var','year')]))
pairs <- m %>% distinct(Study_ID,candidate_pair_id,Pair_Burn,Pair_Unburn,Fire_ID_Analysis)
# Approved role IDs represent physical watersheds; Uzun seasonal Site labels
# are aliases of those same watersheds, not additional independent locations.
sites <- s %>% filter(Include_Site) %>% semi_join(pairs,by=c('Study_ID','candidate_pair_id')) %>%
  group_by(Study_ID,Burn_Unburn,Pair) %>%
  summarise(site_labels=paste(sort(unique(Site)),collapse='; '),.groups='drop')
write_csv(sites,file.path(out,'paper_watershed_inventory.csv'))
write_csv(pairs,file.path(out,'paper_pair_inventory.csv'))
counts <- m %>% group_by(Study_ID) %>% summarise(
  annual_responses=n(),DOC_responses=sum(response_var=='DOC'),NO3_responses=sum(response_var=='NO3'),
  pairs=n_distinct(candidate_pair_id),fires=n_distinct(Fire_ID_Analysis),
  first_year=min(year),last_year=max(year),.groups='drop') %>%
  left_join(sites %>% group_by(Study_ID) %>% summarise(
    burned_watersheds=sum(Burn_Unburn=='Burn'),reference_watersheds=sum(Burn_Unburn=='Unburn'),.groups='drop'),by='Study_ID')
metadata <- read_csv(file.path(b,'data/source/Sites_meta_data.csv'),locale=locale(encoding='Latin1'),show_col_types=FALSE)
counts <- counts %>% left_join(metadata %>% select(Study_ID,Title,DOI,Ecosystem) %>% distinct(),by='Study_ID',relationship='one-to-one')
write_csv(counts,file.path(out,'paper_study_inventory.csv'))
summary <- bind_rows(lapply(c('All','DOC','NO3'),function(a) {
  r <- if(a=='All') m else filter(m,response_var==a)
  rs <- s %>% filter(Include_Site) %>% semi_join(r,by=c('Study_ID','candidate_pair_id')) %>% distinct(Study_ID,Burn_Unburn,Pair)
  tibble(analyte=a,annual_responses=nrow(r),studies=n_distinct(r$Study_ID),
    fires=n_distinct(r$Fire_ID_Analysis),study_fire_records=nrow(distinct(r,Study_ID,Fire_ID_Analysis)),
    pairs=n_distinct(r$candidate_pair_id),burned_watersheds=sum(rs$Burn_Unburn=='Burn'),
    reference_watersheds=sum(rs$Burn_Unburn=='Unburn'))
}))
write_csv(summary,file.path(out,'paper_inventory_summary.csv'))
print(summary,width=Inf)
