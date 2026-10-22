# Regression checks for reviewed physical-site filtering and rebuilt effects.
library(tidyverse)
library(here)
b <- here('agent_workflows','vibe_coding')
p <- read_csv(file.path(b,'config/pairing_decisions_analysis.csv'),show_col_types=FALSE)
s <- read_csv(file.path(b,'config/reviewed_site_selection.csv'),show_col_types=FALSE)
d <- read_csv(file.path(b,'data/derived/reviewed_sites_agent_v1/01_daily_time_series_paired.csv'),show_col_types=FALSE)
a <- read_csv(file.path(b,'data/derived/reviewed_sites_agent_v1/effect_sizes_yearly.csv'),show_col_types=FALSE)
m <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
stopifnot(setequal(a$candidate_pair_id,p$candidate_pair_id[p$Include_Analysis]),
 !any(a$candidate_pair_id %in% p$candidate_pair_id[!p$Include_Analysis]),
 !anyDuplicated(a[c('candidate_pair_id','response_var','year')]),
 !anyDuplicated(m[c('candidate_pair_id','response_var','year')]),
 all(m$Coauthor_Confirmation=='confirmed'),nrow(m)==nrow(a),
 sum(s$Site_Decision=='excluded_author_confirmed')==3)
for (spec in list(c('Rhea et al. 2021','Control_1','U1'),
                  c('Crandall et al. 2021','Control_1','Hobble Creek Lower'),
                  c('Crandall et al. 2021','Control_2','Mill Race'))) {
 stopifnot(identical(unique(d$Site[d$Study_ID==spec[1]&d$Pair==spec[2]]),spec[3]))
}
stopifnot(setequal(unique(d$Site[d$Study_ID=='Mast & Clow; 2008']),c('Coal','Pinchot')))
w <- a %>% filter(Study_ID=='Writer et al. 2014',response_var=='NO3',Pair_Burn=='Site_1',year==2013)
stopifnot(nrow(w)==1,w$lnRR_n==2,abs(w$lnRR_var-0.37284128138369427)<1e-10,
          all(a$n_both_observed+a$n_with_interpolation==a$lnRR_n))
cat('Reviewed-site exclusions, 28-pair coverage, annual uniqueness, observation counts, and Writer regression checks passed.\n')
