# Compare weighting and interpolation on explicitly matched record inventories.
library(tidyverse)
library(here)
b <- here('agent_workflows','vibe_coding')
primary <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
observed <- read_csv(file.path(b,'data/derived/paper_observed_only_model_table.csv'),show_col_types=FALSE)
matched_path <- file.path(b,'data/derived/paper_interpolated_matched_model_table.csv')
write_csv(semi_join(primary,observed,by=c('candidate_pair_id','response_var','year')),matched_path)
settings <- tribble(~scenario,~weight,~input,
 'equal_row','equal_row','lasso_model_table.csv',
 'precision_family','precision_family','lasso_model_table.csv',
 'observed_only','equal_study','paper_observed_only_model_table.csv',
 'interpolated_matched','equal_study','paper_interpolated_matched_model_table.csv')
for(i in seq_len(nrow(settings))) {
  Sys.setenv(PAPER_SCENARIO=settings$scenario[i],PAPER_WEIGHT_METHOD=settings$weight[i],
    PAPER_MODEL_TABLE=file.path(b,'data/derived',settings$input[i]))
  tryCatch(source(file.path(b,'R/11_fit_issue6_revised_primary_agent_v1.R'),local=new.env(parent=globalenv())),
    finally=Sys.unsetenv(c('PAPER_SCENARIO','PAPER_WEIGHT_METHOD','PAPER_MODEL_TABLE')))
}
all_performance <- bind_rows(read_csv(file.path(b,'output/paper/tables/performance_equal_study.csv'),show_col_types=FALSE) %>% mutate(scenario='primary'),
  map_dfr(settings$scenario,function(sc) read_csv(file.path(b,'output/paper/sensitivity',sc,'performance_equal_study.csv'),show_col_types=FALSE) %>% mutate(scenario=sc)))
write_csv(all_performance,file.path(b,'output/paper/tables/weighting_interpolation_performance.csv'))
