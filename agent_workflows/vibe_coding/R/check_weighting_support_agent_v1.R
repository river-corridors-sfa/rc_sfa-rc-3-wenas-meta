# Focused regression checks for weighting and fold preprocessing.
library(tidyverse)
library(here)
b <- here('agent_workflows','vibe_coding')
# Load definitions without running the model loops.
e <- new.env(parent=globalenv())
Sys.setenv(PAPER_SCENARIO='validation')
for(expr in parse(file.path(b,'R/11_fit_issue6_revised_primary_agent_v1.R'))) {
  if(is.call(expr) && identical(expr[[1]],as.name('<-')) && identical(expr[[2]],as.name('predictions'))) break
  eval(expr,e)
}
Sys.unsetenv('PAPER_SCENARIO')
x <- tibble(Study_ID=c('A','A','A','B'),candidate_pair_id=c('a','a','b','c'))
w <- e$weights_for(x)
stopifnot(abs(sum(w[1:3])-w[4])<1e-12,abs(sum(w[1:2])-w[3])<1e-12)
boot <- bind_rows(mutate(x,bootstrap_cluster=if_else(Study_ID=='A',1,2)),mutate(filter(x,Study_ID=='A'),bootstrap_cluster=3))
bw <- e$weights_for(boot)
stopifnot(diff(range(tapply(bw,boot$bootstrap_cluster,sum)))<1e-12)
e$transformations <- c(x='none',z='none')
tr <- tibble(x=c(1,2,3,NA),z=c(2,4,6,8))
a <- e$prepare(tr,tibble(x=1000,z=2000),c('x','z'))
c <- e$prepare(tr,tibble(x=-1000,z=-2000),c('x','z'))
stopifnot(identical(a$train,c$train),all(is.finite(as.matrix(a$train))))
m <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
o <- read_csv(file.path(b,'data/derived/paper_observed_only_model_table.csv'),show_col_types=FALSE)
s <- read_csv(file.path(b,'data/audit/paper_sampling_support.csv'),show_col_types=FALSE)
stopifnot(nrow(m)==104,nrow(o)==89,all(o$n_with_interpolation==0),all(o$lnRR_n>=2),
  sum(s$observed_only_eligible)==nrow(o),all(s$n_observed<=s$n_daily))
cat('PASS: equal study/pair/draw weights; training-only preprocessing; observed-only eligibility.\n')
