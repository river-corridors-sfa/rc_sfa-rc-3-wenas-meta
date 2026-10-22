# Conditional inference using provisional annual variances and a PSD working covariance.
# These fits assess covariance assumptions; they do not validate daily-derived precision.
library(tidyverse)
library(here)
library(metafor)
b <- here('agent_workflows','vibe_coding')
.libPaths(c(file.path(b,'.R-library'),.libPaths()))
if(!requireNamespace('clubSandwich',quietly=TRUE)) stop('Install clubSandwich for CR2 inference')
m <- read_csv(file.path(b,'data/derived/lasso_model_table.csv'),show_col_types=FALSE)
settings <- crossing(reference_rho=c(0,.5,.8),temporal_phi=c(0,.5,.8))
results <- list(); failures <- list(); fits <- list()
for(a in c('DOC','NO3')) {
  d <- filter(m,response_var==a,is.finite(lnRR_var),lnRR_var>0)
  for(i in seq_len(nrow(settings))) {
    rho <- settings$reference_rho[i]; phi <- settings$temporal_phi[i]
    # Mixture of reference-family and pair-specific AR(1) kernels is PSD.
    # Shared controls have identical IDs; unrelated families have zero sampling covariance.
    dt <- abs(outer(d$year,d$year,'-'))
    K <- phi^dt
    same_pair <- outer(d$candidate_pair_id,d$candidate_pair_id,'==')
    same_ref <- outer(d$shared_control_id,d$shared_control_id,'==')
    R <- K*((1-rho)*same_pair+rho*same_ref)
    V <- R*outer(sqrt(d$lnRR_var),sqrt(d$lnRR_var))
    stopifnot(min(eigen(V,symmetric=TRUE,only.values=TRUE)$values)>-1e-8)
    for(spec in c('pooled','time')) {
      fit <- tryCatch(rma.mv(yi=lnRR_mean,V=V,mods=if(spec=='time') ~post_fire_year else ~1,
        random=~1|Study_ID/candidate_pair_id,data=d,method='REML'),error=function(e)e)
      id <- paste(a,rho,phi,spec,sep='_')
      if(inherits(fit,'error')) {failures[[id]] <- tibble(id=id,error=conditionMessage(fit));next}
      # CR2 bias-reduced adjustment with Satterthwaite degrees of freedom.
      adjusted <- tryCatch(robust(fit,cluster=d$Study_ID,clubSandwich=TRUE),error=function(e)e)
      if(inherits(adjusted,'error')) {failures[[id]] <- tibble(id=id,error=conditionMessage(adjusted));next}
      fits[[id]] <- fit
      results[[id]] <- tibble(response_var=a,reference_rho=rho,temporal_phi=phi,specification=spec,
        term=rownames(adjusted$beta),estimate=as.numeric(adjusted$beta),SE=adjusted$se,
        lower=adjusted$ci.lb,upper=adjusted$ci.ub,df=adjusted$dfs,
        studies=n_distinct(d$Study_ID),rows=nrow(d),
        study_heterogeneity=fit$sigma2[1],pair_heterogeneity=fit$sigma2[2],
        inference='conditional_CR2_study_clustered_Satterthwaite',annual_variance='provisional_daily_delta')
    }
  }
}
write_csv(bind_rows(results),file.path(b,'output/paper/tables/working_covariance_inference.csv'))
write_csv(bind_rows(failures),file.path(b,'output/paper/tables/working_covariance_failures.csv'))
saveRDS(fits,file.path(b,'output/paper/working_covariance_models.rds'))
