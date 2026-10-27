# Paired comparisons of fixed Figure 3 held-out errors, with component bootstrap.
# This resamples validation errors, not model fits or CV fold assignments.
library(tidyverse)
library(here)
set.seed(20261027)
root <- here("agent_workflows","vibe_coding")
out <- file.path(root,"output/paper/predictor_group_cv/paired_uncertainty")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
attempts <- as.integer(Sys.getenv("N_ERROR_BOOTSTRAP","100000"))
stopifnot(is.finite(attempts),attempts>=1000)
tolerance <- 1e-10
models <- c("intercept_only","time_only","time_wildfire","time_watershed","time_full")
labels <- c(intercept_only="Intercept only",time_only="Time",time_wildfire="Time + wildfire",
            time_watershed="Time + watershed",time_full="Time + watershed + wildfire")
combinations <- combn(models,2)
comparisons <- tibble(comparison_id=seq_len(ncol(combinations)),
  reference_model=combinations[1,],candidate_model=combinations[2,],
  reference_label=unname(labels[reference_model]),candidate_label=unname(labels[candidate_model]))
write_csv(comparisons,file.path(out,"comparisons.csv"))

predictions <- read_csv(file.path(root,"output/paper/predictor_group_cv/heldout_predictions.csv"),show_col_types=FALSE)
input <- read_csv(file.path(root,"data/derived/lasso_model_table.csv"),show_col_types=FALSE) %>%
  mutate(row_id=row_number())
clusters <- read_csv(file.path(root,"output/paper/balanced_synthesis/study_fire_clusters.csv"),show_col_types=FALSE) %>%
  distinct(Study_ID,dependence_cluster)
stopifnot(!anyDuplicated(clusters$Study_ID),nrow(predictions)==length(models)*nrow(input),
  !anyDuplicated(predictions[c("response_var","row_id","model")]),
  all(predictions$Study_ID==predictions$held_out_study),all(is.finite(predictions$predicted)))
membership <- predictions %>% distinct(response_var,Study_ID) %>% left_join(clusters,by="Study_ID")
stopifnot(!anyNA(membership$dependence_cluster))
fire_check <- input %>% distinct(Study_ID,Fire_ID_Analysis) %>% left_join(clusters,by="Study_ID") %>%
  group_by(Fire_ID_Analysis) %>% summarise(clusters=n_distinct(dependence_cluster),.groups="drop")
stopifnot(all(fire_check$clusters==1))

publication_errors <- predictions %>% group_by(response_var,model,Study_ID) %>%
  summarise(n_records=n(),MSE=mean((observed-predicted)^2),
    MAE=mean(abs(observed-predicted)),.groups="drop") %>% mutate(RMSE=sqrt(MSE)) %>%
  left_join(clusters,by="Study_ID")
coverage <- publication_errors %>% count(response_var,Study_ID)
stopifnot(all(coverage$n==length(models)))
overall <- publication_errors %>% group_by(response_var,model) %>%
  summarise(publications=n(),RMSE=sqrt(mean(MSE)),.groups="drop")
reference_performance <- read_csv(file.path(root,"output/paper/predictor_group_cv/performance.csv"),show_col_types=FALSE)
check <- overall %>% left_join(reference_performance %>% select(response_var,model,saved_RMSE=RMSE),
  by=c("response_var","model"))
stopifnot(!anyNA(check$saved_RMSE),max(abs(check$RMSE-check$saved_RMSE))<1e-12)

paired <- list()
for(i in seq_len(nrow(comparisons))) {
  reference <- publication_errors %>% filter(model==comparisons$reference_model[i]) %>%
    select(response_var,Study_ID,dependence_cluster,n_records,reference_MSE=MSE,reference_RMSE=RMSE)
  candidate <- publication_errors %>% filter(model==comparisons$candidate_model[i]) %>%
    select(response_var,Study_ID,candidate_MSE=MSE,candidate_RMSE=RMSE)
  paired[[i]] <- left_join(reference,candidate,by=c("response_var","Study_ID")) %>%
    mutate(comparison_id=comparisons$comparison_id[i],reference_model=comparisons$reference_model[i],
      candidate_model=comparisons$candidate_model[i],
      RMSE_difference=candidate_RMSE-reference_RMSE,
      RMSE_improvement=reference_RMSE-candidate_RMSE,
      improvement_percent=100*RMSE_improvement/reference_RMSE,
      outcome=case_when(RMSE_improvement>tolerance ~ "improved",
        RMSE_improvement < -tolerance ~ "worsened",TRUE ~ "tied"))
}
paired <- bind_rows(paired)
stopifnot(all(is.finite(paired$RMSE_improvement)),all(paired$reference_RMSE>0))
counts <- paired %>% group_by(response_var,comparison_id,reference_model,candidate_model) %>%
  summarise(publications=n(),improved=sum(outcome=="improved"),tied=sum(outcome=="tied"),
    worsened=sum(outcome=="worsened"),.groups="drop")
stopifnot(all(counts$improved+counts$tied+counts$worsened==counts$publications))

uncertainty <- list(); model_uncertainty <- list(); component_tables <- list()
for(a in c("DOC","NO3")) {
  d <- publication_errors %>% filter(response_var==a)
  components <- d %>% distinct(Study_ID,dependence_cluster) %>% count(dependence_cluster,name="publications") %>%
    arrange(dependence_cluster) %>% mutate(draw_column=paste0("component_",row_number()))
  component_tables[[a]] <- d %>% distinct(Study_ID,dependence_cluster) %>%
    left_join(components,by="dependence_cluster") %>% mutate(response_var=a)
  summed_mse <- d %>% group_by(dependence_cluster,model) %>% summarise(total_MSE=sum(MSE),.groups="drop") %>%
    pivot_wider(names_from=model,values_from=total_MSE) %>% arrange(dependence_cluster)
  stopifnot(identical(components$dependence_cluster,summed_mse$dependence_cluster))
  C <- nrow(components)
  # One multinomial draw selects C complete dependence components. Shared-fire
  # publications have identical multiplicity in every model comparison.
  multiplicity <- t(rmultinom(attempts,size=C,prob=rep(1/C,C)))
  colnames(multiplicity) <- components$draw_column
  publication_count <- as.numeric(multiplicity %*% components$publications)
  boot_mse <- sweep(multiplicity %*% as.matrix(summed_mse[models]),1,publication_count,"/")
  boot_rmse <- sqrt(boot_mse)
  stopifnot(all(is.finite(boot_rmse)),all(boot_rmse>0))
  # This is a ratio of component totals: each included publication retains equal
  # influence, rather than forcing the two-publication component to study weight one.
  point_rmse <- sqrt(colSums(as.matrix(summed_mse[models]))/sum(components$publications))
  for(model in models) {
    model_uncertainty[[paste(a,model)]] <- tibble(response_var=a,model=model,
      publications=sum(components$publications),components=C,RMSE=point_rmse[[model]],
      lower_RMSE=quantile(boot_rmse[,model],.025),upper_RMSE=quantile(boot_rmse[,model],.975))
  }
  for(i in seq_len(nrow(comparisons))) {
    reference <- comparisons$reference_model[i]
    candidate <- comparisons$candidate_model[i]
    improvement <- boot_rmse[,reference]-boot_rmse[,candidate]
    improvement[abs(improvement)<=tolerance] <- 0
    percent <- 100*improvement/boot_rmse[,reference]
    point <- point_rmse[[reference]]-point_rmse[[candidate]]
    if(abs(point)<=tolerance) point <- 0
    uncertainty[[paste(a,i)]] <- tibble(response_var=a,comparison_id=comparisons$comparison_id[i],
      reference_model=reference,candidate_model=candidate,components=C,bootstrap_draws=attempts,
      reference_RMSE=point_rmse[[reference]],candidate_RMSE=point_rmse[[candidate]],
      RMSE_improvement=point,lower_RMSE_improvement=quantile(improvement,.025),
      upper_RMSE_improvement=quantile(improvement,.975),
      improvement_percent=100*point/point_rmse[[reference]],
      lower_improvement_percent=quantile(percent,.025),upper_improvement_percent=quantile(percent,.975),
      fraction_resamples_positive=mean(improvement>tolerance),
      fraction_resamples_tied=mean(abs(improvement)<=tolerance),
      interval="95_percentile_component_bootstrap_fixed_heldout_errors")
  }
  write_csv(bind_cols(tibble(draw=seq_len(attempts),publications=publication_count,
      distinct_components=rowSums(multiplicity>0)),as_tibble(multiplicity),
      as_tibble(boot_rmse) %>% rename_with(~paste0("RMSE_",.x))),
    file.path(out,paste0("bootstrap_errors_",a,".csv.gz")))
}
summary <- bind_rows(uncertainty) %>% left_join(counts,
  by=c("response_var","comparison_id","reference_model","candidate_model"))
write_csv(publication_errors,file.path(out,"publication_model_errors.csv"))
write_csv(paired,file.path(out,"paired_publication_RMSE_differences.csv"))
write_csv(counts,file.path(out,"publication_improvement_counts.csv"))
write_csv(summary,file.path(out,"overall_RMSE_improvement_uncertainty.csv"))
write_csv(bind_rows(model_uncertainty),file.path(out,"model_RMSE_uncertainty.csv"))
write_csv(bind_rows(component_tables),file.path(out,"component_membership.csv"))
input_paths <- c("data/derived/lasso_model_table.csv",
  "output/paper/predictor_group_cv/heldout_predictions.csv",
  "output/paper/predictor_group_cv/performance.csv",
  "output/paper/balanced_synthesis/study_fire_clusters.csv",
  "R/analysis/04_summarize_paired_cv_uncertainty_agent_v1.R")
write_csv(tibble(path=input_paths,md5=unname(tools::md5sum(file.path(root,input_paths)))),file.path(out,"input_manifest.csv"))
write_csv(tibble(seed=20261027,draws=attempts,tie_tolerance=tolerance,
  resampling_unit="connected_publication_fire_component",target="equal_publication_mean_MSE_then_square_root",
  interval="95_percentile",model_refitting=FALSE,prediction_source="Figure_3_time_unpenalized_group_CV"),
  file.path(out,"run_settings.csv"))
capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
writeLines(c(
  "Paired Figure 3 cross-validation error comparisons",
  "Positive RMSE improvement = reference RMSE minus candidate RMSE. RMSE_difference has the opposite sign.",
  "Within-publication RMSE is sqrt(mean squared held-out annual error). Counts classify differences within 1e-10 lnRR units as ties.",
  "Overall RMSE is sqrt(mean publication MSE), not the mean publication RMSE. Every publication has equal point-estimate weight.",
  "Bootstrap draws select the same number of connected publication/fire components as observed, with replacement. All publications in a selected component enter together, once per component selection. Model comparisons use identical draw multiplicities.",
  "Rhea and Writer share one dependence component through High Park. There are six DOC and eleven nitrate components. Publication counts can vary across bootstrap draws because this component contains two publications.",
  "Percentile intervals resample fixed held-out prediction errors. They describe uncertainty from the represented publication/fire-group composition conditional on the fitted CV predictions. They do not refit/re-tune models, account fully for covariance induced by overlapping CV training sets, or remove shared-fire information crossing the original publication validation splits.",
  "Few independent components, especially for DOC, limit interval reliability. Resample-positive fractions are descriptive frequencies, not posterior probabilities or formal p-values. Improvement counts are publication counts, not independent binomial trials.",
  "No cross-analyte significance test is performed; publication coverage and candidate soil variables differ. Figure 3 is not modified by this calculation."),
  file.path(out,"methods_and_limits.txt"))
print(summary %>% filter(reference_model=="intercept_only"),width=Inf)
message("Paired CV comparisons and component-resampled error uncertainty complete.")
