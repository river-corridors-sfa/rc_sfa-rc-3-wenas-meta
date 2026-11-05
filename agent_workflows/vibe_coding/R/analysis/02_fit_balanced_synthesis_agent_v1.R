# Author-selected primary pooled synthesis (October 22, 2026).
# Adapted from exploratory/R/01_review_annual_uncertainty_agent_v1.R.
# Annual variances and publication-based prediction groups remain unchanged.
# CR2 documentation: https://jepusto.github.io/clubSandwich/reference/vcovCR.lm.html
library(tidyverse)
library(here)
b <- here('agent_workflows', 'vibe_coding')
.libPaths(c(file.path(b, '.R-library'), .libPaths()))
library(clubSandwich)

m <- read_csv(file.path(b, 'data/derived/lasso_model_table.csv'), show_col_types=FALSE)
observed <- read_csv(file.path(b, 'data/derived/paper_observed_only_model_table.csv'), show_col_types=FALSE)
out <- file.path(b, 'output/paper/balanced_synthesis')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
stopifnot(!anyNA(m$Study_ID), !anyNA(m$Fire_ID_Analysis),
          all(is.finite(m$lnRR_mean)), all(is.finite(m$post_fire_year)),
          !anyDuplicated(m[c('candidate_pair_id', 'response_var', 'year')]))

# Connected components of studies linked by any included fire. A study's multiple
# fires stay together, as do different publications on the same fire. These are
# conservative working clusters, not evidence of shared physical observations.
links <- distinct(m, Study_ID, Fire_ID_Analysis)
clusters <- tibble(Study_ID=sort(unique(m$Study_ID))) %>%
  mutate(component=seq_len(n()))
repeat {
  previous <- clusters$component
  for (fire in unique(links$Fire_ID_Analysis)) {
    indices <- which(clusters$Study_ID %in% links$Study_ID[links$Fire_ID_Analysis == fire])
    labels <- clusters$component[indices]
    clusters$component[clusters$component %in% labels] <- min(labels)
  }
  if (identical(previous, clusters$component)) break
}
clusters <- clusters %>% group_by(component) %>%
  mutate(dependence_cluster=paste(sort(Study_ID), collapse=' + ')) %>% ungroup()
write_csv(left_join(links, clusters, by='Study_ID'), file.path(out, 'study_fire_clusters.csv'))

inputs <- list(primary=m, observed_only=observed,
               matched_interpolated=semi_join(m, observed,
                 by=c('candidate_pair_id', 'response_var', 'year')))
coefficients <- list(); influences <- list(); weights <- list()
for (scenario in names(inputs)) {
  for (analyte in c('DOC', 'NO3')) {
    d <- inputs[[scenario]] %>% filter(response_var == analyte) %>%
      left_join(select(clusters, Study_ID, dependence_cluster), by='Study_ID') %>%
      group_by(Study_ID, candidate_pair_id) %>% mutate(pair_years=n()) %>%
      group_by(Study_ID) %>% mutate(study_pairs=n_distinct(candidate_pair_id)) %>%
      ungroup() %>% mutate(review_weight=1 / study_pairs / pair_years)
    # These are design/balance weights, explicitly not inverse sampling variance.
    totals <- d %>% group_by(Study_ID) %>% summarise(total=sum(review_weight), .groups='drop')
    pair_totals <- d %>% group_by(Study_ID, candidate_pair_id) %>%
      summarise(total=sum(review_weight), target=first(1/study_pairs), .groups='drop')
    stopifnot(all(abs(totals$total-1) < 1e-10),
              all(abs(pair_totals$total-pair_totals$target) < 1e-10))
    weights[[paste(scenario, analyte)]] <- d %>%
      select(Study_ID, candidate_pair_id, response_var, year, dependence_cluster, review_weight) %>%
      mutate(scenario=scenario)
    for (spec in c('pooled', 'time')) {
      formula <- if (spec == 'time') lnRR_mean ~ post_fire_year else lnRR_mean ~ 1
      fit <- lm(formula, data=d, weights=review_weight)
      if (spec == 'pooled') {
        expected <- d %>% group_by(Study_ID, candidate_pair_id) %>%
          summarise(y=mean(lnRR_mean), .groups='drop') %>% group_by(Study_ID) %>%
          summarise(y=mean(y), .groups='drop') %>% summarise(y=mean(y)) %>% pull(y)
        stopifnot(abs(unname(coef(fit)[1])-expected) < 1e-10)
      }
      for (clustering in c('study', 'study_fire_component')) {
        group <- if (clustering == 'study') d$Study_ID else d$dependence_cluster
        V <- vcovCR(fit, cluster=group, type='CR2', inverse_var=FALSE,
                    target=rep(1, nrow(d)))
        tests <- coef_test(fit, vcov=V, test='Satterthwaite')
        intervals <- conf_int(fit, vcov=V, test='Satterthwaite')
        stopifnot(all(is.finite(tests$SE)), all(tests$df_Satt > 0))
        id <- paste(scenario, analyte, spec, clustering)
        coefficients[[id]] <- tibble(scenario=scenario, response_var=analyte,
          specification=spec, clustering=clustering, term=names(coef(fit)),
          estimate=unname(coef(fit)), SE=tests$SE, df=tests$df_Satt,
          lower=intervals$CI_L, upper=intervals$CI_U,
          clusters=n_distinct(group), studies=n_distinct(d$Study_ID), rows=nrow(d),
          status=case_when(
            scenario == 'primary' & spec == 'pooled' & clustering == 'study_fire_component' ~ 'primary_synthesis',
            spec == 'time' ~ 'exploratory_time_association',
            TRUE ~ 'synthesis_sensitivity'),
          target='equal_study_equal_pair_equal_available_year',
          uncertainty='CR2_identity_working_target_nonprecision_weights')
      }
      # Remove complete dependence components; all remaining study/pair totals
      # are already one and 1/n_pairs, so relative weights remain unchanged.
      for (cluster in unique(d$dependence_cluster)) {
        reduced <- filter(d, dependence_cluster != cluster)
        deleted <- lm(formula, data=reduced, weights=review_weight)
        influences[[paste(scenario, analyte, spec, cluster)]] <- tibble(
          scenario=scenario, response_var=analyte, specification=spec,
          omitted_cluster=cluster, term=names(coef(fit)),
          full_estimate=unname(coef(fit)), deleted_estimate=unname(coef(deleted)),
          change=unname(coef(deleted)-coef(fit)))
      }
    }
  }
}
results <- bind_rows(coefficients)
write_csv(results, file.path(out, 'balanced_CR2_results.csv'))
write_csv(bind_rows(influences), file.path(out, 'leave_component_out.csv'))
write_csv(bind_rows(weights), file.path(out, 'balanced_weights.csv'))
capture.output(sessionInfo(), file=file.path(out, 'session_info.txt'))
print(filter(results, scenario == 'primary', clustering == 'study_fire_component'), width=Inf)
message('Balanced synthesis complete: primary pooled study/fire-component CR2; other fits are supporting results.')
