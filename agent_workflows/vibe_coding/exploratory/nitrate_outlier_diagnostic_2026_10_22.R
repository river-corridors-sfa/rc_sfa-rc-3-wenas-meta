# Diagnostic exclusions only; primary inputs and results are preserved.
# Reuse the primary definitions without executing its output or bootstrap steps.
expressions <- parse("agent_workflows/vibe_coding/R/analysis/01_fit_issue6_revised_primary_agent_v1.R")
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1]], as.name("write_csv"))) {
    if (exists("fit_nested")) break
    next
  }
  eval(expression)
}
out <- file.path(root, "exploratory/output/nitrate_outlier_diagnostic_2026_10_22")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
rows <- filter(data, response_var == "NO3")
candidates <- sets$NO3$revised_primary
low <- rows$row_id[which.min(rows$lnRR_mean)]
high <- rows$row_id[which.max(rows$lnRR_mean)]
second_low <- rows$row_id[order(rows$lnRR_mean)[2]]
extreme_studies <- rows$Study_ID[match(c(low, high), rows$row_id)]
scenarios <- list(
  all_records = rows,
  omit_lowest_record = filter(rows, row_id != low),
  omit_highest_record = filter(rows, row_id != high),
  omit_lowest_and_highest = filter(rows, !row_id %in% c(low, high)),
  omit_two_lowest_records = filter(rows, !row_id %in% c(low, second_low)),
  omit_Crandall_study = filter(rows, Study_ID != extreme_studies[1]),
  omit_Rhea_study = filter(rows, Study_ID != extreme_studies[2]),
  omit_Crandall_and_Rhea = filter(rows, !Study_ID %in% extreme_studies),
  omit_Hauer_Spencer_study = filter(rows, Study_ID != "Hauer & Spencer 1998")
)
# Expose the existing tuning calculations without changing fitting decisions.
fit_diagnostic <- fit_nested
fit_body <- body(fit_diagnostic)
fit_body[[length(fit_body)]] <- quote(list(
  glmnet.fit = fit, lambda.1se = lambdas[selected], lambda.min = lambdas[best],
  tuning = tibble(lambda = lambdas, mean_study_MSE = avg, SE = se),
  errors = errors, studies = studies))
body(fit_diagnostic) <- fit_body
summaries <- list()
fold_results <- list()
for (scenario_name in names(scenarios)) {
  current <- scenarios[[scenario_name]]
  full <- fit_diagnostic(current, candidates)
  write_csv(full$tuning, file.path(out, paste0(scenario_name, "_tuning.csv")))
  predictions <- list()
  for (study in sort(unique(current$Study_ID))) {
    train <- filter(current, Study_ID != study)
    test <- filter(current, Study_ID == study)
    prep <- prepare(train, test, candidates)
    fit <- fit_nested(train, candidates)
    predicted <- as.numeric(predict(fit, newx = as.matrix(prep$test[prep$predictors]), s = "lambda.1se"))
    baseline <- weighted.mean(train$lnRR_mean, weights_for(train))
    predictions[[study]] <- tibble(Study_ID = study,
      lasso_MSE = mean((test$lnRR_mean - predicted)^2),
      intercept_MSE = mean((test$lnRR_mean - baseline)^2),
      n_selected = sum(as.matrix(coef(fit, s = "lambda.1se"))[-1] != 0),
      lambda_1se = fit$lambda.1se)
  }
  predictions <- bind_rows(predictions)
  fold_results[[scenario_name]] <- mutate(predictions, scenario = scenario_name)
  beta <- as.matrix(coef(full$glmnet.fit, s = full$lambda.1se))[-1, 1]
  selected <- names(beta)[beta != 0]
  best <- which.min(full$tuning$mean_study_MSE)
  summaries[[scenario_name]] <- tibble(scenario = scenario_name,
    n = nrow(current), n_studies = n_distinct(current$Study_ID),
    lambda_1se = full$lambda.1se, lambda_min = full$lambda.min,
    full_selected = paste(selected, collapse = "; "),
    outer_folds_with_predictors = sum(predictions$n_selected > 0),
    equal_study_lasso_RMSE = sqrt(mean(predictions$lasso_MSE)),
    equal_study_intercept_RMSE = sqrt(mean(predictions$intercept_MSE)),
    tuning_best_MSE = full$tuning$mean_study_MSE[best],
    tuning_best_SE = full$tuning$SE[best],
    tuning_intercept_MSE = full$tuning$mean_study_MSE[1],
    full_jerr = full$glmnet.fit$jerr)
}
summary <- bind_rows(summaries)
write_csv(summary, file.path(out, "summary.csv"))
write_csv(bind_rows(fold_results), file.path(out, "outer_folds.csv"))
write_csv(rows %>% arrange(lnRR_mean) %>% select(row_id, Study_ID,
  candidate_pair_id, year, lnRR_mean, annual_mean_burn, annual_mean_reference,
  n_both_observed), file.path(out, "effect_sizes.csv"))
print(summary, width = Inf)
