# ==============================================================================
# Script: run_all_agent_v1.R
# Purpose: Run the provisional agent analysis workflow from pair adoption through
#          manuscript-facing outputs, with a Markdown run report.
# ==============================================================================

library(here)

required_packages <- c(
  "tidyverse", "here", "lubridate", "metafor", "glmnet", "forcats"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

workflow_dir <- here("agent_workflows", "vibe_coding")
script_dir <- file.path(workflow_dir, "R")
config_dir <- file.path(workflow_dir, "config")
derived_dir <- file.path(workflow_dir, "data", "derived")
audit_dir <- file.path(workflow_dir, "data", "audit")
output_dir <- file.path(workflow_dir, "output")
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")
log_dir <- file.path(output_dir, "logs")
figure_dir <- file.path(output_dir, "figures")
report_dir <- file.path(output_dir, "reports")
report_path <- file.path(report_dir, "run_all_agent_v1_report.md")

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

predictor_labels <- c(
  "(Intercept)" = "Intercept",
  "intrcpt" = "Intercept",
  "post_fire_year" = "Post-fire year",
  "time_since_fire" = "Time since fire",
  "burn_percent_fire_year" = "Burned watershed area (%)",
  "burn_sev_high" = "High-severity burn (%)",
  "burn_sev_mod" = "Moderate-severity burn (%)",
  "burn_sev_low" = "Low-severity burn (%)",
  "Area_watershed_km" = "Watershed area (km)",
  "maxelevsmo" = "Maximum smoothed elevation",
  "slope" = "Watershed slope",
  "tmean8110ws" = "1981-2010 mean temperature",
  "precip8110ws" = "1981-2010 precipitation",
  "forest_cover" = "Forest cover (%)",
  "urban_cover" = "Urban cover (%)",
  "grassland_cover" = "Grassland cover (%)",
  "wetland_cover" = "Wetland cover (%)",
  "ag_cover" = "Agricultural cover (%)",
  "omws" = "Soil organic matter",
  "rckdepws" = "Depth to bedrock",
  "clayws" = "Soil clay content",
  "glacial_till" = "Glacial till (%)",
  "bfiws" = "Baseflow index",
  "permws" = "Soil permeability",
  "runoffws" = "Mean annual runoff"
)

format_predictor_name <- function(values) {
  original_values <- as.character(values)
  readable_values <- unname(predictor_labels[original_values])
  ifelse(is.na(readable_values), original_values, readable_values)
}

format_predictor_text <- function(lines) {
  if (length(lines) == 0) return(lines)

  output_lines <- as.character(lines)
  raw_names <- names(predictor_labels)
  raw_names <- raw_names[order(nchar(raw_names), decreasing = TRUE)]

  for (raw_name in raw_names) {
    output_lines <- gsub(
      raw_name,
      predictor_labels[[raw_name]],
      output_lines,
      fixed = TRUE
    )
  }

  output_lines
}

format_predictor_columns <- function(data) {
  predictor_columns <- intersect(
    c("predictor", "predictor_1", "predictor_2", "term"),
    names(data)
  )
  if (length(predictor_columns) == 0) return(data)

  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(predictor_columns),
        format_predictor_name
      )
    )
}

relative_path <- function(path) {
  root <- normalizePath(here(), winslash = "/", mustWork = TRUE)
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root_prefix <- paste0(root, "/")
  if (startsWith(path_norm, root_prefix)) {
    return(substr(path_norm, nchar(root_prefix) + 1, nchar(path_norm)))
  }
  path_norm
}

append_md <- function(lines = "") {
  cat(paste(lines, collapse = "\n"), "\n", file = report_path, append = TRUE, sep = "")
}

append_code_block <- function(lines, language = "") {
  if (length(lines) == 0) lines <- "(none)"
  lines <- as.character(lines)
  lines <- format_predictor_text(lines)
  append_md(c(paste0("```", language), lines, "```"))
}

append_printed_table <- function(data, title = NULL, max_rows = 20) {
  if (!is.null(title)) append_md(c("", paste0("**", title, "**"), ""))
  if (is.null(data)) {
    append_md("_File missing or unreadable._")
    return(invisible(NULL))
  }
  if (nrow(data) == 0) {
    append_md("_No rows._")
    return(invisible(NULL))
  }

  data <- format_predictor_columns(data)
  total_rows <- nrow(data)
  display_data <- utils::head(data, max_rows)
  append_code_block(
    capture.output(print(tibble::as_tibble(display_data), n = max_rows, width = Inf))
  )
  if (total_rows > max_rows) {
    append_md(paste0("_Showing first ", max_rows, " of ", total_rows, " rows._"))
  }
}

read_csv_if_present <- function(path) {
  if (!file.exists(path)) return(NULL)
  if (is.na(file.info(path)$size) || file.info(path)$size == 0) {
    return(tibble::tibble())
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(error) {
      append_md(paste0("_Could not read ", relative_path(path), ": ", conditionMessage(error), "_"))
      NULL
    }
  )
}

csv_status <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(file = relative_path(path), status = "missing", rows = NA_integer_, size_kb = NA_real_))
  }
  size_bytes <- file.info(path)$size
  rows <- if (is.na(size_bytes) || size_bytes == 0) {
    0L
  } else {
    data <- read_csv_if_present(path)
    if (is.null(data)) NA_integer_ else nrow(data)
  }
  tibble::tibble(
    file = relative_path(path),
    status = ifelse(size_bytes == 0, "empty", "present"),
    rows = rows,
    size_kb = round(size_bytes / 1024, 1)
  )
}

append_file_status <- function(paths, title) {
  status <- dplyr::bind_rows(lapply(paths, csv_status))
  append_printed_table(status, title = title, max_rows = nrow(status))
}

append_failure_log <- function(path, title) {
  failures <- read_csv_if_present(path)
  append_printed_table(failures, title = title, max_rows = 20)
}

append_pairing_diagnostics <- function() {
  analysis_pairs <- read_csv_if_present(file.path(config_dir, "pairing_decisions_analysis.csv"))
  append_file_status(
    c(file.path(config_dir, "pairing_decisions_analysis.csv")),
    "Generated Files"
  )
  if (is.null(analysis_pairs) || nrow(analysis_pairs) == 0) return(invisible(NULL))

  summary <- tibble::tibble(
    rows = nrow(analysis_pairs),
    candidate_pairs = dplyr::n_distinct(analysis_pairs$candidate_pair_id),
    included_pairs = sum(as.logical(analysis_pairs$Include_Analysis), na.rm = TRUE),
    pending_confirmation = sum(analysis_pairs$Coauthor_Confirmation == "pending", na.rm = TRUE),
    shared_reference_pairs = sum(analysis_pairs$shared_reference, na.rm = TRUE)
  )
  append_printed_table(summary, "Pairing Summary", max_rows = 20)

  counts <- analysis_pairs |>
    dplyr::count(
      Pairing_Type_Analysis,
      Coauthor_Confirmation,
      shared_reference,
      name = "n"
    )
  append_printed_table(counts, "Pairing Type Counts", max_rows = 20)
}

append_model_table_diagnostics <- function() {
  model_table <- read_csv_if_present(file.path(derived_dir, "lasso_model_table.csv"))
  append_file_status(
    c(
      file.path(derived_dir, "lasso_model_table.csv"),
      file.path(audit_dir, "geospatial_site_join.csv"),
      file.path(audit_dir, "approved_pairs_without_effect_sizes.csv")
    ),
    "Generated Files"
  )
  if (is.null(model_table) || nrow(model_table) == 0) return(invisible(NULL))

  response_summary <- model_table |>
    dplyr::group_by(response_var) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_studies = dplyr::n_distinct(Study_ID),
      n_comparisons = dplyr::n_distinct(Comparison_ID),
      n_pairs = dplyr::n_distinct(candidate_pair_id),
      finite_lnRR = sum(is.finite(lnRR_mean)),
      usable_variances = sum(variance_status == "usable", na.rm = TRUE),
      pending_pair_rows = sum(pairing_confirmation_pending, na.rm = TRUE),
      .groups = "drop"
    )
  append_printed_table(response_summary, "Model Table by Analyte", max_rows = 20)

  variance_summary <- model_table |>
    dplyr::count(response_var, variance_status, name = "n")
  append_printed_table(variance_summary, "Variance Status", max_rows = 20)
}

append_audit_diagnostics <- function() {
  append_file_status(
    c(
      file.path(audit_dir, "pair_structure.csv"),
      file.path(audit_dir, "shared_reference_structure.csv"),
      file.path(audit_dir, "response_audit.csv"),
      file.path(audit_dir, "predictor_missingness.csv"),
      file.path(audit_dir, "predictor_correlations.csv"),
      file.path(config_dir, "predictor_dictionary.csv")
    ),
    "Generated Files"
  )

  append_printed_table(read_csv_if_present(file.path(audit_dir, "pair_structure.csv")), "Pair Structure", max_rows = 20)
  append_printed_table(read_csv_if_present(file.path(audit_dir, "response_audit.csv")), "Response Audit", max_rows = 20)

  predictor_dictionary <- read_csv_if_present(file.path(config_dir, "predictor_dictionary.csv"))
  if (!is.null(predictor_dictionary) && nrow(predictor_dictionary) > 0) {
    include_value <- tolower(as.character(predictor_dictionary$include_primary)) %in% c("true", "t", "1")
    primary_predictors <- predictor_dictionary |>
      dplyr::filter(include_value) |>
      dplyr::select(
        predictor,
        predictor_group,
        transformation,
        proportion_missing,
        n_unique,
        decision_status
      )
    append_printed_table(primary_predictors, "Primary Predictor Set", max_rows = 20)
  }

  predictor_correlations <- read_csv_if_present(file.path(audit_dir, "predictor_correlations.csv"))
  if (!is.null(predictor_correlations) && nrow(predictor_correlations) > 0) {
    top_correlations <- predictor_correlations |>
      dplyr::arrange(dplyr::desc(abs(rho))) |>
      utils::head(10)
    append_printed_table(top_correlations, "Largest Absolute Spearman Correlations", max_rows = 10)
  }
}

append_meta_diagnostics <- function() {
  append_file_status(
    c(
      file.path(table_dir, "meta_model_summary.csv"),
      file.path(log_dir, "meta_model_failures.csv")
    ),
    "Generated Files"
  )

  meta_summary <- read_csv_if_present(file.path(table_dir, "meta_model_summary.csv"))
  if (!is.null(meta_summary) && nrow(meta_summary) > 0) {
    display_summary <- meta_summary |>
      dplyr::select(
        response_var,
        model,
        inference,
        term,
        estimate,
        std_error,
        ci_lower,
        ci_upper,
        p_value,
        k,
        n_studies,
        percent_change
      )
    append_printed_table(display_summary, "Meta-Analysis Summary", max_rows = 30)
  }

  append_failure_log(file.path(log_dir, "meta_model_failures.csv"), "Meta-Analysis Failures")
}

append_lasso_diagnostics <- function() {
  append_file_status(
    c(
      file.path(table_dir, "grouped_lasso_predictions.csv"),
      file.path(table_dir, "grouped_lasso_performance.csv"),
      file.path(table_dir, "grouped_lasso_coefficients.csv"),
      file.path(table_dir, "grouped_fold_assignments.csv"),
      file.path(log_dir, "grouped_lasso_failures.csv")
    ),
    "Generated Files"
  )

  performance <- read_csv_if_present(file.path(table_dir, "grouped_lasso_performance.csv"))
  append_printed_table(performance, "Leave-One-Study-Out Predictive Performance", max_rows = 20)

  coefficients <- read_csv_if_present(file.path(table_dir, "grouped_lasso_coefficients.csv"))
  if (!is.null(coefficients) && nrow(coefficients) > 0) {
    nonzero_summary <- coefficients |>
      dplyr::filter(predictor != "(Intercept)") |>
      dplyr::group_by(response_var, predictor) |>
      dplyr::summarise(
        selected_folds = sum(coefficient != 0),
        total_folds = dplyr::n(),
        selection_rate = selected_folds / total_folds,
        median_nonzero_coefficient = ifelse(
          any(coefficient != 0),
          stats::median(coefficient[coefficient != 0]),
          NA_real_
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(response_var, dplyr::desc(selection_rate), predictor)
    append_printed_table(nonzero_summary, "Nonzero LASSO Coefficients Across Outer Folds", max_rows = 30)
  }

  append_failure_log(file.path(log_dir, "grouped_lasso_failures.csv"), "Grouped LASSO Failures")
}

append_stability_diagnostics <- function() {
  append_file_status(
    c(
      file.path(table_dir, "bootstrap_coefficients.csv"),
      file.path(table_dir, "lasso_selection_stability.csv"),
      file.path(table_dir, "lasso_sensitivity_summary.csv"),
      file.path(log_dir, "bootstrap_failures.csv")
    ),
    "Generated Files"
  )

  stability <- read_csv_if_present(file.path(table_dir, "lasso_selection_stability.csv"))
  if (!is.null(stability) && nrow(stability) > 0) {
    top_stability <- stability |>
      dplyr::group_by(response_var, scenario) |>
      dplyr::slice_max(selection_frequency, n = 5, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(
        response_var,
        scenario,
        predictor,
        completed_iterations,
        selection_frequency,
        median_coefficient,
        stability_class
      ) |>
      dplyr::arrange(response_var, scenario, dplyr::desc(selection_frequency))
    append_printed_table(top_stability, "Top Predictor Stability by Scenario", max_rows = 40)
  }

  append_failure_log(file.path(log_dir, "bootstrap_failures.csv"), "Bootstrap Failures")
}

append_results_diagnostics <- function() {
  append_file_status(
    c(
      file.path(table_dir, "dataset_structure_table.csv"),
      file.path(table_dir, "pooled_effects_figure_data.csv"),
      file.path(table_dir, "predictive_performance_figure_data.csv"),
      file.path(table_dir, "predictor_stability_figure_data.csv")
    ),
    "Generated Tables"
  )

  append_printed_table(read_csv_if_present(file.path(table_dir, "dataset_structure_table.csv")), "Dataset Structure Table", max_rows = 20)
  append_printed_table(read_csv_if_present(file.path(table_dir, "pooled_effects_figure_data.csv")), "Pooled Effects Figure Data", max_rows = 20)
  append_printed_table(read_csv_if_present(file.path(table_dir, "predictive_performance_figure_data.csv")), "Predictive Performance Figure Data", max_rows = 20)
  append_printed_table(read_csv_if_present(file.path(table_dir, "predictor_stability_figure_data.csv")), "Predictor Stability Figure Data", max_rows = 20)

  figure_paths <- list.files(figure_dir, pattern = "\\.png$", full.names = TRUE)
  figure_inventory <- if (length(figure_paths) == 0) {
    tibble::tibble(file = character(), size_kb = numeric(), modified = character())
  } else {
    info <- file.info(figure_paths)
    tibble::tibble(
      file = vapply(figure_paths, relative_path, character(1)),
      size_kb = round(info$size / 1024, 1),
      modified = format(info$mtime, "%Y-%m-%d %H:%M:%S %Z")
    )
  }
  append_printed_table(figure_inventory, "Generated Figures", max_rows = 20)
}

append_script_diagnostics <- function(script_name) {
  switch(
    script_name,
    "01a_create_provisional_pairings_agent_v1.R" = append_pairing_diagnostics(),
    "02_prepare_analysis_data_agent_v1.R" = append_model_table_diagnostics(),
    "03_audit_pairs_and_predictors_agent_v1.R" = append_audit_diagnostics(),
    "04_fit_meta_analysis_agent_v1.R" = append_meta_diagnostics(),
    "05_fit_grouped_lasso_agent_v1.R" = append_lasso_diagnostics(),
    "06_run_stability_sensitivity_agent_v1.R" = append_stability_diagnostics(),
    "07_make_results_agent_v1.R" = append_results_diagnostics(),
    invisible(NULL)
  )
}

run_script_with_report <- function(script_name) {
  script_path <- file.path(script_dir, script_name)
  if (!file.exists(script_path)) stop("Missing workflow script: ", script_path)

  message("\n===== Running ", script_name, " =====")
  start_time <- Sys.time()
  append_md(c("", paste0("## ", script_name), ""))
  append_md(paste0("- Started: ", format(start_time, "%Y-%m-%d %H:%M:%S %Z")))

  printed_output <- character()
  messages <- character()
  warnings <- character()
  error_message <- NULL
  sink_start <- sink.number(type = "output")
  output_connection <- textConnection("printed_output", open = "w", local = TRUE)
  sink(output_connection, type = "output")

  completed <- tryCatch(
    {
      withCallingHandlers(
        {
          source(script_path, local = new.env(parent = globalenv()))
          TRUE
        },
        message = function(message_condition) {
          messages <<- c(messages, trimws(conditionMessage(message_condition)))
          invokeRestart("muffleMessage")
        },
        warning = function(warning_condition) {
          warnings <<- c(warnings, trimws(conditionMessage(warning_condition)))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(error) {
      error_message <<- conditionMessage(error)
      FALSE
    },
    finally = {
      while (sink.number(type = "output") > sink_start) sink(type = "output")
      close(output_connection)
    }
  )

  end_time <- Sys.time()
  append_md(c(
    paste0("- Finished: ", format(end_time, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- Runtime seconds: ", round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)),
    paste0("- Status: ", ifelse(completed, "completed", "failed"))
  ))

  append_md(c("", "### Console Messages", ""))
  append_code_block(messages)
  append_md(c("", "### Printed Output", ""))
  append_code_block(printed_output)
  append_md(c("", "### Warnings", ""))
  append_code_block(warnings)

  if (!completed) {
    append_md(c("", "### Error", ""))
    append_code_block(error_message)
    stop("Workflow failed in ", script_name, ". See report: ", report_path)
  }

  append_md(c("", "### Diagnostics", ""))
  append_script_diagnostics(script_name)
  message("Completed ", script_name)
}

scripts <- c(
  "01a_create_provisional_pairings_agent_v1.R",
  "02_prepare_analysis_data_agent_v1.R",
  "03_audit_pairs_and_predictors_agent_v1.R",
  "04_fit_meta_analysis_agent_v1.R",
  "05_fit_grouped_lasso_agent_v1.R",
  "06_run_stability_sensitivity_agent_v1.R",
  "07_make_results_agent_v1.R"
)

package_versions <- tibble::tibble(
  package = required_packages,
  version = vapply(
    required_packages,
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )
)

writeLines(
  c(
    "# Agent Workflow Run Report",
    "",
    paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- Repository: ", here()),
    paste0("- Workflow directory: ", workflow_dir),
    paste0("- N_BOOTSTRAP: ", Sys.getenv("N_BOOTSTRAP", unset = "100")),
    "",
    "This report captures console messages, printed output, warnings, and compact diagnostics after each workflow step.",
    "",
    "## Package Versions",
    "",
    "```",
    capture.output(print(package_versions, n = nrow(package_versions), width = Inf)),
    "```"
  ),
  report_path
)

for (script_name in scripts) {
  run_script_with_report(script_name)
}

append_md(c("", "## Final Output Inventory", ""))
append_file_status(
  c(
    file.path(config_dir, "pairing_decisions_analysis.csv"),
    file.path(derived_dir, "lasso_model_table.csv"),
    file.path(audit_dir, "pair_structure.csv"),
    file.path(audit_dir, "shared_reference_structure.csv"),
    file.path(audit_dir, "response_audit.csv"),
    file.path(audit_dir, "predictor_missingness.csv"),
    file.path(audit_dir, "predictor_correlations.csv"),
    file.path(config_dir, "predictor_dictionary.csv"),
    file.path(table_dir, "meta_model_summary.csv"),
    file.path(table_dir, "grouped_lasso_performance.csv"),
    file.path(table_dir, "lasso_selection_stability.csv"),
    file.path(table_dir, "lasso_sensitivity_summary.csv"),
    file.path(table_dir, "dataset_structure_table.csv"),
    file.path(table_dir, "pooled_effects_figure_data.csv"),
    file.path(table_dir, "predictive_performance_figure_data.csv"),
    file.path(table_dir, "predictor_stability_figure_data.csv"),
    file.path(log_dir, "meta_model_failures.csv"),
    file.path(log_dir, "grouped_lasso_failures.csv"),
    file.path(log_dir, "bootstrap_failures.csv")
  ),
  "CSV Outputs"
)
append_results_diagnostics()

append_md(c("", "## Session Info", ""))
append_code_block(capture.output(sessionInfo()))
append_md(c("", "## Workflow Status", "", "Workflow completed."))

message("\nWorkflow completed.")
message("Markdown report written to: ", report_path)
