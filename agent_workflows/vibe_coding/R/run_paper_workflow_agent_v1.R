# Run from the repository root. See ../README.md for modes and prerequisites.
# No arguments preserves the full workflow from prepared data.
library(here)

args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c('--help', '-h'))) {
  cat('Usage: Rscript agent_workflows/vibe_coding/R/run_paper_workflow_agent_v1.R [prepare|primary|sensitivities|figures|all] [--dry-run]\n',
      'Default: all. PAPER_FROM_SOURCE=1 rebuilds source data with all.\n',
      'prepare always rebuilds source data; other modes use saved inputs.\n',
      '--dry-run prints the execution order without running scripts.\n', sep = '')
} else {
  dry_run <- '--dry-run' %in% args
  modes <- args[args != '--dry-run']
  if (length(modes) > 1 || (length(modes) == 1 &&
      !modes %in% c('prepare', 'primary', 'sensitivities', 'figures', 'all'))) {
    stop('Choose one mode: prepare, primary, sensitivities, figures, or all. Use --help for usage.')
  }
  mode <- if (length(modes)) modes else 'all'
  from_source <- Sys.getenv('PAPER_FROM_SOURCE', '0') == '1'
  if (from_source && !mode %in% c('prepare', 'all')) {
    stop('PAPER_FROM_SOURCE=1 applies to prepare or all only. Run prepare first, or unset it.')
  }
  # The primary output location must not depend on a leftover sensitivity override.
  overrides <- c('PAPER_SCENARIO', 'PAPER_WEIGHT_METHOD', 'PAPER_MODEL_TABLE')
  if (any(nzchar(Sys.getenv(overrides)))) {
    stop('Unset PAPER_SCENARIO, PAPER_WEIGHT_METHOD, and PAPER_MODEL_TABLE before using the runner; use script 11 directly for custom scenarios.')
  }
  root <- here('agent_workflows', 'vibe_coding')
  script_dir <- file.path(root, 'R')
  preparation <- c(
    'preparation/01_read_and_gapfill_preserve_observations_agent_v1.R',
    'preparation/01_pairing_review_workbook_agent_v1.R',
    'preparation/01b_promote_reviewed_pairings_agent_v1.R',
    'preparation/01c_apply_reviewed_sites_agent_v1.R',
    'preparation/02_prepare_analysis_data_agent_v1.R',
    'preparation/03_audit_pairs_and_predictors_agent_v1.R'
  )
  audits <- c('checks/16_verify_paper_inventory_agent_v1.R',
              'checks/17_audit_sampling_support_agent_v1.R')
  primary <- 'analysis/11_fit_issue6_revised_primary_agent_v1.R'
  sensitivities <- c('sensitivities/18_run_weighting_sensitivities_agent_v1.R',
                     'sensitivities/19_fit_working_covariance_inference_agent_v1.R')
  figures <- c(
    'figures/13_plot_paper_maps_agent_v1.R',
    'figures/08_plot_response_boxplots_agent_v1.R',
    'figures/14_plot_paper_lasso_results_agent_v1.R',
    'figures/15_plot_paper_predictor_correlations_agent_v1.R',
    'figures/12_plot_issue6_revised_primary_violins_agent_v1.R'
  )
  steps <- switch(mode,
    prepare = c(preparation, audits),
    primary = c(audits, primary),
    sensitivities = c(audits, sensitivities),
    figures = figures,
    all = c(if (from_source) preparation, audits, primary, sensitivities, figures)
  )
  missing_scripts <- steps[!file.exists(file.path(script_dir, steps))]
  if (length(missing_scripts)) stop('Missing workflow scripts: ', paste(missing_scripts, collapse = ', '))
  message('Paper workflow mode: ', mode, if (dry_run) ' (dry run)' else '')
  if (dry_run) {
    cat(paste(steps, collapse = '\n'), '\n', sep = '')
  } else {
    required <- if (mode == 'prepare' || (mode == 'all' && from_source)) character() else
      'data/derived/lasso_model_table.csv'
    if (mode %in% c('primary', 'sensitivities', 'all') && !from_source) required <- c(required,
      'config/reviewed_site_selection.csv',
      'data/derived/reviewed_sites_agent_v1/effect_sizes_daily.csv')
    if (mode == 'sensitivities') required <- c(required, 'output/paper/tables/performance_equal_study.csv')
    if (mode == 'figures') required <- c(required,
      'output/paper/tables/selection_stability.csv',
      'config/issue6_revised_primary_dictionary.csv',
      'config/predictor_dictionary.csv', 'config/issue6_revised_primary_predictors.csv')
    missing_inputs <- required[!file.exists(file.path(root, required))]
    if (length(missing_inputs)) stop('Missing saved inputs: ', paste(missing_inputs, collapse = ', '),
      '. Run prepare and/or primary first; see agent_workflows/vibe_coding/README.md.')
    dir.create(file.path(root, 'data/audit'), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(root, 'output/paper/tables'), recursive = TRUE, showWarnings = FALSE)
    for (step in steps) {
      message('Running ', step)
      source(file.path(script_dir, step), local = new.env(parent = globalenv()))
    }
    message('Paper workflow complete (', mode, '). Outputs: agent_workflows/vibe_coding/output/paper/')
  }
}
