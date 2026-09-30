# ==============================================================================
# Script: 07_make_results_agent_v1.R
# Purpose: Create provisional manuscript-facing tables and figures from saved
#          audit and model outputs. This script does not refit models.
# ==============================================================================

library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
audit_dir <- file.path(workflow_dir, "data", "audit")
table_dir <- file.path(workflow_dir, "output", "tables")
figure_dir <- file.path(workflow_dir, "output", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(
  file.path(audit_dir, "pair_structure.csv"),
  file.path(table_dir, "meta_model_summary.csv"),
  file.path(table_dir, "grouped_lasso_performance.csv"),
  file.path(table_dir, "lasso_selection_stability.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Run scripts 02 through 06 first. Missing: ", paste(missing_files, collapse = ", "))
}

pair_structure <- read_csv(file.path(audit_dir, "pair_structure.csv"), show_col_types = FALSE)
meta_summary <- read_csv(file.path(table_dir, "meta_model_summary.csv"), show_col_types = FALSE)
performance <- read_csv(file.path(table_dir, "grouped_lasso_performance.csv"), show_col_types = FALSE)
stability <- read_csv(file.path(table_dir, "lasso_selection_stability.csv"), show_col_types = FALSE)

dataset_structure_table <- pair_structure %>%
  mutate(
    pairing_status = if_else(
      n_pending_pair_rows > 0,
      "Provisional: co-author confirmation pending",
      "Confirmed"
    )
  )
write_csv(dataset_structure_table, file.path(table_dir, "dataset_structure_table.csv"))

pooled_effect_source <- meta_summary %>%
  filter(model == "intercept_only", term %in% c("intrcpt", "(Intercept)", "term_1")) %>%
  mutate(
    response_var = factor(response_var, levels = c("DOC", "NO3")),
    model_label = "Reported variance"
  )

if (nrow(pooled_effect_source) > 0) {
  pooled_effect_plot <- ggplot(
    pooled_effect_source,
    aes(x = estimate, y = response_var)
  ) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.15) +
    geom_point(size = 3) +
    theme_bw() +
    labs(x = "Annual log response ratio", y = NULL)

  ggsave(
    file.path(figure_dir, "pooled_effects.png"),
    pooled_effect_plot, width = 7, height = 4.5, units = "in", dpi = 300
  )
  write_csv(pooled_effect_source, file.path(table_dir, "pooled_effects_figure_data.csv"))
}

performance_plot <- ggplot(
  performance,
  aes(x = model, y = RMSE, fill = response_var)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_bw() +
  labs(x = NULL, y = "Held-out RMSE", fill = "Analyte") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(
  file.path(figure_dir, "predictive_performance.png"),
  performance_plot, width = 8, height = 5, units = "in", dpi = 300
)
write_csv(performance, file.path(table_dir, "predictive_performance_figure_data.csv"))

stability_source <- stability %>%
  filter(scenario == "lasso_family_balanced") %>%
  mutate(predictor = forcats::fct_reorder(predictor, selection_frequency))

stability_plot <- ggplot(
  stability_source,
  aes(x = selection_frequency, y = predictor, colour = stability_class)
) +
  geom_vline(xintercept = c(0.40, 0.75), linetype = 2, colour = "grey70") +
  geom_point(size = 2.5) +
  facet_wrap(~ response_var) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Study-bootstrap selection frequency", y = NULL, colour = "Class")

ggsave(
  file.path(figure_dir, "predictor_stability.png"),
  stability_plot, width = 9, height = 6, units = "in", dpi = 300
)
write_csv(stability_source, file.path(table_dir, "predictor_stability_figure_data.csv"))

message("Wrote provisional result tables to: ", table_dir)
message("Wrote provisional figures to: ", figure_dir)
message("Do not use for final reporting until pairing confirmation and audit review are complete.")
