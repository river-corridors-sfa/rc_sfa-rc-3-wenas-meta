# Distributions and sample variances of the predictors used in the issue #6 LASSOs.
library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
model_table <- read_csv(file.path(workflow_dir, "data/derived/lasso_model_table.csv"),
                        show_col_types = FALSE) %>%
  filter(response_var %in% c("DOC", "NO3"), is.finite(lnRR_mean))
dictionary <- read_csv(file.path(workflow_dir, "config/issue6_predictor_dictionary.csv"),
                       show_col_types = FALSE)
output_dir <- file.path(workflow_dir, "output/issue_6_curated_lasso")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_summaries <- list()
for (analyte in c("DOC", "NO3")) {
  current_flag <- if (analyte == "DOC") "DOC_current" else "NO3_current"
  curated_flag <- if (analyte == "DOC") "DOC_curated" else "NO3_curated"
  plotted_predictors <- dictionary %>%
    filter(.data[[current_flag]] | .data[[curated_flag]] | sensitivity_bfi) %>%
    mutate(role = case_when(
      .data[[curated_flag]] ~ "Curated model",
      sensitivity_bfi ~ "Baseflow sensitivity",
      TRUE ~ "Current set only"
    )) %>%
    select(predictor, predictor_label, role)

  values <- model_table %>%
    filter(response_var == analyte) %>%
    select(candidate_pair_id, all_of(plotted_predictors$predictor)) %>%
    pivot_longer(-candidate_pair_id, names_to = "predictor", values_to = "value") %>%
    group_by(predictor, candidate_pair_id) %>%
    filter(predictor == "post_fire_year" | row_number() == 1) %>%
    ungroup() %>%
    filter(is.finite(value)) %>%
    left_join(plotted_predictors, by = "predictor") %>%
    mutate(analyte = analyte,
           observation_unit = if_else(predictor == "post_fire_year", "pair-year", "watershed pair"))

  summary <- values %>%
    group_by(analyte, predictor, predictor_label, role, observation_unit) %>%
    summarise(n = n(), sample_variance = if (n() > 1) var(value) else NA_real_,
              minimum = min(value), q1 = as.numeric(quantile(value, .25)),
              median = median(value), q3 = as.numeric(quantile(value, .75)),
              maximum = max(value), .groups = "drop") %>%
    mutate(panel_label = paste0(predictor_label, "\nn = ", n,
                                "  |  variance = ", signif(sample_variance, 3)))
  plot_summaries[[analyte]] <- summary %>% select(-panel_label)
  values <- values %>%
    left_join(summary %>% select(predictor, panel_label), by = "predictor") %>%
    mutate(panel_label = factor(panel_label, levels = summary$panel_label))

  plot <- ggplot(values, aes(x = "", y = value, fill = role)) +
    geom_boxplot(width = .42, outlier.shape = NA, alpha = .72, linewidth = .45) +
    geom_point(position = position_jitter(width = .12, height = 0, seed = 42),
               alpha = .57, size = 1.55, shape = 21, stroke = .2) +
    facet_wrap(~ panel_label, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = c("Curated model" = "#4A85A8",
                                 "Current set only" = "#D39B55",
                                 "Baseflow sensitivity" = "#8A75AA")) +
    labs(title = paste0(if_else(analyte == "NO3", "Nitrate", "DOC"),
                        " LASSO predictor distributions"),
         subtitle = "Raw values; each panel has its own vertical scale",
         x = NULL, y = "Predictor value (units in label where known)", fill = "Predictor set",
         caption = paste(
           "Watershed attributes: one value per pair; post-fire year: one value per pair-year.",
           "Sample variance is in squared units of each predictor. Boxes show median and IQR; whiskers extend to 1.5 × IQR.",
           sep = "\n")) +
    theme_bw(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 9, face = "bold"),
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_text(hjust = 0, size = 8),
          legend.position = "bottom")
  file_name <- paste0(tolower(analyte), "_predictor_boxplots.png")
  ggsave(file.path(output_dir, file_name), plot, width = 12, height = 11,
         dpi = 300, bg = "white")
  message("Saved ", file_name)

  violin_plot <- ggplot(values, aes(x = "", y = value, fill = role)) +
    geom_violin(width = .72, trim = TRUE, scale = "width", alpha = .7,
                linewidth = .45) +
    stat_summary(fun = median, geom = "crossbar", width = .18,
                 colour = "#263238", linewidth = .5) +
    geom_point(position = position_jitter(width = .12, height = 0, seed = 42),
               alpha = .57, size = 1.55, shape = 21, stroke = .2) +
    facet_wrap(~ panel_label, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = c("Curated model" = "#4A85A8",
                                 "Current set only" = "#D39B55",
                                 "Baseflow sensitivity" = "#8A75AA")) +
    labs(title = paste0(if_else(analyte == "NO3", "Nitrate", "DOC"),
                        " LASSO predictor distributions"),
         subtitle = "Raw values; each panel has its own vertical scale",
         x = NULL, y = "Predictor value (units in label where known)", fill = "Predictor set",
         caption = paste(
           "Watershed attributes: one value per pair; post-fire year: one value per pair-year.",
           "Sample variance is in squared units. Violin width shows smoothed density; bars mark medians; dots show data.",
           sep = "\n")) +
    theme_bw(base_size = 11) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 9, face = "bold"),
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_text(hjust = 0, size = 8),
          legend.position = "bottom")
  violin_name <- paste0(tolower(analyte), "_predictor_violins.png")
  ggsave(file.path(output_dir, violin_name), violin_plot, width = 12, height = 11,
         dpi = 300, bg = "white")
  message("Saved ", violin_name)
}

write_csv(bind_rows(plot_summaries), file.path(output_dir, "predictor_distribution_summary.csv"))
