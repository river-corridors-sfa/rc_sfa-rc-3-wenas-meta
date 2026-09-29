# One violin figure for every predictor in the user-defined DOC and nitrate sets.
library(tidyverse)
library(here)

root <- here("agent_workflows", "vibe_coding")
out <- file.path(root, "output", "issue_6_revised_primary")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

config <- read_csv(file.path(root, "config/issue6_revised_primary_predictors.csv"),
                   show_col_types = FALSE)
dictionary <- read_csv(file.path(root, "config/predictor_dictionary.csv"),
                       show_col_types = FALSE)
data <- read_csv(file.path(root, "data/derived/lasso_model_table.csv"),
                 show_col_types = FALSE) %>%
  filter(response_var %in% c("DOC", "NO3"), is.finite(lnRR_mean))
predictors <- unique(config$predictor)
if (!all(predictors %in% names(data))) stop("A configured predictor is missing from the model table.")

values <- data %>%
  select(response_var, candidate_pair_id, all_of(predictors)) %>%
  pivot_longer(all_of(predictors), names_to = "predictor", values_to = "value") %>%
  inner_join(config %>% select(response_var, predictor),
             by = c("response_var", "predictor")) %>%
  group_by(response_var, predictor, candidate_pair_id) %>%
  filter(predictor == "post_fire_year" | row_number() == 1) %>%
  ungroup() %>%
  filter(is.finite(value)) %>%
  mutate(response_var = factor(response_var, levels = c("DOC", "NO3"),
                               labels = c("DOC", "Nitrate")))

summary <- values %>%
  group_by(response_var, predictor) %>%
  summarise(n = n(), sample_variance = var(value),
            minimum = min(value), q1 = as.numeric(quantile(value, .25)),
            median = median(value), q3 = as.numeric(quantile(value, .75)),
            maximum = max(value), .groups = "drop") %>%
  left_join(dictionary %>% select(predictor, predictor_label), by = "predictor") %>%
  mutate(observation_unit = if_else(predictor == "post_fire_year", "pair-year", "watershed pair"))
write_csv(summary, file.path(out, "revised_primary_predictor_distribution_summary.csv"))

panel_labels <- summary %>%
  mutate(detail = paste0(as.character(response_var), " n=", n,
                         ", s²=", signif(sample_variance, 3))) %>%
  group_by(predictor, predictor_label) %>%
  summarise(panel_label = paste0(first(predictor_label), "\n",
                                 paste(detail, collapse = " | ")), .groups = "drop")
values <- values %>%
  left_join(panel_labels %>% select(predictor, panel_label), by = "predictor") %>%
  mutate(panel_label = factor(panel_label,
                              levels = panel_labels$panel_label[match(predictors, panel_labels$predictor)]))

figure <- ggplot(values, aes(x = response_var, y = value, fill = response_var)) +
  geom_violin(trim = TRUE, scale = "width", alpha = .68, linewidth = .45,
              width = .78) +
  stat_summary(fun = median, geom = "crossbar", width = .17,
               colour = "#263238", linewidth = .5) +
  geom_point(position = position_jitter(width = .11, height = 0, seed = 42),
             shape = 21, size = 1.45, stroke = .2, alpha = .6) +
  facet_wrap(~panel_label, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c(DOC = "#4A85A8", Nitrate = "#D39B55"), drop = FALSE) +
  labs(title = "Revised primary LASSO predictor distributions",
       subtitle = "All variables in the DOC and nitrate primary sets; raw values and separate scales",
       x = NULL, y = "Predictor value (units in panel label where known)",
       caption = paste(
         "Watershed attributes: one value per pair; post-fire year: one value per pair-year.",
         "Violin width is smoothed density; bars mark medians and dots show observations. s² is sample variance in squared units.",
         "Soil organic matter is DOC-only; clay content is nitrate-only.", sep = "\n")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 8.5, face = "bold"),
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_text(hjust = 0, size = 8))

ggsave(file.path(out, "all_revised_primary_predictor_violins.png"), figure,
       width = 13, height = 11, dpi = 300, bg = "white")
message("Saved combined revised-primary violin plot to ", out)
