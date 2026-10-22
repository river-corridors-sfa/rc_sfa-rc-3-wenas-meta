# Figure 3: study-bootstrap predictor selection stability.
library(tidyverse)
library(here)

root <- here("agent_workflows", "vibe_coding")
table_dir <- file.path(root, "output/paper/tables")
figure_dir <- file.path(root, "output/paper/figures/figure_3_lasso_results")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

stability <- read_csv(file.path(table_dir, "selection_stability.csv"),
                      show_col_types = FALSE) %>%
  left_join(read_csv(file.path(root, "config/issue6_revised_primary_dictionary.csv"),
                     show_col_types = FALSE) %>% select(predictor, predictor_label),
            by = "predictor") %>%
  mutate(response_var = factor(response_var, levels = c("NO3", "DOC"),
                               labels = c("Nitrate", "DOC")),
         predictor_key = paste(response_var, predictor_label, sep = "___"))
predictor_order <- stability %>%
  arrange(selection_frequency, predictor_label) %>%
  pull(predictor_key)
stability <- stability %>%
  mutate(predictor_key = factor(predictor_key, levels = predictor_order))

nitrate_color <- "#4386B5"
doc_color <- "#D49B47"
ink <- "#222222"
paper_theme <- theme_classic(base_size = 11) +
  theme(text = element_text(colour = ink),
        axis.text = element_text(colour = ink, size = 9),
        axis.title = element_text(colour = ink, size = 10),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = ink, linewidth = .45),
        panel.border = element_rect(colour = ink, fill = NA, linewidth = .45),
        strip.background = element_rect(fill = "white", colour = ink,
                                        linewidth = .45),
        strip.text = element_text(colour = ink, size = 10),
        legend.position = "none")

stability_plot <- ggplot(stability,
                         aes(x = selection_frequency, y = predictor_key,
                             colour = response_var)) +
  geom_segment(aes(x = 0, xend = selection_frequency,
                   y = predictor_key, yend = predictor_key),
               colour = "#D4D8DA", linewidth = .6) +
  geom_point(size = 2.8) +
  geom_text(aes(label = scales::percent(selection_frequency, accuracy = .1)),
            nudge_x = .018, hjust = 0, size = 2.8, colour = ink) +
  facet_wrap(~response_var, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = c("Nitrate" = nitrate_color,
                                 "DOC" = doc_color)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .25),
                     labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
  labs(x = "Bootstrap selection frequency", y = NULL) +
  paper_theme +
  theme(axis.text.y = element_text(colour = ink, size = 8))

ggsave(file.path(figure_dir, "figure_3_lasso_results.png"), stability_plot,
       width = 12, height = 6, dpi = 300, bg = "white")
message("Saved Figure 3 from the final primary-set selection table.")
