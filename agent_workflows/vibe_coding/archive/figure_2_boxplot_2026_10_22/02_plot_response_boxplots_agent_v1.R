# Plot the annual responses in the latest prepared model table, without refitting.
library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
figure_dir <- file.path(workflow_dir, "output", "paper", "figures", "figure_2_response_boxplots")
table_dir <- file.path(workflow_dir, "output", "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

responses <- read_csv(
  file.path(workflow_dir, "data", "derived", "lasso_model_table.csv"),
  show_col_types = FALSE
) %>%
  filter(response_var %in% c("NO3", "DOC"), is.finite(lnRR_mean)) %>%
  mutate(analyte = factor(response_var, levels = c("NO3", "DOC"),
                          labels = c("Nitrate", "DOC")))

# Keep finite responses even when their sampling variance is unavailable:
# this is an unweighted descriptive plot of the full response dataset.
response_summary <- responses %>%
  group_by(analyte) %>%
  summarise(n = n(), studies = n_distinct(Study_ID),
            minimum = min(lnRR_mean), q1 = quantile(lnRR_mean, 0.25),
            median = median(lnRR_mean), q3 = quantile(lnRR_mean, 0.75),
            maximum = max(lnRR_mean), .groups = "drop")
axis_labels <- setNames(
  paste0(response_summary$analyte, "\nn = ", response_summary$n,
         "; ", response_summary$studies, " studies"),
  response_summary$analyte
)

response_plot <- ggplot(responses, aes(analyte, lnRR_mean, fill = analyte)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#555555",
             linewidth = 0.45) +
  geom_boxplot(width = 0.45, alpha = 0.55, outlier.shape = NA,
               linewidth = 0.55, colour = "#222222") +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 42),
             shape = 21, size = 2, alpha = 0.78, stroke = 0.25,
             colour = "#222222") +
  scale_fill_manual(values = c(Nitrate = "#4386B5", DOC = "#D49B47")) +
  scale_x_discrete(labels = axis_labels) +
  scale_y_continuous(expand = expansion(mult = c(0.035, 0.045))) +
  labs(x = NULL, y = "Annual log response ratio (lnRR)") +
  theme_classic(base_size = 13) +
  theme(text = element_text(colour = "#222222"),
        axis.text = element_text(colour = "#222222", size = 11),
        axis.text.x = element_text(lineheight = 1.15, margin = margin(t = 7)),
        axis.title = element_text(colour = "#222222", size = 12),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "#222222", linewidth = 0.45),
        panel.border = element_rect(colour = "#222222", fill = NA,
                                    linewidth = 0.45),
        legend.position = "none",
        plot.margin = margin(10, 14, 10, 10))

ggsave(file.path(figure_dir, "figure_2_response_boxplots.png"), response_plot,
       width = 6.6, height = 5.7, dpi = 300, bg = "white")
write_csv(response_summary, file.path(table_dir, "figure_2_response_summary.csv"))
print(response_summary)
