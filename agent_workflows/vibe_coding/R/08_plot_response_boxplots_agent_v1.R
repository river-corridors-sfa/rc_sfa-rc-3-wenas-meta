# Plot the annual responses in the latest prepared model table, without refitting.
library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
figure_dir <- file.path(workflow_dir, "output", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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
         " | ", response_summary$studies, " studies"),
  response_summary$analyte
)

response_plot <- ggplot(responses, aes(analyte, lnRR_mean, fill = analyte)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_boxplot(width = 0.45, alpha = 0.65, outlier.shape = NA,
               linewidth = 0.6) +
  geom_point(position = position_jitter(width = 0.12, height = 0, seed = 42),
             shape = 21, size = 2.1, alpha = 0.65, stroke = 0.3) +
  scale_fill_manual(values = c(Nitrate = "#4386B5", DOC = "#D49B47")) +
  scale_x_discrete(labels = axis_labels) +
  labs(title = "Nitrate and DOC responses",
       subtitle = "Annual burned-to-reference log response ratios",
       x = NULL, y = "Annual mean lnRR (unitless)",
       caption = paste(
         "Each point is one watershed-pair year; all finite responses included, unweighted.",
         "Boxes: median and interquartile range; whiskers: within 1.5 × IQR.",
         "Dashed line: no difference between burned and reference watersheds.",
         sep = "\n")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 17),
        plot.caption = element_text(hjust = 0, colour = "grey35", size = 9),
        axis.text.x = element_text(size = 12),
        plot.margin = margin(15, 18, 12, 12))

ggsave(file.path(figure_dir, "final_response_boxplots.png"), response_plot,
       width = 7, height = 6.5, dpi = 300, bg = "white")
write_csv(response_summary, file.path(workflow_dir, "output", "tables",
                                      "final_response_boxplot_summary.csv"))
print(response_summary)
