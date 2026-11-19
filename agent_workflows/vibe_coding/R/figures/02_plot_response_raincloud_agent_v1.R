# Figure 2: balance-weighted distributions and saved primary CR2 synthesis.
# Replaces the boxplot preserved in archive/figure_2_boxplot_2026_10_22/.
library(tidyverse)
library(here)

workflow_dir <- here('agent_workflows', 'vibe_coding')
figure_dir <- file.path(workflow_dir, 'output/paper/figures/figure_2_response_raincloud')
table_dir <- file.path(workflow_dir, 'output/paper/tables')
synthesis_dir <- file.path(workflow_dir, 'output/paper/balanced_synthesis')
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

responses <- read_csv(file.path(workflow_dir, 'data/derived/lasso_model_table.csv'),
                      show_col_types = FALSE) %>%
  filter(response_var %in% c('NO3', 'DOC'), is.finite(lnRR_mean))
weights <- read_csv(file.path(synthesis_dir, 'balanced_weights.csv'), show_col_types = FALSE) %>%
  filter(scenario == 'primary')
pooled <- read_csv(file.path(synthesis_dir, 'balanced_CR2_results.csv'), show_col_types = FALSE) %>%
  filter(status == 'primary_synthesis')
keys <- c('Study_ID', 'candidate_pair_id', 'response_var', 'year')
stopifnot(!anyDuplicated(responses[keys]), !anyDuplicated(weights[keys]),
          nrow(anti_join(responses, weights, by = keys)) == 0,
          nrow(anti_join(weights, responses, by = keys)) == 0,
          nrow(pooled) == 2, setequal(pooled$response_var, c('NO3', 'DOC')))
responses <- responses %>%
  left_join(select(weights, all_of(keys), dependence_cluster, review_weight), by = keys) %>%
  mutate(analyte = factor(response_var, levels = c('NO3', 'DOC'), labels = c('Nitrate', 'DOC')),
         x = as.numeric(analyte))

# Verify saved balancing weights/estimates against the current figure input.
weight_checks <- responses %>% group_by(Study_ID, candidate_pair_id, response_var) %>%
  summarise(years = n(), pair_weight = sum(review_weight), .groups = 'drop') %>%
  group_by(Study_ID, response_var) %>%
  mutate(expected = 1 / n()) %>% ungroup()
study_checks <- responses %>% group_by(Study_ID, response_var) %>%
  summarise(total = sum(review_weight), .groups = 'drop')
stopifnot(all(is.finite(responses$review_weight)), all(responses$review_weight > 0),
          all(abs(weight_checks$pair_weight - weight_checks$expected) < 1e-10),
          all(abs(study_checks$total - 1) < 1e-10))
response_summary <- responses %>% group_by(response_var, analyte, x) %>%
  summarise(n = n(), studies = n_distinct(Study_ID),
            component_count = n_distinct(dependence_cluster),
            minimum = min(lnRR_mean), q1 = quantile(lnRR_mean, 0.25),
            median = median(lnRR_mean), q3 = quantile(lnRR_mean, 0.75),
            maximum = max(lnRR_mean), balanced_mean = weighted.mean(lnRR_mean, review_weight),
            .groups = 'drop') %>%
  left_join(select(pooled, response_var, estimate, lower, upper, df, rows, clusters), by = 'response_var')
stopifnot(all(abs(response_summary$balanced_mean - response_summary$estimate) < 1e-10),
          all(response_summary$n == response_summary$rows),
          all(response_summary$component_count == response_summary$clusters),
          all(is.finite(response_summary$lower)), all(is.finite(response_summary$upper)))

# Gaussian KDE uses the synthesis balance weights, normalized within analyte.
# Explicit per-analyte nrd0 bandwidth controls smoothing only, not inference.
# Each cloud has the same maximum width and is trimmed to its observed range.
clouds <- list()
for (analyte_name in levels(responses$analyte)) {
  d <- filter(responses, analyte == analyte_name)
  bandwidth <- bw.nrd0(d$lnRR_mean)
  cloud <- density(d$lnRR_mean, weights = d$review_weight / sum(d$review_weight),
                   bw = bandwidth, from = min(d$lnRR_mean), to = max(d$lnRR_mean), n = 512)
  centre <- first(d$x) - 0.07
  edge <- centre - 0.35 * cloud$y / max(cloud$y)
  clouds[[analyte_name]] <- tibble(analyte = analyte_name,
    x = c(centre, edge, centre), lnRR = c(min(cloud$x), cloud$x, max(cloud$x)),
    bandwidth = bandwidth)
}
cloud_data <- bind_rows(clouds)
axis_labels <- paste0(response_summary$analyte, '\nn = ', response_summary$n,
                      '; ', response_summary$studies, ' studies\n',
                      response_summary$clusters, ' fire groups')

response_plot <- ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = '#555555', linewidth = 0.45) +
  geom_polygon(data = cloud_data, aes(x, lnRR, group = analyte, fill = analyte),
               alpha = 0.55, colour = '#333333', linewidth = 0.4) +
  geom_point(data = responses, aes(x = x + 0.04, y = lnRR_mean, fill = analyte),
             position = position_jitter(width = 0.055, height = 0, seed = 42),
             shape = 21, size = 1.6, alpha = 0.48, stroke = 0.2, colour = '#222222') +
  geom_errorbar(data = response_summary,
                 aes(x = x + 0.25, ymin = lower, ymax = upper),
                 width = 0.075, linewidth = 0.9, colour = '#222222') +
  geom_point(data = response_summary, aes(x = x + 0.25, y = estimate, fill = analyte),
             shape = 23, size = 3.7, stroke = 0.8, colour = '#222222') +
  scale_fill_manual(values = c(Nitrate = '#4386B5', DOC = '#D49B47')) +
  scale_x_continuous(breaks = response_summary$x, labels = axis_labels,
                     limits = c(0.48, 2.48), expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.035, 0.045))) +
  labs(x = NULL, y = 'Annual log response ratio (lnRR)') +
  theme_classic(base_size = 13) +
  theme(text = element_text(colour = '#222222'),
        axis.text = element_text(colour = '#222222', size = 11),
        axis.text.x = element_text(lineheight = 1.1, margin = margin(t = 7)),
        axis.title = element_text(colour = '#222222', size = 12),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = '#222222', linewidth = 0.45),
        panel.border = element_rect(colour = '#222222', fill = NA, linewidth = 0.45),
        legend.position = 'none', plot.margin = margin(10, 14, 10, 10))
ggsave(file.path(figure_dir, 'figure_2_response_raincloud.png'), response_plot,
       width = 6.6, height = 5.7, dpi = 300, bg = 'white')
ggsave(file.path(figure_dir, 'figure_2_response_raincloud.pdf'), response_plot,
       width = 6.6, height = 5.7, bg = 'white')
write_csv(response_summary, file.path(table_dir, 'figure_2_response_summary.csv'))
write_csv(select(responses, all_of(keys), lnRR_mean, dependence_cluster, review_weight),
          file.path(table_dir, 'figure_2_response_points_weights.csv'))
write_csv(cloud_data, file.path(table_dir, 'figure_2_response_cloud_polygons.csv'))
writeLines(paste(
  'Figure 2. Annual nitrate and DOC responses and balanced pooled synthesis.',
  sprintf('Small circles show all retained annual burned–reference pair responses (%s); six singleton annual records and one author-selected extreme low nitrate record are excluded.',
    paste(paste(response_summary$n, response_summary$analyte), collapse = '; ')),
  'Left half-violins show Gaussian kernel densities weighted equally by study, pair within study, and available year within pair.',
  'Bandwidths use the per-analyte nrd0 rule; clouds are trimmed to the observed ranges and scaled to equal maximum widths.',
  'Diamonds show balanced pooled log response ratios; vertical bars show 95% CR2 confidence intervals with Satterthwaite degrees of freedom, clustered by connected study/fire components.',
  'Nitrate represents 12 publications and 11 fire groups; DOC represents seven publications and six fire groups. Fire groups are connected study/fire components used for uncertainty estimation, not counts of unique wildfires. All records within a publication stay together, and publications sharing a fire are linked. Rhea and Writer are linked through High Park.',
  'The dashed line marks zero (equal burned and reference concentrations). Cloud spread describes response variation, not uncertainty in the pooled estimate.',
  'Responses are log ratios of annual arithmetic mean concentrations on eligible matched positive-concentration dates, pooled across available post-fire years.'),
  file.path(figure_dir, 'figure_2_caption.txt'))
print(response_summary, width = Inf)
