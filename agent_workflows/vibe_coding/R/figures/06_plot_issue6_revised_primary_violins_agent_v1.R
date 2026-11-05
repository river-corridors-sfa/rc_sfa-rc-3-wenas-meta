# One violin figure for every predictor in the user-defined DOC and nitrate sets.
library(tidyverse)
library(here)

root <- here("agent_workflows", "vibe_coding")
figure_dir <- file.path(root, "output", "paper", "figures", "si_figure_2_predictor_violins")
table_dir <- file.path(root, "output", "paper", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

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
  mutate(response_var = factor(response_var, levels = c("NO3", "DOC"),
                               labels = c("Nitrate", "DOC")))

summary <- values %>%
  group_by(response_var, predictor) %>%
  summarise(n = n(), sample_variance = var(value),
            minimum = min(value), q1 = as.numeric(quantile(value, .25)),
            median = median(value), q3 = as.numeric(quantile(value, .75)),
            maximum = max(value), .groups = "drop") %>%
  left_join(dictionary %>% select(predictor, predictor_label), by = "predictor") %>%
  mutate(observation_unit = if_else(predictor == "post_fire_year", "pair-year", "watershed pair"))
# Preserve the established companion-table order independently of plot order.
summary <- summary %>% arrange(desc(response_var), predictor)
write_csv(summary, file.path(table_dir, "si_figure_2_predictor_distribution_summary.csv"))

# Units follow the repository data dictionaries, without converting raw values:
# v2_Cavaiani_2024_Metaanalysis_dd.csv and
# gis_data/nhd_geospatial_data/nhd_variable_dictionary.csv.
panel_labels <- c(
  post_fire_year = "A  Post-fire year (yr)",
  burn_percent_fire_year = "B  Burned watershed area (%)",
  burn_sev_high = "C  High-severity burn (%)",
  Area_watershed_km = "D  Watershed area (km²)",
  runoffws = "E  Mean annual runoff (mm)",
  bfiws = "F  Baseflow index (%)",
  forest_cover = "G  Forest cover (%)",
  omws = "H  Soil organic matter (%)",
  clayws = "I  Soil clay content (%)")
stopifnot(setequal(predictors, names(panel_labels)))
values <- values %>%
  mutate(panel_label = factor(panel_labels[predictor], levels = panel_labels[predictors]))

figure <- ggplot(values, aes(x = response_var, y = value, fill = response_var)) +
  geom_violin(trim = TRUE, scale = "width", alpha = .45, linewidth = .4,
              colour = "#444444", width = .78) +
  geom_point(position = position_jitter(width = .11, height = 0, seed = 42),
             shape = 21, size = 1.3, stroke = .25, alpha = .65, colour = "#222222") +
  stat_summary(fun = median, geom = "crossbar", width = .23,
               colour = "#222222", linewidth = .45) +
  facet_wrap(~panel_label, scales = "free_y", ncol = 3) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(Nitrate = "#4386B5", DOC = "#D49B47"), drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(.06, .08)),
                     labels = scales::label_number(big.mark = ",")) +
  labs(x = NULL, y = NULL) +
  theme_classic(base_size = 9) +
  theme(text = element_text(colour = "#222222"),
        axis.text = element_text(colour = "#222222", size = 8),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "#222222", linewidth = .4),
        panel.border = element_rect(colour = "#222222", fill = NA, linewidth = .45),
        strip.background = element_rect(fill = "white", colour = "#222222", linewidth = .45),
        strip.text = element_text(colour = "#222222", size = 8),
        panel.spacing = grid::unit(9, "pt"),
        legend.position = "none", plot.margin = margin(8, 8, 8, 8))

ggsave(file.path(figure_dir, "si_figure_2_predictor_violins.png"), figure,
       width = 7.5, height = 8, dpi = 600, bg = "white")
ggsave(file.path(figure_dir, "si_figure_2_predictor_violins.pdf"), figure,
       width = 7.5, height = 8, device = pdf, useDingbats = FALSE, bg = "white")
counts <- summary %>%
  arrange(match(predictor, predictors), response_var) %>%
  mutate(detail = paste0(as.character(response_var), " n = ", n)) %>%
  group_by(predictor) %>%
  summarise(detail = paste(detail, collapse = "; "), .groups = "drop") %>%
  arrange(match(predictor, predictors)) %>%
  mutate(detail = paste0(substr(panel_labels[predictor], 1, 1), ": ", detail)) %>%
  pull(detail)
caption <- paste0(
  "SI Figure 2. Final-set predictor distributions. Panels A–I show the nine-variable union ",
  "of the two final eight-predictor sets, with nitrate before DOC. ",
  "Static attributes contribute one finite value per watershed pair; post-fire year contributes ",
  "one value per retained pair-year, separately by analyte. Raw values precede transformation ",
  "and standardization, with independent vertical scales and units in panel labels. ",
  "Post-fire year uses reported time since fire where available and follow-up year otherwise; ",
  "the fallback need not equal elapsed years since wildfire. ",
  "Violin widths show Gaussian kernel densities (default nrd0 bandwidth), trimmed to observed ",
  "ranges and scaled to equal maximum width within each panel. Points show every finite value ",
  "with horizontal jitter only; black bars mark medians. ",
  "Organic matter is DOC-only and clay content nitrate-only; empty positions indicate predictors ",
  "outside that analyte's final set, not missing observations. ",
  paste(counts, collapse = ". "), ". ",
  "These unweighted coverage distributions give more influence to publications with more pairs ",
  "(or more available years in panel A); they do not depict the equal-publication fitting or ",
  "pooled-synthesis target. Density spread is variation, not uncertainty in a pooled response. ",
  "Sample sizes, sample variances in squared source units, ranges and quartiles are in ",
  "si_figure_2_predictor_distribution_summary.csv. Units follow the repository data dictionaries; ",
  "watershed area is in square kilometers, runoff in millimeters, and baseflow index and soil properties in percent."
)
writeLines(caption, file.path(figure_dir, "si_figure_2_caption.txt"))
message("Saved SI Figure 2 PNG/PDF, standalone caption and distribution summary.")
