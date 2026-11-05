# SI Figure 1: Spearman correlations for every available predictor-like field.
library(tidyverse)
library(here)

root <- here("agent_workflows", "vibe_coding")
figure_dir <- file.path(root, "output/paper/figures/si_figure_1_predictor_correlations")
table_dir <- file.path(root, "output/paper/tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

data <- read_csv(file.path(root, "data/derived/lasso_model_table.csv"),
                 show_col_types = FALSE) %>% filter(is.finite(lnRR_mean))
dictionary <- read_csv(file.path(root, "config/predictor_dictionary.csv"),
                       show_col_types = FALSE)
extra_predictors <- c("time_since_fire", "followup_year", "burn_sev_mod", "burn_sev_low")
predictors <- unique(c(dictionary$predictor, extra_predictors))
missing <- setdiff(predictors, names(data))
if (length(missing)) stop("Missing predictor fields: ", paste(missing, collapse = ", "))

labels <- c(setNames(dictionary$predictor_label, dictionary$predictor),
            time_since_fire = "Reported time since fire",
            followup_year = "Follow-up year",
            burn_sev_mod = "Moderate-severity burn (%)",
            burn_sev_low = "Low-severity burn (%)")
correlation <- cor(as.matrix(data[predictors]),
                   use = "pairwise.complete.obs", method = "spearman")
pair_counts <- outer(predictors, predictors, Vectorize(function(a, b)
  sum(is.finite(data[[a]]) & is.finite(data[[b]]))))
dimnames(pair_counts) <- dimnames(correlation)

correlation_long <- as.data.frame(as.table(correlation), stringsAsFactors = FALSE) %>%
  rename(predictor_1 = Var1, predictor_2 = Var2, rho = Freq) %>%
  mutate(n_pairwise = as.vector(pair_counts),
         label_1 = labels[as.character(predictor_1)],
         label_2 = labels[as.character(predictor_2)])
write_csv(correlation_long, file.path(table_dir, "si_figure_1_predictor_correlations.csv"))

# Numeric column keys keep the full 24-field matrix readable at export size.
# The matching row labels retain every predictor name without rotated text.
labels["Area_watershed_km"] <- "Watershed area (km²)"
axis_labels <- paste0(seq_along(predictors), ". ", labels[predictors])
correlation_long <- correlation_long %>%
  mutate(label_1 = labels[as.character(predictor_1)],
         label_2 = labels[as.character(predictor_2)]) %>%
  mutate(label_1 = factor(label_1, levels = rev(labels[predictors])),
         label_2 = factor(label_2, levels = labels[predictors]))
figure <- ggplot(correlation_long, aes(x = label_2, y = label_1, fill = rho)) +
  geom_tile(colour = "white", linewidth = .16) +
  geom_text(aes(label = if_else(abs(rho) >= .65, sprintf("%.2f", rho), "")),
            size = 1.9, colour = "#222222") +
  scale_x_discrete(labels = seq_along(predictors), position = "top", expand = c(0, 0)) +
  scale_y_discrete(labels = rev(axis_labels), expand = c(0, 0)) +
  scale_fill_gradient2(low = "#6399BE", mid = "white", high = "#D99A87",
                       midpoint = 0, limits = c(-1, 1),
                       breaks = seq(-1, 1, .5), name = expression(Spearman~rho),
                       na.value = "#D9D9D9") +
  guides(fill = guide_colourbar(display = "rectangles")) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(text = element_text(colour = "#222222"),
        axis.text.x = element_text(size = 7, colour = "#222222"),
        axis.text.y = element_text(size = 7.5, colour = "#222222"),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "#222222", fill = NA, linewidth = .45),
        legend.position = "bottom", legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = grid::unit(1, "cm"),
        plot.margin = margin(8, 8, 8, 8))
ggsave(file.path(figure_dir, "si_figure_1_predictor_correlation_matrix.png"),
       figure, width = 7.5, height = 7.2, dpi = 600, bg = "white")
ggsave(file.path(figure_dir, "si_figure_1_predictor_correlation_matrix.pdf"),
       figure, width = 7.5, height = 7.2, device = pdf, useDingbats = FALSE, bg = "white")
caption <- paste0(
  "SI Figure 1. Predictor correlations. Pairwise-complete Spearman correlations among ",
  length(predictors), " fields: ", nrow(dictionary),
  " screened candidate fields and four additional timing and burn-severity fields. ",
  "Column numbers match the numbered predictor labels on the rows. ",
  "The matrix uses ", nrow(data), " retained annual DOC and nitrate analyte records; ",
  "static watershed attributes repeat across years and analytes. ",
  "This is an unweighted descriptive matrix, not an independent-sample or balance-weighted diagnostic. ",
  "Cells with absolute correlation at least 0.65 are labeled; all coefficients are dimensionless. ",
  "Gray cells, if present, indicate undefined correlations. ",
  "Pairwise sample counts range from ", min(pair_counts), " to ", max(pair_counts),
  "; missing values are omitted separately for each pair of fields. ",
  "Exact coefficients and pairwise counts are in si_figure_1_predictor_correlations.csv. ",
  "A separate pair-level or balance-weighted diagnostic would be needed before using these correlations to justify redundancy decisions."
)
writeLines(caption, file.path(figure_dir, "si_figure_1_caption.txt"))
message("Saved SI Figure 1 PNG/PDF, standalone caption and correlation table for ",
        length(predictors), " variables.")
