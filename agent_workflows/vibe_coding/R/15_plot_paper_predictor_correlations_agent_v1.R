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

correlation_long <- correlation_long %>%
  mutate(label_1 = factor(label_1, levels = rev(labels[predictors])),
         label_2 = factor(label_2, levels = labels[predictors]))
figure <- ggplot(correlation_long, aes(x = label_2, y = label_1, fill = rho)) +
  geom_tile(colour = "white", linewidth = .16) +
  geom_text(aes(label = if_else(abs(rho) >= .65, sprintf("%.2f", rho), "")),
            size = 2.2, colour = "#20292C") +
  scale_fill_gradient2(low = "#3A6C9A", mid = "white", high = "#B65F4A",
                       midpoint = 0, limits = c(-1, 1), name = "Spearman ρ") +
  coord_equal() +
  labs(title = "SI Figure 1. Correlations among all available predictors",
       subtitle = "Twenty screened candidates plus four additional timing and burn-severity fields",
       x = NULL, y = NULL,
       caption = paste(
         "Pairwise-complete Spearman correlations across the 110 finite annual DOC and nitrate model rows.",
         "Annual rows from the same watershed pair repeat static attributes; this is a descriptive redundancy diagnostic.",
         "Cells with |ρ| ≥ 0.65 are labeled; exact values and pairwise counts are in the companion CSV.",
         sep = "\n")) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_text(hjust = 0, size = 8),
        plot.margin = margin(12, 15, 12, 12))
ggsave(file.path(figure_dir, "si_figure_1_predictor_correlation_matrix.png"),
       figure, width = 15, height = 13, dpi = 300, bg = "white")
message("Saved SI Figure 1 and correlation table for ", length(predictors), " variables.")
