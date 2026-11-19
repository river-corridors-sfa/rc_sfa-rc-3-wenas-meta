library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

workflow <- here::here("agent_workflows", "vibe_coding")
annual <- read_csv(file.path(workflow, "data/derived/lasso_model_table.csv"),
                   show_col_types = FALSE)
references <- read_csv(file.path(workflow, "data/audit/shared_reference_structure.csv"),
                       show_col_types = FALSE)

# Count reference reuse from the audited groups rather than the workbook labels.
annual <- annual |>
  left_join(select(references, shared_control_id, n_pairs), by = "shared_control_id",
            relationship = "many-to-one") |>
  mutate(year_since_fire = year - as.integer(sub(".*_", "", Fire_ID_Analysis)))
stopifnot(!anyNA(annual$n_pairs), !anyNA(annual$year_since_fire),
          !anyDuplicated(annual[c("candidate_pair_id", "response_var", "year")]))

years <- sort(unique(annual$year_since_fire))
fixed_counts <- annual |>
  filter(n_pairs == 1) |>
  count(year_since_fire, name = "fixed") |>
  complete(year_since_fire = years, fill = list(fixed = 0L))

# Count zero for choices with no effect sizes in a given year. The choices
# across reference groups are independent, so their per-year bounds add.
shared_counts <- annual |>
  filter(n_pairs > 1) |>
  count(shared_control_id, candidate_pair_id, year_since_fire, name = "effect_sizes") |>
  complete(nesting(shared_control_id, candidate_pair_id), year_since_fire = years,
           fill = list(effect_sizes = 0L))
shared_bounds <- shared_counts |>
  group_by(shared_control_id, year_since_fire) |>
  summarise(lower = min(effect_sizes), upper = max(effect_sizes), .groups = "drop") |>
  group_by(year_since_fire) |>
  summarise(lower = sum(lower), upper = sum(upper), .groups = "drop")

plot_counts <- fixed_counts |>
  left_join(shared_bounds, by = "year_since_fire") |>
  mutate(minimum = fixed + lower, maximum = fixed + upper,
         label = if_else(minimum == maximum, as.character(minimum),
                         paste(minimum, maximum, sep = "\u2013")),
         year_label = factor(year_since_fire, levels = years))
bars <- plot_counts |>
  pivot_longer(c(fixed, lower), names_to = "pair_type", values_to = "effect_sizes")

figure <- ggplot(plot_counts, aes(x = year_label)) +
  geom_col(data = bars, aes(y = effect_sizes, fill = pair_type), width = 0.64,
           position = position_stack(reverse = TRUE)) +
  geom_errorbar(data = filter(plot_counts, minimum < maximum),
                aes(ymin = minimum, ymax = maximum), width = 0.18,
                linewidth = 0.6, colour = "#183348") +
  geom_text(aes(y = maximum + 0.53, label = label), vjust = 0, size = 4,
            fontface = "bold", colour = "#183348") +
  scale_fill_manual(
    values = c(fixed = "#244F68", lower = "#5BA393"),
    breaks = c("fixed", "lower"),
    labels = c("Existing 1:1 pairs", "Added pairs from shared references")
  ) +
  scale_y_continuous(breaks = seq(0, max(plot_counts$maximum), 4),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Annual effect sizes in the 18-pair design",
    subtitle = "11 existing 1:1 pairs and one burn per shared reference in 7 studies",
    x = "Years since fire",
    y = "Annual effect sizes (DOC + nitrate)",
    fill = NULL,
    caption = paste(
      "Bars show the minimum count. Ranges reflect which burn is retained.",
      "Year 0 is the fire year. Years 6–14 have no records.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#E3EAEE", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 17, colour = "#183348"),
    plot.subtitle = element_text(colour = "#334C5D", margin = margin(b = 9)),
    plot.caption = element_text(hjust = 0, colour = "#334C5D", margin = margin(t = 10)),
    plot.margin = margin(15, 20, 15, 16)
  )

ggsave(file.path(workflow, "exploratory/output/one_to_one_annual_effect_sizes.png"),
       figure, width = 8.4, height = 5.3, dpi = 300, bg = "white")
print(select(plot_counts, year_since_fire, fixed, minimum, maximum))
