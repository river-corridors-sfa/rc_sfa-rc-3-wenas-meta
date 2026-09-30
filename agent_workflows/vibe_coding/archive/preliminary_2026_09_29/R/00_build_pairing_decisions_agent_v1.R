# ==============================================================================
# Script: 00_build_pairing_decisions_agent_v1.R
# Purpose: Inventory candidate burned-reference watershed contrasts for author
#          review. This script does not decide that a pairing is valid.
#
# Source scripts are human generated and remain unchanged. This new script was
# created for the agent workflow under agent_workflows/vibe_coding/.
# ==============================================================================

library(tidyverse)
library(here)

# ---- 1. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")

effect_size_path <- file.path(
  workflow_dir,
  "data", "source", "effect_sizes_yearly.csv"
)

site_metadata_path <- file.path(
  workflow_dir,
  "data", "source", "Sites_meta_data.csv"
)

output_path <- file.path(
  workflow_dir,
  "config", "pairing_decisions.csv"
)

# ---- 2. Read source snapshots ----------------------------------------------

effect_sizes_yearly <- read_csv(
  effect_size_path,
  show_col_types = FALSE
)

site_metadata <- read_csv(
  site_metadata_path,
  show_col_types = FALSE
)

# ---- 3. Reduce the existing effect-size table to candidate pairs -----------

# The existing table is used only to inventory contrasts that have already
# appeared in the workflow. Its presence does not establish that a pairing is
# ecologically justified or statistically independent.

candidate_pairs <- effect_sizes_yearly %>%
  group_by(
    Study_ID,
    Comparison_ID,
    Pair_Burn,
    Pair_Unburn
  ) %>%
  summarise(
    analytes_available = paste(
      sort(unique(response_var)),
      collapse = ";"
    ),
    first_calendar_year = min(year, na.rm = TRUE),
    last_calendar_year = max(year, na.rm = TRUE),
    n_calendar_years = n_distinct(year),
    .groups = "drop"
  )

# ---- 4. Identify reference watersheds reused across contrasts --------------

candidate_pairs <- candidate_pairs %>%
  group_by(Study_ID, Pair_Unburn) %>%
  mutate(
    shared_control_id = paste(
      Study_ID,
      Pair_Unburn,
      sep = " | "
    ),
    n_pairs_sharing_reference = n_distinct(Comparison_ID),
    shared_reference = n_pairs_sharing_reference > 1
  ) %>%
  ungroup()

# ---- 5. Add study-level fire and design metadata ---------------------------

study_metadata <- site_metadata %>%
  select(
    Study_ID,
    Fire_name,
    Fire_year,
    Study_design
  ) %>%
  distinct(Study_ID, .keep_all = TRUE)

pairing_decisions <- candidate_pairs %>%
  left_join(
    study_metadata,
    by = "Study_ID"
  ) %>%
  mutate(
    candidate_pair_id = paste(
      Study_ID,
      Comparison_ID,
      Pair_Burn,
      Pair_Unburn,
      sep = " | "
    ),
    multi_fire_metadata =
      str_detect(coalesce(Fire_name, ""), fixed("_")) |
      str_detect(as.character(coalesce(Fire_year, "")), fixed("_")),
    pairing_source = "effect_sizes_yearly.csv",
    pairing_type_suggested = if_else(
      shared_reference,
      "shared_reference_candidate",
      "one_to_one_candidate"
    ),

    # These fields are deliberately blank for author review.
    Fire_ID_Final = NA_character_,
    Pairing_Type_Final = NA_character_,
    Include = NA_character_,
    Decision_Status = "pending_author_review",

    Decision_Notes = case_when(
      shared_reference & multi_fire_metadata ~
        "Shared reference and fire assignment require author review.",
      shared_reference ~
        paste(
          "Multiple burned-watershed contrasts use the same reference",
          "identifier; confirm the study design and dependence handling."
        ),
      multi_fire_metadata ~
        "Confirm which fire belongs to this comparison.",
      TRUE ~
        "Confirm that this is a study-designated burned-reference pair."
    )
  ) %>%
  rename(
    Fire_name_metadata = Fire_name,
    Fire_year_metadata = Fire_year,
    Study_design_metadata = Study_design
  ) %>%
  select(
    Study_ID,
    Comparison_ID,
    Pair_Burn,
    Pair_Unburn,
    candidate_pair_id,
    Fire_name_metadata,
    Fire_year_metadata,
    Study_design_metadata,
    analytes_available,
    first_calendar_year,
    last_calendar_year,
    n_calendar_years,
    shared_control_id,
    n_pairs_sharing_reference,
    shared_reference,
    multi_fire_metadata,
    pairing_source,
    pairing_type_suggested,
    Fire_ID_Final,
    Pairing_Type_Final,
    Include,
    Decision_Status,
    Decision_Notes
  ) %>%
  arrange(
    Study_ID,
    Comparison_ID,
    Pair_Burn,
    Pair_Unburn
  )

# ---- 6. Checks --------------------------------------------------------------

stopifnot(
  nrow(pairing_decisions) ==
    n_distinct(effect_sizes_yearly$pair_key)
)

stopifnot(
  !anyDuplicated(pairing_decisions$candidate_pair_id)
)

print(
  pairing_decisions %>%
    count(
      pairing_type_suggested,
      shared_reference,
      multi_fire_metadata
    )
)

# ---- 7. Export the author-review table -------------------------------------

dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  pairing_decisions,
  output_path,
  na = ""
)

message(
  "Wrote ",
  nrow(pairing_decisions),
  " candidate pairings to ",
  output_path
)
