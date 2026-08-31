# ==============================================================================
# Script: 01a_create_provisional_pairings_agent_v1.R
# Purpose: Create a separate analysis pairing table that provisionally includes
#          the existing lnRR pairs while co-author confirmation is pending.
#
# This script does not overwrite pairing_decisions.csv, the review workbook, or
# pairing_decisions_reviewed.csv. Final review fields remain unchanged.
# ==============================================================================

library(tidyverse)
library(here)

# ---- 1. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")

candidate_path <- file.path(
  workflow_dir,
  "config", "pairing_decisions.csv"
)

analysis_path <- file.path(
  workflow_dir,
  "config", "pairing_decisions_analysis.csv"
)

# ---- 2. Read the established lnRR pair inventory ---------------------------

candidate_pairs <- read_csv(
  candidate_path,
  show_col_types = FALSE,
  col_types = cols(
    Fire_ID_Final = col_character(),
    Pairing_Type_Final = col_character(),
    Include = col_character(),
    Decision_Status = col_character(),
    Decision_Notes = col_character()
  )
)

stopifnot(
  nrow(candidate_pairs) == 36,
  !anyDuplicated(candidate_pairs$candidate_pair_id)
)

# ---- 3. Add provisional analysis fields ------------------------------------

# Comparison_ID is used conservatively as the temporary fire/comparison group.
# This avoids inventing fire assignments for studies with multi-fire metadata.

analysis_pairs <- candidate_pairs %>%
  mutate(
    Fire_ID_Analysis = Comparison_ID,
    Pairing_Type_Analysis = if_else(
      shared_reference,
      "designated_shared_reference",
      "designated_one_to_one"
    ),
    Composite_Reference_Candidate = str_detect(
      Pair_Unburn,
      "^Control_[0-9]+_[0-9]+"
    ),
    Include_Analysis = TRUE,
    Analysis_Decision_Status = "provisionally_approved",
    Coauthor_Confirmation = "pending",
    Analysis_Assumption = paste(
      "Pair appeared in the established lnRR workflow and is treated as",
      "previously manually reviewed for provisional analysis. Confirm with",
      "a co-author before final analysis or manuscript reporting."
    )
  )

# ---- 4. Checks and export ---------------------------------------------------

stopifnot(
  nrow(analysis_pairs) == nrow(candidate_pairs),
  all(analysis_pairs$Include_Analysis),
  all(analysis_pairs$Analysis_Decision_Status == "provisionally_approved"),
  all(!is.na(analysis_pairs$Fire_ID_Analysis)),
  all(!is.na(analysis_pairs$Pairing_Type_Analysis))
)

write_csv(
  analysis_pairs,
  analysis_path,
  na = ""
)

message(
  "Wrote ",
  nrow(analysis_pairs),
  " provisional analysis pairings to: ",
  analysis_path
)

