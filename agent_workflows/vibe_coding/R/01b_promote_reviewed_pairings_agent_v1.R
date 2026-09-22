# Promote a fresh strict workbook import into the analysis configuration.
# Excluded candidates remain in the configuration for provenance.
library(tidyverse)
library(here)

workflow_dir <- here("agent_workflows", "vibe_coding")
source(file.path(workflow_dir, "R", "01_pairing_review_workbook_agent_v1.R"),
       local = new.env(parent = globalenv()))

validation <- read_csv(file.path(workflow_dir, "data", "audit", "pairing_review_validation.csv"),
                       show_col_types = FALSE)
reviewed <- read_csv(file.path(workflow_dir, "config", "pairing_decisions_reviewed.csv"),
                     show_col_types = FALSE)
stopifnot(nrow(validation) == 0, !anyDuplicated(reviewed$candidate_pair_id),
          all(reviewed$Decision_Status %in% c("approved", "excluded")),
          all(as.logical(reviewed$Include) == (reviewed$Decision_Status == "approved")))

analysis_pairs <- reviewed %>%
  mutate(
    Fire_ID_Analysis = Fire_ID_Final,
    Pairing_Type_Analysis = Pairing_Type_Final,
    Include_Analysis = as.logical(Include),
    Analysis_Decision_Status = if_else(Include_Analysis, "confirmed", "excluded"),
    Coauthor_Confirmation = "confirmed",
    Composite_Reference_Candidate = Pairing_Type_Final == "composite_reference",
    Analysis_Assumption = "Pairing decisions imported from the reviewed Excel workbook; effect-size and predictor assumptions still require audit.",
    n_pairs_sharing_reference_original = n_pairs_sharing_reference,
    shared_reference_original = shared_reference
  ) %>%
  group_by(Study_ID, shared_control_id) %>%
  mutate(
    n_pairs_sharing_reference = n_distinct(candidate_pair_id[Include_Analysis]),
    shared_reference = Include_Analysis & n_pairs_sharing_reference > 1
  ) %>%
  ungroup()

write_csv(analysis_pairs, file.path(workflow_dir, "config", "pairing_decisions_analysis.csv"), na = "")
print(count(analysis_pairs, Analysis_Decision_Status, Include_Analysis))
