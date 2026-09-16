# ==============================================================================
# Script: 01_pairing_review_workbook_agent_v1.R
# Purpose: Create an author-review workbook for candidate watershed pairings,
#          or validate and import decisions from the completed workbook.
#
# This script never overwrites the original pairing_decisions.csv. Imported
# decisions are written to pairing_decisions_reviewed.csv.
# ==============================================================================

library(tidyverse)
library(here)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    "Package 'openxlsx' is required. Install it with install.packages('openxlsx')."
  )
}

library(openxlsx)

# ---- 1. Choose the action ---------------------------------------------------

# Use "create" to build or refresh the review workbook.
# After review, change this to "import" to validate and export the decisions.
review_action <- "create"

# TRUE allows an in-progress review to be exported with validation warnings.
# Change to FALSE before producing the final reviewed decisions table.
allow_partial_import <- TRUE

# ---- 2. Paths ---------------------------------------------------------------

workflow_dir <- here("agent_workflows", "vibe_coding")

pairing_decisions_path <- file.path(
  workflow_dir,
  "config", "pairing_decisions.csv"
)

daily_data_path <- file.path(
  workflow_dir,
  "data", "source", "01_daily_time_series_paired.csv"
)

review_workbook_path <- file.path(
  workflow_dir,
  "config", "pairing_review_workbook.xlsx"
)

reviewed_decisions_path <- file.path(
  workflow_dir,
  "config", "pairing_decisions_reviewed.csv"
)

validation_path <- file.path(
  workflow_dir,
  "data", "audit", "pairing_review_validation.csv"
)

# ---- 3. Read the candidate decisions and site lookup -----------------------

pairing_decisions <- read_csv(
  pairing_decisions_path,
  show_col_types = FALSE,
  col_types = cols(
    Fire_ID_Final = col_character(),
    Pairing_Type_Final = col_character(),
    Include = col_character(),
    Decision_Status = col_character(),
    Decision_Notes = col_character()
  )
)

daily_data <- read_csv(
  daily_data_path,
  show_col_types = FALSE
)

# Pair labels such as Site_1 and Control_1 are difficult to review without the
# corresponding watershed names. Preserve every unique name associated with a
# study and pair rather than silently choosing the first one.

site_lookup <- daily_data %>%
  filter(
    !is.na(Study_ID),
    !is.na(Pair),
    !is.na(Site),
    !is.na(Burn_Unburn)
  ) %>%
  distinct(
    Study_ID,
    Pair,
    Site,
    Burn_Unburn
  ) %>%
  group_by(
    Study_ID,
    Pair,
    Burn_Unburn
  ) %>%
  summarise(
    Site_Name = paste(
      sort(unique(Site)),
      collapse = "; "
    ),
    .groups = "drop"
  )

burned_site_lookup <- site_lookup %>%
  filter(Burn_Unburn == "Burn") %>%
  transmute(
    Study_ID,
    Pair_Burn = Pair,
    Burned_Site_Name = Site_Name
  )

reference_site_lookup <- site_lookup %>%
  filter(Burn_Unburn == "Unburn") %>%
  transmute(
    Study_ID,
    Pair_Unburn = Pair,
    Reference_Site_Name = Site_Name
  )

# ---- 4. Build the review table ---------------------------------------------

review_table <- pairing_decisions %>%
  left_join(
    burned_site_lookup,
    by = c("Study_ID", "Pair_Burn")
  ) %>%
  left_join(
    reference_site_lookup,
    by = c("Study_ID", "Pair_Unburn")
  ) %>%
  mutate(
    Review_Priority = case_when(
      shared_reference & multi_fire_metadata ~ "1 - shared reference + multi-fire",
      multi_fire_metadata ~ "2 - multi-fire",
      shared_reference ~ "2 - shared reference",
      TRUE ~ "3 - routine confirmation"
    ),
    Evidence_Source = NA_character_,
    Reviewer = NA_character_,
    Review_Date = as.Date(NA)
  ) %>%
  relocate(
    Review_Priority,
    Study_ID,
    Comparison_ID,
    Pair_Burn,
    Burned_Site_Name,
    Pair_Unburn,
    Reference_Site_Name
  ) %>%
  relocate(
    Fire_ID_Final,
    Pairing_Type_Final,
    Include,
    Decision_Status,
    Evidence_Source,
    Reviewer,
    Review_Date,
    Decision_Notes,
    .after = pairing_type_suggested
  ) %>%
  arrange(
    Review_Priority,
    Study_ID,
    Comparison_ID
  )

study_summary <- review_table %>%
  group_by(
    Study_ID,
    Fire_name_metadata,
    Fire_year_metadata
  ) %>%
  summarise(
    n_candidate_pairs = n(),
    n_shared_reference_pairs = sum(shared_reference, na.rm = TRUE),
    multi_fire_metadata = any(multi_fire_metadata, na.rm = TRUE),
    analytes_available = paste(
      sort(unique(analytes_available)),
      collapse = "; "
    ),
    burned_sites = paste(
      sort(unique(na.omit(Burned_Site_Name))),
      collapse = "; "
    ),
    reference_sites = paste(
      sort(unique(na.omit(Reference_Site_Name))),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(multi_fire_metadata),
    desc(n_shared_reference_pairs),
    Study_ID
  )

# ---- 5. Controlled review values -------------------------------------------

pairing_type_values <- c(
  "designated_one_to_one",
  "designated_shared_reference",
  "composite_reference",
  "regional_reference",
  "automatic_cross_product",
  "unclear"
)

include_values <- c(
  "TRUE",
  "FALSE"
)

decision_status_values <- c(
  "pending_author_review",
  "approved",
  "excluded",
  "needs_source_check",
  "needs_team_decision"
)

controlled_values <- bind_rows(
  tibble(
    Field = "Pairing_Type_Final",
    Allowed_Value = pairing_type_values
  ),
  tibble(
    Field = "Include",
    Allowed_Value = include_values
  ),
  tibble(
    Field = "Decision_Status",
    Allowed_Value = decision_status_values
  )
)

instructions <- tribble(
  ~Step, ~Instruction,
  1, "Review one Study_ID block at a time, beginning with priority 1 and 2 rows.",
  2, "Use Burned_Site_Name and Reference_Site_Name to connect generic pair labels to watershed names.",
  3, "Consult the original paper or extraction source before approving a shared-reference, composite-reference, or multi-fire row.",
  4, "Complete the yellow fields: Fire_ID_Final, Pairing_Type_Final, Include, Decision_Status, Evidence_Source, Reviewer, Review_Date, and Decision_Notes.",
  5, "Use Decision_Status = approved only when Include is TRUE and the final fire and pairing type are complete.",
  6, "Use Decision_Status = excluded only when Include is FALSE and Decision_Notes explains why.",
  7, "Save the workbook, set review_action to import, and rerun this script.",
  8, "Review data/audit/pairing_review_validation.csv. Set allow_partial_import to FALSE before final export."
)

# ---- 6. Create the review workbook -----------------------------------------

if (review_action == "create") {

  workbook <- createWorkbook(
    creator = "Agent workflow"
  )

  addWorksheet(
    workbook,
    "Instructions",
    gridLines = FALSE,
    zoom = 100
  )

  addWorksheet(
    workbook,
    "Pairing Review",
    gridLines = FALSE,
    zoom = 85
  )

  addWorksheet(
    workbook,
    "Study Summary",
    gridLines = FALSE,
    zoom = 90
  )

  addWorksheet(
    workbook,
    "Controlled Values",
    gridLines = FALSE,
    zoom = 100
  )

  title_style <- createStyle(
    fontSize = 16,
    textDecoration = "bold",
    fontColour = "#FFFFFF",
    fgFill = "#1F4E78",
    halign = "left",
    valign = "center"
  )

  input_style <- createStyle(
    fgFill = "#FFF2CC",
    border = "TopBottomLeftRight",
    borderColour = "#D6B656",
    valign = "top",
    wrapText = TRUE
  )

  high_priority_style <- createStyle(
    fgFill = "#F4CCCC",
    fontColour = "#9C0006",
    textDecoration = "bold"
  )

  medium_priority_style <- createStyle(
    fgFill = "#FCE5CD",
    fontColour = "#7F6000"
  )

  date_style <- createStyle(
    numFmt = "yyyy-mm-dd"
  )

  wrap_style <- createStyle(
    wrapText = TRUE,
    valign = "top"
  )

  writeData(
    workbook,
    "Instructions",
    "Watershed Pairing Review",
    startRow = 1,
    startCol = 1
  )

  mergeCells(
    workbook,
    "Instructions",
    cols = 1:2,
    rows = 1
  )

  addStyle(
    workbook,
    "Instructions",
    title_style,
    rows = 1,
    cols = 1:2,
    gridExpand = TRUE
  )

  setRowHeights(
    workbook,
    "Instructions",
    rows = 1,
    heights = 26
  )

  writeDataTable(
    workbook,
    "Instructions",
    instructions,
    startRow = 3,
    tableStyle = "TableStyleMedium2"
  )

  setColWidths(
    workbook,
    "Instructions",
    cols = 1:2,
    widths = c(8, 110)
  )

  addStyle(
    workbook,
    "Instructions",
    wrap_style,
    rows = 4:(nrow(instructions) + 3),
    cols = 2,
    gridExpand = TRUE,
    stack = TRUE
  )

  writeDataTable(
    workbook,
    "Pairing Review",
    review_table,
    tableStyle = "TableStyleMedium2"
  )

  freezePane(
    workbook,
    "Pairing Review",
    firstActiveRow = 2,
    firstActiveCol = 6
  )

  editable_fields <- c(
    "Fire_ID_Final",
    "Pairing_Type_Final",
    "Include",
    "Decision_Status",
    "Evidence_Source",
    "Reviewer",
    "Review_Date",
    "Decision_Notes"
  )

  editable_columns <- match(
    editable_fields,
    names(review_table)
  )

  review_rows <- 2:(nrow(review_table) + 1)

  addStyle(
    workbook,
    "Pairing Review",
    input_style,
    rows = review_rows,
    cols = editable_columns,
    gridExpand = TRUE,
    stack = TRUE
  )

  dataValidation(
    workbook,
    "Pairing Review",
    cols = match("Pairing_Type_Final", names(review_table)),
    rows = review_rows,
    type = "list",
    value = paste0('"', paste(pairing_type_values, collapse = ","), '"')
  )

  dataValidation(
    workbook,
    "Pairing Review",
    cols = match("Include", names(review_table)),
    rows = review_rows,
    type = "list",
    value = paste0('"', paste(include_values, collapse = ","), '"')
  )

  dataValidation(
    workbook,
    "Pairing Review",
    cols = match("Decision_Status", names(review_table)),
    rows = review_rows,
    type = "list",
    value = paste0('"', paste(decision_status_values, collapse = ","), '"')
  )

  addStyle(
    workbook,
    "Pairing Review",
    date_style,
    rows = review_rows,
    cols = match("Review_Date", names(review_table)),
    gridExpand = TRUE,
    stack = TRUE
  )

  high_priority_rows <- which(
    str_starts(review_table$Review_Priority, "1 -")
  ) + 1

  medium_priority_rows <- which(
    str_starts(review_table$Review_Priority, "2 -")
  ) + 1

  addStyle(
    workbook,
    "Pairing Review",
    high_priority_style,
    rows = high_priority_rows,
    cols = match("Review_Priority", names(review_table)),
    gridExpand = TRUE,
    stack = TRUE
  )

  addStyle(
    workbook,
    "Pairing Review",
    medium_priority_style,
    rows = medium_priority_rows,
    cols = match("Review_Priority", names(review_table)),
    gridExpand = TRUE,
    stack = TRUE
  )

  review_widths <- rep(15, ncol(review_table))
  review_widths[match("Review_Priority", names(review_table))] <- 30
  review_widths[match("Study_ID", names(review_table))] <- 28
  review_widths[match("Comparison_ID", names(review_table))] <- 30
  review_widths[match("Burned_Site_Name", names(review_table))] <- 28
  review_widths[match("Reference_Site_Name", names(review_table))] <- 32
  review_widths[match("candidate_pair_id", names(review_table))] <- 45
  review_widths[match("Fire_name_metadata", names(review_table))] <- 30
  review_widths[match("Evidence_Source", names(review_table))] <- 35
  review_widths[match("Decision_Notes", names(review_table))] <- 55

  setColWidths(
    workbook,
    "Pairing Review",
    cols = seq_along(review_widths),
    widths = review_widths
  )

  addStyle(
    workbook,
    "Pairing Review",
    wrap_style,
    rows = review_rows,
    cols = seq_len(ncol(review_table)),
    gridExpand = TRUE,
    stack = TRUE
  )

  writeDataTable(
    workbook,
    "Study Summary",
    study_summary,
    tableStyle = "TableStyleMedium2"
  )

  freezePane(
    workbook,
    "Study Summary",
    firstActiveRow = 2,
    firstActiveCol = 2
  )

  setColWidths(
    workbook,
    "Study Summary",
    cols = seq_len(ncol(study_summary)),
    widths = c(28, 30, 18, 16, 22, 18, 25, 40, 40)
  )

  addStyle(
    workbook,
    "Study Summary",
    wrap_style,
    rows = 2:(nrow(study_summary) + 1),
    cols = seq_len(ncol(study_summary)),
    gridExpand = TRUE,
    stack = TRUE
  )

  writeDataTable(
    workbook,
    "Controlled Values",
    controlled_values,
    tableStyle = "TableStyleMedium2"
  )

  setColWidths(
    workbook,
    "Controlled Values",
    cols = 1:2,
    widths = c(28, 42)
  )

  dir.create(
    dirname(review_workbook_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  saveWorkbook(
    workbook,
    review_workbook_path,
    overwrite = TRUE
  )

  message(
    "Created review workbook: ",
    review_workbook_path
  )
}

# ---- 7. Validate and import the completed workbook -------------------------

if (review_action == "import") {

  reviewed_table <- readWorkbook(
    review_workbook_path,
    sheet = "Pairing Review",
    detectDates = TRUE,
    check.names = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      across(
        c(
          Fire_ID_Final,
          Pairing_Type_Final,
          Include,
          Decision_Status,
          Evidence_Source,
          Reviewer,
          Decision_Notes
        ),
        ~ na_if(str_trim(as.character(.x)), "")
      )
    )

  validation_results <- reviewed_table %>%
    transmute(
      candidate_pair_id,
      Study_ID,
      Comparison_ID,
      Decision_Status,
      Validation_Issue = case_when(
        is.na(Decision_Status) ~
          "Decision_Status is blank.",
        !Decision_Status %in% decision_status_values ~
          "Decision_Status is not an allowed value.",
        Decision_Status == "approved" &
          (is.na(Include) | Include != "TRUE") ~
          "Approved rows must have Include = TRUE.",
        Decision_Status == "approved" & is.na(Fire_ID_Final) ~
          "Approved rows require Fire_ID_Final.",
        Decision_Status == "approved" & is.na(Pairing_Type_Final) ~
          "Approved rows require Pairing_Type_Final.",
        Decision_Status == "approved" &
          !Pairing_Type_Final %in% pairing_type_values ~
          "Pairing_Type_Final is not an allowed value.",
        Decision_Status == "excluded" &
          (is.na(Include) | Include != "FALSE") ~
          "Excluded rows must have Include = FALSE.",
        Decision_Status == "excluded" & is.na(Decision_Notes) ~
          "Excluded rows require a reason in Decision_Notes.",
        Decision_Status %in% c(
          "pending_author_review",
          "needs_source_check",
          "needs_team_decision"
        ) ~
          "Review is not final.",
        is.na(Evidence_Source) ~
          "Evidence_Source is blank.",
        is.na(Reviewer) ~
          "Reviewer is blank.",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Validation_Issue))

  dir.create(
    dirname(validation_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  write_csv(
    validation_results,
    validation_path,
    na = ""
  )

  if (nrow(validation_results) > 0 & !allow_partial_import) {
    stop(
      "Review validation failed. See: ",
      validation_path
    )
  }

  reviewed_export <- reviewed_table %>%
    select(
      any_of(names(pairing_decisions)),
      Evidence_Source,
      Reviewer,
      Review_Date
    )

  write_csv(
    reviewed_export,
    reviewed_decisions_path,
    na = ""
  )

  message(
    "Imported ",
    nrow(reviewed_export),
    " reviewed rows to: ",
    reviewed_decisions_path
  )

  message(
    "Validation issues: ",
    nrow(validation_results),
    ". See: ",
    validation_path
  )
}

