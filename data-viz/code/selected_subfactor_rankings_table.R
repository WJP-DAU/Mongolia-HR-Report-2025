## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Selected Countries Subfactor Rankings Table
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 0. Setup ------------------------------------------------------------------------------------------------

library(pacman)
p_load(char = c("tidyverse", "openxlsx"))

# Path configuration
if(Sys.info()["user"] == "santiagopardo"){
  path2DA <- "/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/Data Analytics/6. Country Reports/Mongolia-HR-Report-2025/data-viz"
} else {
  path2DA <- "INSERT_YOUR_PATH_HERE"
}

## 1. Define countries -------------------------------------------------------------------------------------

MAIN_COUNTRY <- "Mongolia"

COMP_COUNTRIES <- c(
  "Kazakhstan",
  "Hong Kong SAR, China",
  "China",
  "Kyrgyz Republic",
  "Uzbekistan",
  "Nepal",
  "Korea, Rep.",
  "Japan"
)

SELECTED_COUNTRIES <- c(MAIN_COUNTRY, COMP_COUNTRIES)

## 2. Load data --------------------------------------------------------------------------------------------

master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
)

data_2025 <- master_data %>%
  filter(Year == "2025")

## 3. Identify subfactors ----------------------------------------------------------------------------------

subfactor_cols <- names(data_2025) %>%
  stringr::str_subset("^[1-8]\\.[0-9]+\\.")

subfactor_labels <- tibble(
  column = subfactor_cols,
  code = stringr::str_extract(column, "^[1-8]\\.[0-9]+"),
  label = column %>%
    stringr::str_remove("^[1-8]\\.[0-9]+\\.*") %>%
    stringr::str_replace_all("\\.", " ") %>%
    stringr::str_squish()
)

## 4. Ranking function -------------------------------------------------------------------------------------

create_subfactor_table <- function(subfactor_col) {
  score_data <- data_2025 %>%
    transmute(
      Country,
      Region,
      score = as.numeric(.data[[subfactor_col]])
    ) %>%
    filter(!is.na(score))

  total_global <- nrow(score_data)

  global_ranks <- score_data %>%
    arrange(desc(score), Country) %>%
    mutate(
      global_rank = row_number(),
      total_global = total_global
    ) %>%
    select(Country, global_rank, total_global)

  regional_ranks <- score_data %>%
    group_by(Region) %>%
    arrange(desc(score), Country, .by_group = TRUE) %>%
    mutate(
      regional_rank = row_number(),
      total_regional = n()
    ) %>%
    ungroup() %>%
    select(Country, regional_rank, total_regional)

  score_data %>%
    left_join(global_ranks, by = "Country") %>%
    left_join(regional_ranks, by = "Country") %>%
    filter(Country %in% SELECTED_COUNTRIES) %>%
    arrange(desc(score), Country) %>%
    mutate(selected_rank = row_number()) %>%
    transmute(
      `Selected Countries Rank` = selected_rank,
      Country = as.character(Country),
      Region,
      Score = round(score, 2),
      `Regional Rank` = paste0(regional_rank, "/", total_regional),
      `Global Rank` = paste0(global_rank, "/", total_global)
    )
}

## 5. Export to Excel --------------------------------------------------------------------------------------

wb <- createWorkbook()

title_style <- createStyle(
  fontSize = 13,
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#2A2A94",
  halign = "left",
  valign = "center"
)

subtitle_style <- createStyle(
  fontSize = 10,
  fontColour = "#524F4C",
  halign = "left",
  valign = "center"
)

header_style <- createStyle(
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#524F4C",
  halign = "center",
  valign = "center",
  border = "bottom"
)

country_style <- createStyle(halign = "left", valign = "center")

text_style <- createStyle(halign = "center", valign = "center")

score_style <- createStyle(
  halign = "center",
  valign = "center",
  numFmt = "0.00"
)

mongolia_style <- createStyle(
  fgFill = "#E8E8F4",
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)

for (i in seq_len(nrow(subfactor_labels))) {
  subfactor <- subfactor_labels[i, ]
  sheet_name <- subfactor$code
  output_table <- create_subfactor_table(subfactor$column)

  addWorksheet(wb, sheet_name)

  writeData(wb, sheet_name, paste0(subfactor$code, " ", subfactor$label), startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:ncol(output_table), rows = 1)
  addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:ncol(output_table), gridExpand = TRUE)

  writeData(wb, sheet_name, "Selected countries, 2025 scores. Regional rank is calculated within each country's WJP region; global rank is calculated across all countries with available 2025 data.", startRow = 2, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:ncol(output_table), rows = 2)
  addStyle(wb, sheet_name, subtitle_style, rows = 2, cols = 1:ncol(output_table), gridExpand = TRUE)

  writeData(wb, sheet_name, output_table, startRow = 4, startCol = 1)
  addStyle(wb, sheet_name, header_style, rows = 4, cols = 1:ncol(output_table), gridExpand = TRUE)
  addStyle(wb, sheet_name, text_style, rows = 5:(nrow(output_table) + 4), cols = c(1, 5, 6), gridExpand = TRUE)
  addStyle(wb, sheet_name, country_style, rows = 5:(nrow(output_table) + 4), cols = 2, gridExpand = TRUE)
  addStyle(wb, sheet_name, text_style, rows = 5:(nrow(output_table) + 4), cols = 3, gridExpand = TRUE)
  addStyle(wb, sheet_name, score_style, rows = 5:(nrow(output_table) + 4), cols = 4, gridExpand = TRUE)

  mongolia_row <- which(output_table$Country == MAIN_COUNTRY) + 4
  addStyle(wb, sheet_name, mongolia_style, rows = mongolia_row, cols = 1:ncol(output_table), gridExpand = TRUE, stack = TRUE)

  addFilter(wb, sheet_name, rows = 4, cols = 1:ncol(output_table))
  freezePane(wb, sheet_name, firstActiveRow = 5)
  setColWidths(wb, sheet_name, cols = 1, widths = 22)
  setColWidths(wb, sheet_name, cols = 2, widths = 24)
  setColWidths(wb, sheet_name, cols = 3, widths = 28)
  setColWidths(wb, sheet_name, cols = 4:6, widths = 15)
  setRowHeights(wb, sheet_name, rows = 1, heights = 24)
  setRowHeights(wb, sheet_name, rows = 2, heights = 32)
}

output_path <- file.path(path2DA, "outputs/Selected_Countries_Subfactor_Rankings.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE)

message("Table exported to: outputs/Selected_Countries_Subfactor_Rankings.xlsx")
