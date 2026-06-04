## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Mongolia Factor Rankings Table
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

## 1. Load data --------------------------------------------------------------------------------------------

MAIN_COUNTRY <- "Mongolia"

master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
)

# Factor columns
factor_cols <- c(
  "Factor.1:.Constraints.on.Government.Powers",
  "Factor.2:.Absence.of.Corruption",
  "Factor.3:.Open.Government",
  "Factor.4:.Fundamental.Rights",
  "Factor.5:.Order.and.Security",
  "Factor.6:.Regulatory.Enforcement",
  "Factor.7:.Civil.Justice",
  "Factor.8:.Criminal.Justice"
)

factor_labels <- c(
  "Constraints on Government Powers",
  "Absence of Corruption",
  "Open Government",
  "Fundamental Rights",
  "Order and Security",
  "Regulatory Enforcement",
  "Civil Justice",
  "Criminal Justice"
)

## 2. Get 2025 data for all countries ---------------------------------------------------------------------

data_2025 <- master_data %>%
  filter(Year == "2025") %>%
  select(Country, Region, all_of(factor_cols))

data_2020 <- master_data %>%
  filter(Year == "2020") %>%
  select(Country, all_of(factor_cols))

# Total countries
total_countries <- nrow(data_2025)

# Total EAP countries
total_eap <- data_2025 %>%
  filter(Region == "East Asia and Pacific") %>%
  nrow()

## 3. Calculate rankings for each factor ------------------------------------------------------------------

results <- list()

for (i in seq_along(factor_cols)) {
  col <- factor_cols[i]

  # 2025 scores for all countries
  scores_2025 <- data_2025 %>%
    select(Country, Region, score_2025 = all_of(col)) %>%
    mutate(score_2025 = as.numeric(score_2025)) %>%
    filter(!is.na(score_2025))

  # 2020 scores
  scores_2020 <- data_2020 %>%
    select(Country, score_2020 = all_of(col)) %>%
    mutate(score_2020 = as.numeric(score_2020))

  # Merge and calculate ranks
  combined <- scores_2025 %>%
    left_join(scores_2020, by = "Country") %>%
    # Global rank
    arrange(desc(score_2025)) %>%
    mutate(global_rank = row_number()) %>%
    # Regional rank (EAP only)
    group_by(Region) %>%
    arrange(desc(score_2025), .by_group = TRUE) %>%
    mutate(regional_rank = row_number()) %>%
    ungroup()

  # Get Mongolia's data
  mongolia_row <- combined %>%
    filter(Country == MAIN_COUNTRY) %>%
    mutate(
      change_6yr = round(((score_2025 - score_2020) / score_2020) * 100, 1),
      factor_num = i,
      factor_name = factor_labels[i]
    )

  results[[i]] <- mongolia_row
}

## 4. Combine results and format --------------------------------------------------------------------------

final_table <- bind_rows(results) %>%
  arrange(factor_num) %>%
  transmute(
    Factor = paste0(factor_num, " ", factor_name),
    `Global Rank` = paste0(global_rank, "/", total_countries),
    `Regional Rank` = paste0(regional_rank, "/", total_eap),
    `6-Year % Change (2020-2025)` = paste0(ifelse(change_6yr > 0, "+", ""), change_6yr, "%")
  )

# Print preview
print(final_table)

## 5. Export to Excel -------------------------------------------------------------------------------------

write.xlsx(
  final_table,
  file.path(path2DA, "outputs/Mongolia_Factor_Rankings.xlsx"),
  overwrite = TRUE
)

message("Table exported to: outputs/Mongolia_Factor_Rankings.xlsx")
