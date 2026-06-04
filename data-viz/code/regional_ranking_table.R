## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            East Asia & Pacific Regional Ranking Table
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

master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
)

# Clean column names for easier access
master_data <- master_data %>%
  rename(
    Overall_Score = `WJP.Rule.of.Law.Index:.Overall.Score`
  ) %>%
  mutate(Overall_Score = as.numeric(Overall_Score))

## 2. Get 2025 and 2020 data -------------------------------------------------------------------------------

# 2025 data for all countries (for global ranking)
data_2025 <- master_data %>%
  filter(Year == "2025") %>%
  select(Country, Region, Overall_Score) %>%
  rename(Overall_Score_2025 = Overall_Score)

# 2020 data
data_2020 <- master_data %>%
  filter(Year == "2020") %>%
  select(Country, Overall_Score) %>%
  rename(Overall_Score_2020 = Overall_Score)

# Merge 2025 and 2020 data
combined_data <- data_2025 %>%
  left_join(data_2020, by = "Country") %>%
  filter(!is.na(Overall_Score_2025))

## 3. Calculate rankings -----------------------------------------------------------------------------------

# Calculate Global Rank (all countries)
combined_data <- combined_data %>%
  arrange(desc(Overall_Score_2025)) %>%
  mutate(Global_Rank = row_number())

total_countries <- nrow(combined_data)

# Filter to East Asia & Pacific only (using Region column)
eap_data <- combined_data %>%
  filter(Region == "East Asia and Pacific")

# Calculate Regional Rank
eap_data <- eap_data %>%
  arrange(desc(Overall_Score_2025)) %>%
  mutate(Regional_Rank = row_number())

total_eap <- nrow(eap_data)

# Calculate 6-Year % Change (2020-2025)
eap_data <- eap_data %>%
  mutate(
    Change_6yr = round(((Overall_Score_2025 - Overall_Score_2020) / Overall_Score_2020) * 100, 1)
  )

## 4. Format output table ----------------------------------------------------------------------------------

output_table <- eap_data %>%
  arrange(Regional_Rank) %>%
  transmute(
    Country = Country,
    `Overall Score` = round(Overall_Score_2025, 2),
    `Regional Rank` = paste0(Regional_Rank, "/", total_eap),
    `Global Rank` = paste0(Global_Rank, "/", total_countries),
    `6-Year % Change (2020-2025)` = paste0(ifelse(Change_6yr > 0, "+", ""), Change_6yr, "%")
  )

# Print preview
print(as_tibble(output_table), n = 20)

## 5. Export to Excel --------------------------------------------------------------------------------------

write.xlsx(
  output_table,
  file.path(path2DA, "outputs/EAP_Regional_Ranking_Table.xlsx"),
  overwrite = TRUE
)

message("Table exported to: outputs/EAP_Regional_Ranking_Table.xlsx")
