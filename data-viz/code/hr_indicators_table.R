## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Human Rights Indicators Table - Mongolia Country Report
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

## 1. Define countries and indicators ---------------------------------------------------------------------

# Countries in order for the table
COUNTRIES <- c(
  "Mongolia",
  "Nepal",
  "Hong Kong SAR, China",
  "Kazakhstan",
  "Uzbekistan",
  "Kyrgyz Republic",
  "Korea, Rep.",
  "China",
  "Japan"
)

# Indicators grouped by category
indicators <- list(
  "Civil and Political Rights" = c(
    "4.1" = "4.1.Equal.treatment.and.absence.of.discrimination",
    "4.2" = "4.2.The.right.to.life.and.security.of.the.person.is.effectively.guaranteed",
    "4.4" = "4.4.Freedom.of.opinion.and.expression.is.effectively.guaranteed",
    "4.5" = "4.5.Freedom.of.belief.and.religion.is.effectively.guaranteed",
    "4.6" = "4.6.Freedom.from.arbitrary.interference.with.privacy.is.effectively.guaranteed",
    "4.7" = "4.7.Freedom.of.assembly.and.association.is.effectively.guaranteed"
  ),
  "Participation and Access to Information" = c(
    "1.5" = "1.5.Government.powers.are.subject.to.non-governmental.checks",
    "3.2" = "3.2.Right.to.information",
    "3.3" = "3.3.Civic.participation",
    "3.4" = "3.4.Complaint.mechanisms"
  ),
  "Labor and Property Rights" = c(
    "4.8" = "4.8.Fundamental.labor.rights.are.effectively.guaranteed",
    "6.5" = "6.5.The.government.does.not.expropriate.without.lawful.process.and.adequate.compensation"
  ),
  "Fair and Impartial Justice" = c(
    "4.3" = "4.3.Due.process.of.the.law.and.rights.of.the.accused",
    "6.4" = "6.4.Due.process.is.respected.in.administrative.proceedings",
    "6.3" = "6.3.Administrative.proceedings.are.conducted.without.unreasonable.delay",
    "7.1" = "7.1.People.can.access.and.afford.civil.justice",
    "7.2" = "7.2.Civil.justice.is.free.of.discrimination",
    "7.4" = "7.4.Civil.justice.is.free.of.improper.government.influence",
    "8.4" = "8.4.Criminal.system.is.impartial",
    "8.6" = "8.6.Criminal.system.is.free.of.improper.government.influence"
  )
)

# Short labels for indicators (sin Title Case según estándares WJP)
indicator_labels <- c(
  "4.1" = "Equality and discrimination",
  "4.2" = "The right to life and security of the person is effectively guaranteed",
  "4.4" = "Freedom of expression and opinion",
  "4.5" = "Freedom of belief and religion",
  "4.6" = "Freedom from interference with privacy",
  "4.7" = "Freedom of assembly and association",
  "1.5" = "Non-governmental checks on government power",
  "3.2" = "Right to information",
  "3.3" = "Civic participation",
  "3.4" = "Access to complaint mechanisms",
  "4.8" = "Labor rights",
  "6.5" = "Protection from expropriation without due process",
  "4.3" = "Due process of the law and rights of the accused",
  "6.4" = "Due process is respected in administrative proceedings",
  "6.3" = "Administrative proceedings are conducted without unreasonable delay",
  "7.1" = "Access to and affordability of civil justice",
  "7.2" = "Discrimination in civil justice",
  "7.4" = "Improper government influence in civil justice",
  "8.4" = "Impartiality in criminal justice",
  "8.6" = "Improper government influence in criminal justice"
)

## 2. Load data --------------------------------------------------------------------------------------------

master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
)

# Filter to 2025 data only
data_2025 <- master_data %>%
  filter(Year == "2025")

# Get all indicator columns
all_indicators <- unlist(indicators, use.names = FALSE)

## 3. Calculate Regional Average (9 studied countries only) -----------------------------------------------

regional_avg <- data_2025 %>%
  filter(Country %in% COUNTRIES) %>%
  select(all_of(all_indicators)) %>%
  summarise(across(everything(), ~ round(mean(as.numeric(.), na.rm = TRUE), 2))) %>%
  mutate(Country = "Regional Average") %>%
  select(Country, everything())

## 4. Get country data -------------------------------------------------------------------------------------

country_data <- data_2025 %>%
  filter(Country %in% COUNTRIES) %>%
  select(Country, all_of(all_indicators))

# Ensure numeric values
country_data <- country_data %>%
  mutate(across(-Country, ~ round(as.numeric(.), 2)))

# Order countries as specified
country_data <- country_data %>%
  mutate(Country = factor(Country, levels = COUNTRIES)) %>%
  arrange(Country) %>%
  mutate(Country = as.character(Country))

# Combine with regional average
combined_data <- bind_rows(country_data, regional_avg)

## 5. Build output table -----------------------------------------------------------------------------------

# Create empty dataframe for output
output_rows <- list()
row_idx <- 1

all_countries <- c(COUNTRIES, "Regional Average")

for (category in names(indicators)) {
  # Add category header row
  header_row <- tibble(Indicator = category)
  for (country in all_countries) {
    header_row[[country]] <- NA_real_
  }
  output_rows[[row_idx]] <- header_row
  row_idx <- row_idx + 1

  # Add indicator rows
  for (code in names(indicators[[category]])) {
    col_name <- indicators[[category]][[code]]
    label <- paste0(code, " ", indicator_labels[[code]])

    row_data <- tibble(Indicator = label)

    for (country in all_countries) {
      value <- combined_data %>%
        filter(Country == country) %>%
        pull(!!sym(col_name))

      row_data[[country]] <- ifelse(length(value) > 0 && !is.na(value), value, NA_real_)
    }

    output_rows[[row_idx]] <- row_data
    row_idx <- row_idx + 1
  }
}

output_table <- bind_rows(output_rows)

# Print preview
print(as_tibble(output_table), n = 30)

## 6. Export to Excel with formatting ----------------------------------------------------------------------

wb <- createWorkbook()
addWorksheet(wb, "HR Indicators")

# Write data
writeData(wb, "HR Indicators", output_table, startRow = 1, startCol = 1)

# Define styles for category headers
style_civil <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#C41E3A",  # Red/pink
  textDecoration = "bold",
  halign = "left"
)

style_participation <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#2A5CAA",  # Blue
  textDecoration = "bold",
  halign = "left"
)

style_labor <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#1E3A5F",  # Dark blue
  textDecoration = "bold",
  halign = "left"
)

style_justice <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#8B008B",  # Purple/magenta
  textDecoration = "bold",
  halign = "left"
)

# Style for data cells
style_data <- createStyle(
  halign = "center",
  numFmt = "0.00"
)

# Style for indicator names
style_indicator <- createStyle(
  halign = "left",
  indent = 1
)

# Header style
style_header <- createStyle(
  textDecoration = "bold",
  halign = "center",
  border = "bottom"
)

# Apply header style
addStyle(wb, "HR Indicators", style_header, rows = 1, cols = 1:ncol(output_table), gridExpand = TRUE)

# Find category rows and apply styles
category_styles <- list(
  "Civil and Political Rights" = style_civil,
  "Participation and Access to Information" = style_participation,
  "Labor and Property Rights" = style_labor,
  "Fair and Impartial Justice" = style_justice
)

for (i in 1:nrow(output_table)) {
  indicator_val <- output_table$Indicator[i]

  if (indicator_val %in% names(category_styles)) {
    # Category header row
    addStyle(wb, "HR Indicators", category_styles[[indicator_val]],
             rows = i + 1, cols = 1:ncol(output_table), gridExpand = TRUE)
  } else {
    # Data row - apply indicator style to first column
    addStyle(wb, "HR Indicators", style_indicator, rows = i + 1, cols = 1)
    # Apply data style to value columns
    addStyle(wb, "HR Indicators", style_data, rows = i + 1, cols = 2:ncol(output_table), gridExpand = TRUE)
  }
}

# Set column widths
setColWidths(wb, "HR Indicators", cols = 1, widths = 55)
setColWidths(wb, "HR Indicators", cols = 2:ncol(output_table), widths = 12)

# Save workbook
saveWorkbook(wb, file.path(path2DA, "outputs/Mongolia_HR_Indicators_Table.xlsx"), overwrite = TRUE)

message("Table exported to: outputs/Mongolia_HR_Indicators_Table.xlsx")
