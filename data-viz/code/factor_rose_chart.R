## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Mongolia Factor Rose Chart (Coxcomb)
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 0. Setup ------------------------------------------------------------------------------------------------

library(pacman)
p_load(char = c("tidyverse", "openxlsx", "ggtext", "sysfonts", "showtext"))

# Path configuration
if(Sys.info()["user"] == "santiagopardo"){
  path2DA <- "/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/Data Analytics/6. Country Reports/Mongolia-HR-Report-2025/data-viz"
} else {
  path2DA <- "INSERT_YOUR_PATH_HERE"
}

# Load fonts
path2fonts <- file.path(path2DA, "6. Country Reports/0. Fonts", fsep = "/")
if (file.exists(file.path(path2fonts, "Lato-Regular.ttf"))) {
  font_add(family = "Lato Full",
           regular = file.path(path2fonts, "Lato-Regular.ttf"),
           bold = file.path(path2fonts, "Lato-Bold.ttf"))
  font_add(family = "Lato Medium",
           regular = file.path(path2fonts, "Lato-Medium.ttf"))
}
showtext_auto()

## 1. Load and prepare data --------------------------------------------------------------------------------

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

# Labels for the chart (matching the image layout - value on top, factor below)
factor_labels <- c(
  "Constraints on\nGovernment Powers",
  "Absence of\nCorruption",
  "Open\nGovernment",
  "Fundamental\nRights",
  "Order\nand Security",
  "Regulatory\nEnforcement",
  "Civil\nJustice",
  "Criminal\nJustice"
)

# Get Mongolia 2025 data
mongolia_data <- master_data %>%
  filter(Country == MAIN_COUNTRY, Year == "2025") %>%
  select(all_of(factor_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Value") %>%
  mutate(
    Value = as.numeric(Value),
    Factor_Num = row_number(),
    Label = factor_labels[Factor_Num]
  )

print(mongolia_data)

## 2. Prepare data for Rose Chart --------------------------------------------------------------------------

n_factors <- 8
angle_per_sector <- 360 / n_factors

# Create data for rose chart sectors
rose_data <- mongolia_data %>%
  mutate(
    # Each sector spans angle_per_sector degrees
    xmin = (Factor_Num - 1) * angle_per_sector,
    xmax = Factor_Num * angle_per_sector,
    ymin = 0,
    ymax = Value,
    # Angle for label placement (center of each sector)
    angle_center = (xmin + xmax) / 2,
    # Convert to radians for label positioning
    angle_rad = (90 - angle_center) * pi / 180,
    # Label position (outside the max circle)
    label_r = 1.25,
    label_x = label_r * cos(angle_rad),
    label_y = label_r * sin(angle_rad),
    # Combined label: value on top, factor name below
    combined_label = paste0("**", sprintf("%.2f", Value), "**\n", Label)
  )

## 3. Create Rose Chart ------------------------------------------------------------------------------------

rose_chart <- ggplot(rose_data) +
  # Reference circles (dashed)
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "#d1cfd1", linewidth = 0.3) +

  # Outer circle
  geom_hline(yintercept = 1, linetype = "dotted", color = "#d1cfd1", linewidth = 0.3) +

  # Rose petals (bars in polar coordinates)
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#2A2A94",
    color = "white",
    linewidth = 0.5
  ) +

  # Labels outside the chart (value + factor name)
  geom_richtext(
    aes(x = angle_center, y = 1.3, label = combined_label),
    fill = NA,
    label.color = NA,
    size = 3,
    family = "Lato Full",
    color = "#524F4C",
    lineheight = 1.1
  ) +

  # Convert to polar coordinates
 coord_polar(start = -pi/8) +

  # Set y-axis limits
  scale_y_continuous(limits = c(0, 1.6), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0)) +

  # Theme
 theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(30, 30, 30, 30),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )

## 4. Save the chart ---------------------------------------------------------------------------------------

ggsave(
  plot = rose_chart,
  filename = file.path(path2DA, "outputs/Mongolia_Factor_Rose.svg"),
  width = 180,
  height = 180,
  units = "mm",
  dpi = 72,
  device = "svg"
)

message("Chart saved to: outputs/Mongolia_Factor_Rose.svg")
