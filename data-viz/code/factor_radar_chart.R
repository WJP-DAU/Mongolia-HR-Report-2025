## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Mongolia Factor Radar Chart
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

# Labels for the chart (matching the image layout)
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

## 2. Create Radar Chart -----------------------------------------------------------------------------------

# Radar chart parameters
n_factors <- 8
central_distance <- 0.15

# Function to generate circle coordinates
circle_coords <- function(r, n_axis = n_factors) {
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  x <- r * cos(fi)
  y <- r * sin(fi)
  tibble(x, y, r)
}

# Function to generate axis lines
axis_coords <- function(n_axis = n_factors) {
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
  x1 <- central_distance * cos(fi)
  y1 <- central_distance * sin(fi)
  x2 <- (1 + central_distance) * cos(fi)
  y2 <- (1 + central_distance) * sin(fi)
  tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
}

# Function to generate label coordinates
label_coords <- function(r = 1.55, n_axis = n_factors) {
  fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
  x <- r * cos(fi)
  y <- r * sin(fi)
  tibble(x, y)
}

# Generate data polygon coordinates
polygon_coords <- function(values, n_axis = n_factors) {
  fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
  r <- c(values, values[1]) + central_distance
  x <- r * cos(fi)
  y <- r * sin(fi)
  tibble(x, y)
}

# Prepare polygon data
data_polygon <- polygon_coords(mongolia_data$Value)

# Prepare label data with combined value + factor name
label_data <- label_coords() %>%
  mutate(
    factor_label = mongolia_data$Label,
    value = mongolia_data$Value,
    # Combined label: value in bold blue on top, factor name in gray below (all centered)
    combined_label = paste0(
      "<span style='color:#2A2A94;font-weight:bold;font-size:12pt'>",
      sprintf("%.2f", value),
      "</span><br>",
      "<span style='color:#524F4C;font-size:9pt'>",
      gsub("\n", "<br>", factor_label),
      "</span>"
    )
  )

## 3. Build the plot ---------------------------------------------------------------------------------------

radar_chart <- ggplot() +
  # Outer ring
  geom_polygon(
    data = circle_coords(1 + central_distance),
    aes(x = x, y = y),
    fill = NA,
    color = "#d1cfd1",
    linetype = "dotted"
  ) +

  # Inner rings (0.25, 0.5, 0.75)
  geom_path(
    data = map_df(c(0.25, 0.5, 0.75) + central_distance, circle_coords),
    aes(x = x, y = y, group = r),
    color = "#d1cfd1",
    linetype = "dashed"
  ) +

  # Center ring (zero)
  geom_polygon(
    data = circle_coords(central_distance),
    aes(x = x, y = y),
    fill = "white",
    color = "#d1cfd1"
  ) +

  # Axis lines
  geom_line(
    data = axis_coords(),
    aes(x = x, y = y, group = id),
    color = "#d1cfd1"
  ) +

  # Data polygon (filled)
  geom_polygon(
    data = data_polygon,
    aes(x = x, y = y),
    fill = "#2A2A94",
    alpha = 0.85
  ) +

  # Data polygon outline
  geom_path(
    data = data_polygon,
    aes(x = x, y = y),
    color = "#2A2A94",
    linewidth = 1
  ) +

  # Combined labels: value (bold blue) on top, factor name (gray) below - all centered
  geom_richtext(
    data = label_data,
    aes(x = x, y = y, label = combined_label),
    hjust = 0.5,
    vjust = 0.5,
    family = "Lato Full",
    size = 3,
    fill = NA,
    label.color = NA,
    lineheight = 1.1
  ) +

  # Aesthetics

  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = 0.3)) +
  scale_y_continuous(expand = expansion(mult = 0.3)) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

## 4. Save the chart ---------------------------------------------------------------------------------------

ggsave(
  plot = radar_chart,
  filename = file.path(path2DA, "outputs/Mongolia_Factor_Radar.svg"),
  width = 150,
  height = 150,
  units = "mm",
  dpi = 72,
  device = "svg"
)

message("Chart saved to: outputs/Mongolia_Factor_Radar.svg")
