## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Country Report - RunMe File (Parametrized)
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 0.  Presettings -------------------------------------------------------------------------------------------

# Params
MAIN_COUNTRY    <- "Mongolia"
COMP_COUNTRIES  <- c(
  "Kazakhstan",
  "Hong Kong SAR, China",
  "China",
  "Kyrgyz Republic",
  "Uzbekistan",
  "Nepal",
  "Korea, Rep.",
  "Japan"
)
colors4plot <- c(
  "Mongolia"              = "#2A2A94", 
  "Kazakhstan"            = "#A90099",
  "Hong Kong SAR, China"  = "#3273FF",  
  "China"                 = "#EFA700",   
  "Kyrgyz Republic"       = "#00B67F",
  "Uzbekistan"            = "#FA4D57",   
  "Nepal"                 = "#A68BF2",
  "Korea, Rep."           = "#404040",
  "Japan"                 = "#C46D5E"
)

# load modules
modules <- c("settings", 
             "wrangleData",
             "data_viz")

for (mod in modules){
  source(
    paste0(
      "code/", mod, ".R")
    )
}

# --- Cargar datos (ajusta path2DA desde settings) ---
master_data <- read.xlsx(
  file.path(path2DA, "inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"),
  sheet = "Historical Data",
  check.names = FALSE
) %>%
  filter(Country %in% c(MAIN_COUNTRY, COMP_COUNTRIES)) %>%
  rename_with(~ ifelse(grepl("^[0-9]", .x), substr(.x, 1, 3), .x))

outline <- read.xlsx(
  file.path(path2DA, "inputs/Mongolia_outline.xlsx"),
  sheet = "figure_map"
)

## 1.  Wrangle data ------------------------------------------------------------------------------------------

chart_list <- setNames(as.list(outline %>% pull(id)), outline %>% pull(id))

data_points <- purrr::map(
  chart_list,
  ~ wrangleData(
    figid                 = .x,
    outline               = outline,
    master_data           = master_data,
    main_country          = MAIN_COUNTRY,
    comparison_countries  = COMP_COUNTRIES
  )
)

## 2.  Create Viz --------------------------------------------------------------------------------------------

invisible(lapply(
  chart_list,
  function(figid) {
    callVisualizer(
      figid        = figid,
      outline      = outline,
      data_points  = data_points,
      main_country = MAIN_COUNTRY
    )
  }
))