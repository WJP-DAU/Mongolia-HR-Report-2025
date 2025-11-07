## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Thailand Country Report - RunMe File
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: This is the single-call file for generating data points and visualizations for the EU Thematic
##          reports. 
##          
##          PRESETTINGS:  Load settings and function definitions, data
##          WRANGLE DATA: Create chart lists which are passed to the appropriate wrangle data function to acquire 
##                        data points for visualizations.
##          CREATE VIZ:   Call and apply the appropriate visualization function. 
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load modules
modules <- c(
  "settings", 
  "functions"
  )

for (mod in modules){
  source(
    paste0("code/",mod,".R")
  )
}

# load data - replace with SP path
master_data <- read.xlsx(
  file.path(
    path2DA,"inputs/FINAL_2025_wjp_rule_of_law_index_HISTORICAL_DATA_FILE.xlsx"
  ),
  sheet = "Historical Data",
  check.names = FALSE
) %>%
  filter(
    # (Year %in% c("2023", "2024") & 
       Country %in% c(
         "Mongolia",
         "Kazakhstan", 
         "Hong Kong SAR, China",
         "China", 
         "Kyrgyz Republic", 
         "Uzbekistan", 
         "Nepal", 
         "Georgia")
     )  %>%  
  rename_with(
    ~ ifelse(grepl("^[0-9]", .x), 
             substr(.x, 1, 3), .x) # renaming with numerical indicator
  )

# replace with SP path
outline <- read.xlsx(
  file.path(
    path2DA,"/inputs/Mongolia_outline.xlsx"),
  sheet = "figure_map"
) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Wrangle data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

chart_list <- setNames(
  as.list(outline %>% pull(id)), 
  outline %>% pull(id))
  

data_points <- map(chart_list, function(chart) {
  # Call the wrangleData function for each chart id
  wrangled_data <- wrangleData(figid = chart)
  return(wrangled_data)
})

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Create Viz                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calling the visualizer for each chart
prueba <- lapply(
  chart_list,
  callVisualizer
)

