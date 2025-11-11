## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            EU Thematic Report - Settings
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#devtools::install_github("ctoruno/WJPr")

library(pacman)
p_load(char = c(
  # Visualizations

  
  # Data Loading
  "haven", "writexl", "openxlsx", "janitor","WJPr",
  
  
  # Utilities
  "margins", "kableExtra", "glue", "sysfonts", "showtext", "ggtext",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(Sys.info()["user"] == "santiagopardo"){
  path2DA <- paste0("/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/Data Analytics/6. Country Reports/Mongolia-HR-Report-2025/data-viz")
  path2GPP <- paste0("/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/General Population Poll/GPP 2025/Merged Files/Historical Files/Merged.dta")
} else if (Sys.info()["user"] == "INSERT USER INFO"){
  path2DA <- paste0("INSERT USER PATH")
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Loading Fonts                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path2fonts<- file.path(
  path2DA, "6. Country Reports/0. Fonts", 
  fsep = "/"
)
font_add(family     = "Lato Full",
         regular    = file.path(path2fonts, "Lato-Regular.ttf", fsep = "/"),
         italic     = file.path(path2fonts, "Lato-LightItalic.ttf", fsep = "/"),
         bold       = file.path(path2fonts, "Lato-Bold.ttf", fsep = "/"),
         bolditalic = file.path(path2fonts, "Lato-BoldItalic.ttf", fsep = "/"))
font_add(family  = "Lato Light",
         regular = file.path(path2fonts, "Lato-Light.ttf", fsep = "/"))
font_add(family  = "Lato bold",
         regular = file.path(path2fonts, "Lato-Black.ttf", fsep = "/"))
font_add(family  = "Lato Black Italic",
         regular = file.path(path2fonts, "Lato-BlackItalic.ttf", fsep = "/"))
font_add(family  = "Lato Medium",
         regular = file.path(path2fonts, "Lato-Medium.ttf", fsep = "/"))
showtext_auto()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. General Functions                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1.  General functions -------------------------------------------------------------------------------------

resetDirs <- function(){
  prevOutputs <- list.files("outputs", include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
  if (length(prevOutputs)) file.remove(prevOutputs)
}

saveIT <- function(chart, figid, w, h) {
  ggsave(
    plot   = chart,
    file   = file.path(path2DA, paste0("outputs/", figid, ".svg"), fsep = "/"),
    width  = w, 
    height = h,
    units  = "mm",
    dpi    = 72,
    device = "svg"
  )
}

# Visualizador maestro: ahora recibe main_country y data_points/outline como argumentos
callVisualizer <- function(figid, outline, data_points, main_country){
  message(glue::glue("Working on chart {figid}"))
  
  type <- outline %>%
    dplyr::filter(id == figid) %>%
    dplyr::pull(type) %>%
    dplyr::first()
  
  data4chart <- data_points[[figid]]
  
  # Paletas por tipo
  # Radar: por año
  radar_colors <- c("2025" = "#2A2A94", "2020" = "#A90099", "2015" = "#efa700")
  # Slope: por métrica (mantengo tu convención original)
  slope_metric_colors <- c("4.8" = "#2A2A94", "6.5" = "#A90099")
  # Dumbbell: por país (resalta main_country, gris para otros); se genera según los datos
  if (!is.null(data4chart$Country)) {
    all_countries <- unique(data4chart$Country)
    dumbbell_colors <- setNames(rep("#B0B0B0", length(all_countries)), all_countries)
    if (main_country %in% names(dumbbell_colors)) dumbbell_colors[main_country] <- "#2A2A94"
  } else {
    dumbbell_colors <- colors4plot <- c(
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
  }
  
  # Render según tipo
  if (type == "dumbell"){
    
    chart <- genDumbell(data = data4chart)
    
  } else if (type == "radar"){
    latestYear <- "2025"
    chart <- wjp_radar(
      data      = data4chart,
      axis_var  = "Metric",
      target_var= "Value",
      color_var = "Year",
      maincat   = latestYear,
      label_var = "label_var",
      colors    = radar_colors,
      order_var = "order_var"
    )
  } else if (type == "bars"){
    chart <- genBars(
      data     = data4chart,
      target   = "Value",
      grouping = "Year",
      labels   = "label",
      colors   = "Year"
    )
  } else {
    stop(glue::glue("Tipo de figura no soportado: {type}"))
  }
  
  # Alturas personalizadas (conservo tu lógica)
  if (figid == "F4"){
    height <- 190
  } else if (figid %in% c("F6_A", "F6_B", "F3", "F5")){
    height <- 90
  } else{
    height <- 210
  }
  
  saveIT(chart = chart, figid = figid, w = 230, h = height)
  
  return(list(plot = chart, data = data4chart))
}
