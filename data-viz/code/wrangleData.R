## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Country Report - Wrangle Data
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Wrangling function ------------------------------------------------------------------------------------

wrangleData <- function(figid, outline, master_data, main_country, comparison_countries) {
  # Etiquetas
  metric_labels <- c(
    "1.5" = "Non-Governmental Checks<br> of Government Power",
    "3.3" = "Civic Participation",
    "3.4" = "Access to Complaint<br> Mechanisms",
    "3.2" = "Right to <br> Information",
    "4.3" = "Due Process of<br>the Law and Rights<br> of the Accused",
    "7.4" = "Improper<br> Government Influence<br> in Civil Justice",
    "8.6" = "Improper<br> Government Influence<br> in Criminal Justice",
    "7.1" = "Access to <br> and Affordability<br> of Civil Justice",
    "8.4" = "Impartiality in <br> Criminal Justice",
    "4.1" = "Equality<br> and Discrimination",
    "7.2" = "Discrimination<br> in Civil Justice",
    "4.8" = "Labor Rights",
    "6.4" = "Due Process is <br> Respected in<br>Administrative Proceedings",
    "4.2" = "The Right to <br>Life and Security of<br>the Person is Effectively<br> Guaranteed",
    "6.3" = "Administrative <br>Proceedings are Conducted <br>without Unreasonable Delay",
    "4.7" = "Freedom of <br> Assembly and Association",
    "4.4" = "Freedom of <br> Expression and <br> Opinion",
    "4.5" = "Freedom of <br> Belief and Religion",
    "4.6" = "Freedom from <br> Interference with <br> Privacy",
    "6.5" = "Protection from <br>Expropriation without <br>Due Process"
  )
  dumbell_wrapped_metric_labels <- sapply(metric_labels, function(x) stringr::str_wrap(x, width = 30))
  
  # Chart config
  type <- outline %>% dplyr::filter(id == figid) %>% dplyr::pull(type) %>% dplyr::first()
  variables <- outline %>%
    dplyr::filter(id == figid) %>%
    dplyr::select(!c("id", "section", "type")) %>%
    dplyr::select(where(~ !all(is.na(.)))) %>%
    as.list() %>%
    unlist() %>%
    as.character()
  
  # Wrangle por tipo
  if (type == "dumbell") {
    # 2025, todos los países (main + comparisons)
    data2plot <- master_data %>%
      dplyr::filter(Year == 2025, Country %in% c(main_country, comparison_countries)) %>%
      dplyr::select(Country, Year, dplyr::all_of(variables)) %>%
      tidyr::pivot_longer(cols = !c(Country, Year), names_to = "Metric", values_to = "values") %>%
      dplyr::mutate(
        values    = as.numeric(values),
        label_var = dplyr::recode(Metric, !!!dumbell_wrapped_metric_labels)
      ) %>%
      dplyr::group_by(Metric) %>%
      dplyr::mutate(mean_value = mean(values, na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else if (type == "radar") {
    # 2015, 2020, 2025 solo main_country
    data2join <- master_data %>%
      dplyr::filter(Year %in% c(2015, 2020, 2025), Country == main_country) %>%
      dplyr::select(Year, dplyr::all_of(variables)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(variables), names_to = "Metric", values_to = "Value") %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      tidyr::pivot_wider(names_from = Year, values_from = Value) %>%
      dplyr::rename(Value_2015 = `2015`, Value_2020 = `2020`, Value_2025 = `2025`) %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("Value"), names_to = "Year", values_to = "Value", names_prefix = "Value_") %>%
      dplyr::mutate(
        label_var  = dplyr::recode(Metric, !!!metric_labels),
        Metric     = as.factor(Metric),
        figure     = dplyr::if_else(Year == "2025", paste0(round(Value, 2)), NA_character_),
        latestYear = "2025"
      ) %>%
      dplyr::arrange(Metric, Year, Value) %>%
      dplyr::group_by(Year) %>%
      dplyr::arrange(dplyr::desc(Value)) %>%
      dplyr::mutate(order_var = dplyr::if_else(Year == "2025", dplyr::row_number(), as.numeric(NA))) %>%
      dplyr::ungroup()
    
    figure2.df <- data2join %>%
      dplyr::mutate(figure2 = dplyr::if_else(Year == "2020", paste0(round(Value, 2)), NA_character_)) %>%
      tidyr::drop_na(figure2) %>%
      dplyr::select(Metric, figure2)
    
    figure3.df <- data2join %>%
      dplyr::mutate(figure3 = dplyr::if_else(Year == "2015", paste0(round(Value, 2)), NA_character_)) %>%
      tidyr::drop_na(figure3) %>%
      dplyr::select(Metric, figure3)
    
    order_value <- data2join %>%
      tidyr::drop_na(order_var) %>%
      dplyr::select(Metric, order_var)
    
    data2plot <- data2join %>%
      dplyr::left_join(figure2.df, by = "Metric") %>%
      dplyr::left_join(figure3.df, by = "Metric") %>%
      dplyr::left_join(order_value, by = "Metric") %>%
      dplyr::mutate(
        category = label_var,
        across(label_var, ~paste0(
          "<span style='color:#2a2a9A;font-size:3.514598mm;font-weight:bold'>", figure, "</span>",
          "<span> | </span>",
          "<span style='color:#a90099;font-size:3.514598mm;font-weight:bold'>", figure2, "</span>",
          "<span> | </span>",
          "<span style='color:#efa700;font-size:3.514598mm;font-weight:bold'>", figure3, "</span><br>",
          "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>", label_var, "</span>"
        )),
        figure2   = dplyr::if_else(Year == "2020", NA_character_, figure2),
        label_var = dplyr::if_else(Year == "2020", NA_character_, label_var)
      ) %>%
      dplyr::arrange(dplyr::desc(as.numeric(Year))) %>%
      dplyr::select(-order_var.x) %>%
      dplyr::rename(order_var = order_var.y)
    
  } else if (type %in% c("slope", "bars")) {
    # 2015, 2020, 2025 solo main_country
    data2plot <- master_data %>%
      dplyr::filter(Year %in% c(2015, 2020, 2025), Country == main_country) %>%
      dplyr::select(Year, dplyr::all_of(variables)) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(variables), names_to = "Metric", values_to = "Value") %>%
      dplyr::mutate(label = paste0(round(Value, 2))) %>%
      dplyr::arrange(Year)
  } else {
    stop(glue::glue("Tipo de figura no soportado en wrangleData: {type}"))
  }
  
  return(data2plot)
}
