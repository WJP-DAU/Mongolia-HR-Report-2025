## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Country Report - Data Viz
##
## Author(s):         Santiago Pardo
##
## Dependencies:      World Justice Project
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Data Viz Functions ------------------------------------------------------------------------------------

genDumbell <- function(data) {
  # Data preparation
  data <- data %>%
    group_by(label_var) %>%
    mutate(order_value = ifelse(Country == MAIN_COUNTRY, values, NA)) %>%
    ungroup() %>%
    arrange(order_value) %>%
    mutate(label_var = factor(label_var, levels = unique(label_var)))
  
  # Add a background_fill column to alternate background colors
  data <- data %>%
    mutate(background_fill = as.factor(as.numeric(label_var) %% 2)) # Alternating values (0, 1)
  
  # Calculate mean values for each label_var
  mean_values <- data %>%
    group_by(label_var) %>%
    summarize(mean_value = mean(values, na.rm = TRUE)) %>%
    ungroup()
  
  # Join the mean values back to the main data
  data <- data %>%
    left_join(mean_values, by = "label_var")
  
  # Adjust the x position to move the "Region Average" label further to the right
  # and set y to align it vertically with the axis labels
  # Create the plot
  plot <- ggplot(data) +
    # Add alternating background colors using geom_tile
    geom_tile(aes(x = 0.5, y = reorder(label_var, -order_value), fill = background_fill),
              width = 1, height = 1, alpha = 0.1) +
    scale_fill_manual(values = c("0" = "grey90", "1" = "white"), guide = "none") +
    
    # Plot layers for lollipop segments
    geom_segment(aes(x = tapply(values, label_var, min)[label_var],
                     xend = values,
                     y = label_var,
                     yend = label_var),
                 color = "grey", size = 1) +
    
    # Points for each country with shapes and opacity for Thailand
    geom_point(aes(x = values,
                   y = reorder(label_var, -values),
                   shape = Country == MAIN_COUNTRY,
                   alpha = Country == MAIN_COUNTRY,
                   color = Country),
               size = 5) +
    
    # Display Thailand values as text labels
    geom_text(data = subset(data, Country == MAIN_COUNTRY),
              aes(x = values, y = reorder(label_var, -values),
                  label = sprintf("%.2f", values)),  # Use sprintf() for exactly two decimals
              hjust = 0.45, vjust = -2, size = 4,
              family = "Lato Bold", color = "#524F4C") +
    
    # Add mean value as a text ribbon on the right
    geom_text(data = mean_values,
              aes(x = 1.15, y = reorder(label_var, -mean_value),
                  label = sprintf("%.2f", mean_value)),  # Use sprintf() for exactly two decimals
              hjust = 0, vjust = 0.5, size = 4,
              family = "Lato bold", color = "black") +
    
    # Manually add vertical lines only up to the "1" position
    geom_vline(xintercept = c(0, 0.2, 0.4, 0.6, 0.8, 1),
               color = "#d1cfd1", size = 0.5, linetype = "dashed") +
    
    # Shape and color scales
    scale_shape_manual(values = c(18, 16)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_color_manual(values = colors4plot) +
    
    # Coordinate adjustments with "Region Average" added to x-axis labels
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.175),
                       labels = c("0", "0.2", "0.4", "0.6", "0.8", "1", "Regional \nAverage"),
                       limits = c(0, 1.3), position = "top") +
    coord_cartesian(clip = "off") +
    
    # Custom theme and layout
    WJP_theme() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),  # Remove default major grid lines
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_markdown(family = "Lato Medium",
                                         size = 3.5 * .pt,
                                         color = "Black", hjust = 0,
                                         lineheight = 1.3),
          plot.margin = margin(5, 5, 5, 0))
  
  # Print the plot
  plot
  
  return(plot)
}

# Barras (sin cambios lógicos, ya es genérica)
genBars <- function(
    data,               # Data frame
    target,             # Column name for y-axis values
    grouping,           # Column name for x-axis groups
    labels   = NULL,    # Optional column for text labels
    colors   = NULL,    # Column name to determine colors by group
    color_palette = c("2025" = "#2A2A94", "2020" = "#A90099", "2015" = "#efa700") # Colors to map for fill
) {
  
  # Rename columns in the data frame for simplicity
  data <- data %>%
    rename(
      target_var   = all_of(target),
      grouping_var = all_of(grouping)
    ) %>%
    mutate(grouping_var = factor(grouping_var, levels = c("2015", "2020", "2025")))
  
  
  # Optional label column renaming
  if (!is.null(labels) && labels %in% names(data)) {
    data <- data %>% rename(labels_var = all_of(labels))
  } else {
    data <- data %>% mutate(labels_var = "")
  }
  
  # # Optional colors column renaming or default assignment
  # if (!is.null(colors) && colors %in% names(data)) {
  #   data <- data %>% rename(colors_var = all_of(colors))
  # } else {
  #   data <- data %>% mutate(colors_var = "default")
  # }
  
  # Create the bar plot
  plt <- ggplot(data, aes(x = grouping_var, y = target_var, fill = grouping_var, label = labels_var)) +
    geom_bar(stat = "identity", width = 0.8, show.legend = FALSE) +
    scale_fill_manual(values = color_palette) +
    # Add labels with conditional color based on grouping_var
    geom_text(aes(y = target_var, color = grouping_var), vjust = -0.5, size = 4, family = "Lato Bold", show.legend = F) +
    scale_color_manual(values = color_palette) + # Color for text labels
    # Adjust Y-axis limits
    scale_y_continuous(limits = c(0, 1)) +
    
    # Adjust X-axis labels to have specific colors
    scale_x_discrete(labels = c("2025" = "2025", "2020" = "2020", "2015" = "2015")) +
    labs(y = NULL, x = NULL) +
    WJP_theme() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank()
    )
  
  return(plt)
}


wjp_radar <- function(
    data,             
    axis_var,         
    target_var,       
    label_var,        
    color_var,
    colors,   
    order_var,
    maincat
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(axis_var    = all_of(axis_var),
           target_var  = all_of(target_var),
           label_var   = all_of(label_var),
           color_var   = all_of(color_var),
           order_var   = all_of(order_var)) # %>%
  
  # Radar coordinates are computed for values between [0,1]
  # mutate(
  #   target_var = target_var / 100
  # )
  
  # Counting number of axis for the radar
  nvertix <- length(unique(data$axis_var))
  
  # Distance to the center of the web 
  central_distance <- 0.2
  
  # Function to generate radar coordinates
  circle_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
  }
  
  # Function to generate axis lines
  axis_coords <- function(n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }
  
  # Function to generate axis coordinates
  text_coords <- function(r      = 1.5, 
                          n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
  }
  
  # Y-Axis labels
  axis_measure <- tibble(
    r         = seq(0, 1, 0.2),
    parameter = rep(
      data %>% 
        filter(order_var == 1) %>% 
        ungroup() %>%
        distinct(axis_var) %>% 
        pull(axis_var),
      6
    )
  ) %>%
    bind_cols(
      map_df(
        seq(0, 1, 0.2) + central_distance, 
        text_coords
      ) %>% 
        distinct(r, .keep_all = T) %>% 
        select(-r)
    )
  
  # Generating data points
  rescaled_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble(r, fi) %>% 
      mutate(x = r*cos(fi), y = r*sin(fi)) %>% 
      select(-fi)
  }
  
  rescaled_data <- data %>% 
    bind_rows(
      data %>% 
        filter(axis_var == data %>% 
                 filter(order_var == 1) %>% 
                 distinct(axis_var) %>% 
                 pull(axis_var)) %>%
        mutate(axis_var  = "copy",
               order_var = nvertix)
    ) %>%
    group_by(color_var) %>%
    arrange(order_var) %>%
    mutate(
      coords = rescaled_coords(target_var + central_distance)
    ) %>%
    unnest(cols   = c(coords)) %>%
    mutate(across(x, 
                  ~.x*-1))
  
  # Generating ggplot
  radar <-
    
    # We set up the ggplot
    ggplot(
      data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords),
      aes(x = x,
          y = y)
    ) +
    
    # We draw the outter ring
    geom_polygon(
      data     = circle_coords(1 + central_distance),
      linetype = "dotted",
      color    = "#d1cfd1",
      fill     = NA
    ) +
    
    # We draw the inner rings
    geom_path(
      aes(group = r), 
      lty       = 2, 
      color     = "#d1cfd1"
    ) +
    
    # We draw the ZERO ring
    geom_polygon(
      data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords) %>%
        filter(r == 0.2),
      fill      = NA,
      linetype  = "solid",
      color     = "#d1cfd1"
    ) +
    
    # Then, we draw the Y-axis lines
    geom_line(
      data = axis_coords(), 
      aes(x     = x, 
          y     = y, 
          group = id),
      color = "#d1cfd1"
    ) +
    
    # Along with its labels
    geom_text(
      data = axis_measure, 
      aes(x     = x, 
          y     = y, 
          label = c("0","0.2","0.4","0.6","0.8","1")),
      family      = "Lato Full",
      fontface    = "plain",
      color = "#524F4C"
    ) +
    
    # Then, we add the axis labels
    geom_richtext(
      data  = text_coords() %>%
        mutate(
          n = row_number(),
          across(x,
                 ~.x*-1),
          across(c(x,y),
                 ~if_else(n == 2, .x*1.125, .x)) # We need to adjust by the long text in axis number 8
        ),
      aes(x = x, 
          y = y), 
      label = data %>% 
        arrange(order_var) %>% 
        filter(color_var == maincat) %>% 
        pull(label_var),
      family      = "Lato Full",
      fontface    = "plain",
      fill        = NA, 
      label.color = NA
    ) +
    
    # We add the data points along with its lines
    geom_point(
      data = rescaled_data, 
      aes(x     = x, 
          y     = y, 
          group = color_var, 
          color = as.factor(color_var)), 
      size      = 3
    ) +
    geom_path(
      data = rescaled_data, 
      aes(x     = x, 
          y     = y, 
          group = color_var, 
          color = as.factor(color_var)), 
      size = 1
    ) +
    
    # Remaining aesthetics
    coord_cartesian(clip = "off") + 
    scale_x_continuous(expand = expansion(mult = 0.125)) + 
    scale_y_continuous(expand = expansion(mult = 0.10)) + 
    scale_color_manual(values = colors) +
    theme_void() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      legend.position    = "none"
    )
  
  return(radar)
  
}