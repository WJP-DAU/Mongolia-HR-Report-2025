thailand_bars <- function(
    data,              
    target,        
    grouping,      
    labels     = NULL,        
    colors,        
    cvec       = NULL,            
    direction  = "vertical",         
    stacked    = F,       
    lab_pos    = NULL,    
    expand     = F,      
    order      = NULL,
    width      = 0.9,
    ptheme     = WJP_theme()
){
  
  # Renaming variables in the data frame to match the function naming
  if (is.null(labels)) {
    data <- data %>%
      dplyr::mutate(labels_var    = "") %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping),
                    colors_var    = all_of(colors),
                    lab_pos       = all_of(lab_pos),
                    order_var     = all_of(order))
  } else {
    
    data <- data %>%
      dplyr::rename(target_var    = all_of(target),
                    grouping_var  = all_of(grouping),
                    labels_var    = all_of(labels),
                    colors_var    = all_of(colors),
                    order_var     = all_of(order))
    
    if (is.null(lab_pos)){
      data <- data %>%
        dplyr::mutate(lab_pos = target_var)
    } else {
      data <- data %>%
        dplyr::rename(lab_pos = all_of(lab_pos))
    }
    
  }
  if (grouping == colors) {
    data <- data %>%
      dplyr::mutate(grouping_var = colors_var)
  }
  
  # Creating plot
  if(is.null(order)) {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F, width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold")
    } else {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold")
    }
    
  } else {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F,  width = width) +
        geom_text(aes(y    = target_var + lab_pos),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold")
    } else {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold")
    }
  }
  
  plt <- plt +
    labs(y = "% of respondents")
  
  if (!is.null(cvec)) {
    plt <- plt +
      scale_fill_manual(values = cvec)
  }
  
  if (expand == F) {
    uplimitV = 100
    uplimitH = 105
  } else {
    uplimitV = 110
    uplimitH = 105
  }
  
  if (direction == "vertical") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitV),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      ptheme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D0D1D3"),
            axis.title.x       = element_blank())
  }
  
  if (direction == "horizontal") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitH),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "right") +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      ptheme +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#D0D1D3"),
            axis.title.y       = element_blank(),
            axis.title.x       = element_blank(),
            axis.text.y        = element_text(hjust = 0))
  }
  
  return(plt)
  
}