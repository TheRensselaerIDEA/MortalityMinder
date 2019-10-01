theme.text <- function() {
  theme(
    text = element_text(family = "Bookman"),
    title = element_text(color = "gray25"),
    plot.caption = element_text(
      color = "gray30",
      size = 7.5
    ),
    plot.subtitle = element_text(size = 9)
  )
}

theme.background <- function() {
  theme(
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )
}

base.geo <- function(data) {
  ggplot(data, aes(long, lat, group = group, fill = VAR_)) +
    geom_polygon(size = 0.3, color = "white", alpha = 0.9)
}

theme.line.mort <- function() {
  theme_minimal() + 
    theme.text() + 
    theme.background() +
    theme(
      axis.text.x = element_text(
        angle = 20
      )
    )
}

color.line.cluster <- function(state.choice) {
  
  if (state.choice != "US"){
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
        
      )(4),
      guide = guide_legend(reverse = T)
    )
    
  } else {
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
        
      )(7),
      guide = guide_legend(reverse = T)
    )
  }
  
}

labs.line.mort <- function(state.choice, death.cause) {
  labs(
    title = paste(
      state.choice, 
      paste("Death of", death.cause, "Trend"), 
      sep = ' - '
    ),
    caption = "Data Sorce: CDCWONDER Multi-Cause of Death",
    x = "Period", y = "Mortality Rate (# per 100k)"
  )
}

theme.geo.cluster <- function() {
  theme_void() +
    theme(
      text = element_text(color = "#22211d"), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      
      plot.title = element_text(
        size = 16, hjust = 0.01, 
        margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
      ),
      
      legend.position = c(0.3, 0.02),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    ) + 
    theme.text() + 
    theme.background()
}

color.geo.cluster <- function(n) {
  scale_fill_manual(
    name = "Cluster",
    #c("#ED3F3C0F","#00ACAC0F","#203F9A0F","#F8A51A0F")
    #c("#ABB7B7FF","#BDC3C7FF","#D2D7D3FF","#ECF0F1FF")
    #c("#E9E9E9FF","#D4D4D4FF","#9A9A9AFF","#707070FF")
    #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
    #c("#c6c6c6","#9e9e9e","#787878","#565656")
    values = colorRampPalette(
      c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
    )(n),
    guide = guide_legend(
      keyheight = unit(2, units = "mm"), 
      keywidth = unit(8, units = "mm"), 
      label.position = "bottom", 
      title.position = "top", 
      nrow = 1
    )
  )
}


 
labs.geo.cluster <- function(state.choice) {
  labs(
    title = paste(state.choice, "Trend Cluster\nGeo-Distribution"),
    x = NULL, y = NULL
  )
}

# draw.geo.cluster: Used in app.R to draw state and US maps
draw.geo.cluster <- function(state.choice, mort.cluster) {
  n <- length(unique(pull(mort.cluster, "cluster")))
  ## This saves a df for plot experimentation
  # if (state.choice != "US"){
  #   # geo.match.fetch: defined in GEO_Lib.R
  #   myDf <- geo.map.fetch(state.choice, mort.cluster) %>%
  #     dplyr::rename(VAR_ = cluster)
  #   write_rds(myDf, "myDf.rds")
  # }
  if (state.choice != "US"){
    # geo.match.fetch: defined in GEO_Lib.R
    geo.map.fetch(state.choice, mort.cluster) %>%
      dplyr::rename(VAR_ = cluster) %>%
      ggplot(aes(long, lat, group = group, fill = VAR_, color = VAR_)) +
      geom_polygon(size = 0, color = "white",alpha = 0.9) +
      #base.geo() +
      labs.geo.cluster(state.choice) + 
      color.geo.cluster(n) + 
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  } else {
    # geo.match.fetch: defined in GEO_Lib.R
    geo.map.fetch("US", mort.cluster) %>%
      dplyr::rename(VAR_ = cluster) %>%
      ggplot(aes(long, lat, group = group, fill = VAR_, color = VAR_)) +
      geom_polygon(size = 0, color = "white",alpha = 0.9) +
      #base.geo() +
      labs.geo.cluster(state.choice) + 
      color.geo.cluster(n) + 
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  }
   
}

theme.geo.mort <- function() {
  theme_void() +
    theme(
      text = element_text(color = "#22211d"), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      
      plot.title = element_text(
        size = 16, hjust = 0.01, 
        margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
      ),
      
      legend.position = c(0.32, 0.02),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    ) + 
    theme.text() + 
    theme.background()
}

color.geo.mort <- function(death.cause) {
  
  bins <- bin.geo.mort(death.cause)
  
  #c("#fff7ec","#fee8c8","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#b30000","#7f0000")
  # original
  #c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000")
  vals <- colorRampPalette(
    c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000")
  )(length(bins))
  
  names(vals) <- cut(seq(0, 600, 5), bins) %>% 
    unique()
  
  scale_fill_manual(
    name = "Rate", 
    values = vals,
    guide = guide_legend(
      keyheight = unit(2, units = "mm"), 
      keywidth = unit(4, units = "mm"), 
      label.position = "bottom", 
      title.position = "top", 
      nrow = 1
    )
  )
}

labs.geo.mort <- function(state.choice, period.choice, death.cause) {
  labs(
    title = paste(
      paste(state.choice, paste("- Death of", death.cause)), 
      paste("Rate", period.choice, sep = '\n')
    ),
    # subtitle = period.choice,
    # caption = "Data Sorce:\n\t1.CDCWONDER Multi-Cause of Death",
    x = NULL, y = NULL
  )
}

bin.geo.mort <- function(death.cause) {
  bin <- list(
    "Despair" = c(0, 5, 10, 15, 25, 50, 100, 200, Inf),
    "Cardiovascular" = c(seq(0, 360, 45), Inf),
    "Assault" = c(seq(0, 40, 5), Inf),
    "Cancer" = c(seq(0, 360, 45), Inf)
  )
  return(bin[[death.cause]])
}

draw.geo.mort <- function(state.choice, period.choice, mort.data, death.cause) {
  if (state.choice != "US"){
    geo.map.fetch(state.choice, mort.data) %>% 
      dplyr::rename(VAR_ = death_rate) %>%
      base.geo() + 
      labs.geo.mort(state.choice, period.choice, death.cause) + 
      color.geo.mort(death.cause) + 
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
    
  }else{
    geo.map.fetch("US", mort.data) %>% 
      dplyr::rename(VAR_ = death_rate) %>%
      base.geo() + 
      labs.geo.mort(state.choice, period.choice, death.cause) + 
      color.geo.mort(death.cause) + 
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  }
  
}
