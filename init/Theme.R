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
      ),
      panel.grid.major.x = element_blank() 
    )
}

color.line.cluster <- function(state.choice, n.clusters) {
  
  if (state.choice != "US"){
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59")
        
      )(n.clusters),
      guide = guide_legend(reverse = T)
    )
    
  } else {
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
        
      )(n.clusters),
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
      c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
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

geoTitle <- function(state.choice, death.cause) {
  return (tags$div(
    HTML(paste(state.choice, "Trend Cluster<br/>Geo Distribution", sep = " "))))
}

# draw.geo.cluster: Used in app.R to draw state and US maps
draw.geo.cluster <- function(state.choice, death.cause, mort.cluster) {
  
  dataset <- geo.map.fetch(state.choice, mort.cluster) %>% 
    dplyr::rename(VAR_ = cluster)
  lat_long <- getLatLong(state.choice, dataset)
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  colors <- c("#fef0d9","#fdcc8a","#fc8d59")
  labels <- c("Low", "Medium", "High")
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  dataset$county_fips <- substr(dataset$county_fips, 3, 5)
  dataset <- left_join(as.data.frame(shapes)['COUNTYFP'], dataset, by = c("COUNTYFP" = "county_fips"))
  
  if (state.choice != "US"){
    return (leaflet(shapes, 
                    options = leafletOptions(dragging = FALSE)) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 1,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause), 
                         position = "topleft", 
                         className="map-title") %>%
              addLegend("bottomleft",
                        colors = colors[3],
                        labels = labels[3],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[2],
                        labels = labels[2],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[1],
                        labels = labels[1],
                        title = "Clusters:",
                        opacity = 1))
  }else{
    return (leaflet(shapes, 
                    options = leafletOptions(dragging = FALSE)) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 0,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause), 
                         position = "topleft", 
                         className="map-title") %>%
              addLegend("bottomleft",
                        colors = colors[3],
                        labels = labels[3],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[2],
                        labels = labels[2],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[1],
                        labels = labels[1],
                        title = "Clusters:",
                        opacity = 1))
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
      #ggplot(aes(long, lat, group = group, fill = VAR_, color = VAR_, text = county_name)) +
      
      ggplot(VAR_, aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = group, color = group))+
      
      #geom_polygon(size = 0, color = "white",alpha = 0.9) +
      #base.geo() + 
      labs.geo.mort(state.choice, period.choice, death.cause) + 
      color.geo.mort(death.cause) +
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  }
  
}

title <- function(state.choice, death.cause, period) {
  return (tags$div(
    HTML(paste(state.choice, " - Death of", death.cause, "Rate<br/>", period))))
}

getLatLong <- function(state.choice, dataset) {
  
  max.long <- max(dataset$long)
  max.lat <- max(dataset$lat)
  min.long <- min(dataset$long)
  min.lat <- min(dataset$lat)
  
  switch(state.choice,
         'AK'= {
           min.lat = 50.0
           max.lat = 76.0
           min.long = -164.0
           max.long = -120.0
         },
         "HI"= {
           min.lat = 18.46
           max.lat = 22.46
           min.long = -160.505
           max.long = -154.505
         },
         "MN"= {
           min.lat = 43.0
           max.lat = 49.9
           min.long = -98.3655146
           max.long = -89.3655146
         },
         "ID"= {
           min.lat = 40.4945756
           max.lat = 50.4945756
           min.long = -110.1424303
           max.long = -118.1424303
         },
         "LA" = {
           min.lat = 28.9733766
           max.lat = 33.9733766
           min.long = -96.4299097
           max.long = -87.4299097
         },
         "MI" = {
           min.lat = 40.9435598
           max.lat = 48.9435598
           min.long = -90.4158049
           max.long = -82.4158049
         },
         "NV" = {
           min.lat = 34.502032
           max.lat = 42.502032
           min.long = -120.0230604
           max.long = -114.0230604
         }, 
         "OH" = {
           min.lat = 37.1903624
           max.lat = 43.1903624
           min.long = -86.6692525
           max.long = -79.6692525
         },
         "WV" = {
           min.lat = 36.9201705
           max.lat = 40.9201705
           min.long = -83.1816905
           max.long = -77.1816905
         })
  
  if (state.choice == 'AR' | state.choice == 'MN') {
    padding = 0.5
  } else {
    padding = 0.05
  }
  
  return(c(min.lat - padding, min.long - padding, max.lat + padding, max.long + padding))
}

geo.plot <- function(state.choice, death.cause, mort.data, period) {
  dataset <- geo.map.fetch(state.choice, mort.data) %>% 
    dplyr::rename(VAR_ = death_rate)
  lat_long <- getLatLong(state.choice, dataset)
  
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  colors <- c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000", "#000000")
  labels <- c("[0,5]", "[5,10]", "[10,15]", "[15,25]", "[25,50]", "[50,100]", "[100,200]", "[200,Inf]")
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  dataset$county_fips <- substr(dataset$county_fips, 3, 5)
  dataset <- left_join(as.data.frame(shapes)['COUNTYFP'], dataset, by = c("COUNTYFP" = "county_fips"))
  
  if (state.choice != "US"){
    return (leaflet(shapes, 
                    options = leafletOptions(
                      dragging = FALSE)) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 1,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(title(state.choice, death.cause, period), 
                         position = "topleft", 
                         className="map-title") %>%
              addLegend("bottomleft",
                        colors = colors[8],
                        labels = labels[8],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[7],
                        labels = labels[7],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[6],
                        labels = labels[6],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[5],
                        labels = labels[5],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[4],
                        labels = labels[4],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[3],
                        labels = labels[3],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[2],
                        labels = labels[2],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[1],
                        labels = labels[1],
                        title = "Rate;",
                        opacity = 1))
  }else{
    return (leaflet(shapes, 
                    options = leafletOptions(dragging = FALSE)) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 0,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(title(state.choice, death.cause, period), 
                         position = "topleft", 
                         className="map-title") %>%
              addLegend("bottomleft",
                        colors = colors[8],
                        labels = labels[8],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[7],
                        labels = labels[7],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[6],
                        labels = labels[6],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[5],
                        labels = labels[5],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[4],
                        labels = labels[4],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[3],
                        labels = labels[3],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[2],
                        labels = labels[2],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[1],
                        labels = labels[1],
                        title = "Rate;",
                        opacity = 1))
  }
  
  
}
