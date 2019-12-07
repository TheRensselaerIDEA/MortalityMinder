# Author: Karan Bhanot
# Adapted from "national_map_generator.R and Theme.R
# Create geo plot images for USA

library(htmltools)
library(webshot)

deps <- list("topojson.min.js", 
             htmlDependency(name = "d3-scale-chromatic",
                            version = "1.3.3",
                            src = list(href = "https://d3js.org/"),
                            script = "d3-scale-chromatic.v1.min.js")
)

ui <- fluidPage(
  # include css
  tags$head(includeCSS("../www/custom_no_scroll.css")),
  tags$head(includeCSS("../www/geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  tags$title("D3 NY"),
  tags$div(
    class = "title",
    tags$b("National Death of Despair")
  ),
  tags$div(
    class = "explore_but",
    tags$button(
      id = "play",
      "See changes"
    ),
    tags$span(
      id = "clock",
      "2000-2002"
    ),
    tags$button(
      class = "btn",
      "Explore more",
      onclick="window.location.href=' https://shengjin-li.shinyapps.io/beta/'"
    ) 
  ),
  d3Output("d3", width = '100%', height = '100%'),
  tags$body(),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ScrollMagic/2.0.7/ScrollMagic.min.js"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ScrollMagic/2.0.7/plugins/debug.addIndicators.min.js"),
  tags$script(src = "jquery-ui.min.js")
)

data_to_json <- function(data) {
  jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)
}

server <- function(input, output) {
  output$d3 <- renderD3({
    data_geo <- jsonlite::read_json("../www/all-counties.json")
    usa_states <- jsonlite::read_json("../www/usa-states.json")
    
    causes_of_death <- c("Despair", "Assault", "Cancer", "Cardio", "All Cause")
    
    for(cause in causes_of_death){
      print(paste("Starting", cause))
      
      if (cause == "Cardio") {
        cause = paste(cause, "vascular", sep = "")
      }
      
      mort.cluster.raw <- cluster.counties(cdc.mort.mat(cdc.data, "US", cause), cluster.method="kmeans", cluster.num=6)
      mort.cluster <- order.county.clusters(mort.cluster.raw,
                                            get.cluster.order.map(get.cluster.deathrate.during.time(mort.cluster.raw,
                                                                                                    cdc.data,
                                                                                                    death.cause=cause
                                            ), time.period = "2015-2017"))

      dataset <- geo.map.fetch("US", mort.cluster) %>% dplyr::select(county_fips, cluster) %>% dplyr::arrange(county_fips)

      map <- r2d3(data = list(data_geo,data_to_json(dataset),data_to_json(cause), usa_states),
                  d3_version = 3,
                  dependencies = "../www/topojson.min.js",
                  css = "../www/geoattr.css",
                  script = "../www/d3_geo_animation.js")
      
      address <- paste0("../www/National_geo_image/",cause)
      address <- paste0(address,".png")
      
      print(paste("Saving", cause))
      
      save_d3_png(map, address, width = 992,
                  height = 544, delay = 1, zoom = 1)
      
      print(paste("End", cause))
    }
    print("finished generating map")
  })
}
shinyApp(ui, server)