#Author: Ziyi
#Note: This file takes about five to ten minutes to run.
#Only run it when we make a change on data imputation or significant national graph change

library(htmltools)
deps <- list("topojson.min.js", 
             htmlDependency(name = "d3-scale-chromatic",
                            version = "1.3.3",
                            src = list(href = "https://d3js.org/"),
                            script = "d3-scale-chromatic.v1.min.js")
)

ui <- fluidPage(
  # include css
  tags$head(includeCSS("~/data/AHRQ_Challenge/App_develop/custom_no_scroll.css")),
  tags$head(includeCSS("~/data/AHRQ_Challenge/Code_by_ziyi/git2/MortalityMinder/www/geoattr.css")),
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
    data_geo <- jsonlite::read_json("www/all-counties.json")
    usa_states <- jsonlite::read_json("www/usa-states.json")
    
    causes_of_death <- c("All Cause")
    for(cause in causes_of_death){
      data_stat <- cdc.mort.mat(cdc.data,"US",cause)
      if(cause == "Cardio"){
        data_stat <- cdc.mort.mat(cdc.data,"US",paste0(cause,"vascular"))
      }
      map <- r2d3(data = list(data_geo,data_to_json(data_stat),data_to_json(cause), usa_states),
                d3_version = 3,
                dependencies = "topojson.min.js",
                css = "www/geoattr.css",
                script = "www/d3_animation.js")
      interval <- c(0,1,2,3,4,5)
      for(i in interval){
        a <- i+1
        if(cause == "All Cause"){
          cause <- "All_Cause"
        }
        address <- paste0("www/National_image/",cause)
        address <- paste0(address, "/")
        address <- paste0(address,as.character(a))
        address <- paste0(address,".png")
      
        save_d3_png(map, address, width = 992,
                  height = 544, delay = i*2 + 0.5, zoom = 1)
      }
    }
    print("finished generating map")
  })
}
shinyApp(ui, server)