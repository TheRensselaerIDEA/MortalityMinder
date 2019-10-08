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
  tags$head(includeCSS("~/data/AHRQ_Challenge/Code_by_ziyi/git/MortalityMinder/www/geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  tags$title("D3 NY"),
  tags$div(
    id = "wrapper",
    tags$button(
      id = "play",
      "play"
    ),
    tags$span(
      id = "clock",
      "year"
    )
  ),
  tags$div(
    class = "explore_but",
    tags$button(
      id = "btn",
      "Explore more",
      onclick="window.location.href=' https://shengjin-li.shinyapps.io/mm_online_version_code/'"
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
    data_stat <- cdc.mort.mat(cdc.data,"US")
    r2d3(data = list(data_geo,data_to_json(data_stat)),
         d3_version = 3,
         dependencies = "topojson.min.js",
         css = "www/geoattr.css",
         script = "www/d3.js")
  })
}
shinyApp(ui, server)