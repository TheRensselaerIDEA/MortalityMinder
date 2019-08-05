init <- function() {
  setwd("~/data/AHRQ_Challenge/MortalityMinder/beta.02/init")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.02/init/Librarian.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.02/init/Loader_CHR2019.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.01/init/Loader_CDC.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.01/init/Loader_GEO.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.01/init/Analyzer_PCA.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.01/init/Analyzer_Kmeans.R")
  source("~/data/AHRQ_Challenge/MortalityMinder/beta.01/init/Analyzer_Correlation.R")
}

init()
ui <- fluidPage(
  # include css
  tags$head(includeCSS("~/data/AHRQ_Challenge/MortalityMinder/beta.02/www/custom.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  # navbar
  tags$div(
    class = "navbar",
    tags$div(
      class = "logo",
      tags$img(src="RPI_logo2.png", alt="IDEA",style="width:100%;height:100%;")
    ),
    tags$div(
      calss = "tx",
      tags$a(href="#frontpage","Overview",style="color:#ffffff;"),
      tags$a(href="#page1","Fact", style="color:#ffffff;")
    )
  ),
  
  tags$div(
    class = "main",
    # frontpage
      tags$div(
      id = "frontpage",
      #block1 of frontpage
      tags$div(
        class = "block1",
        #first button block
        tags$div(
          class="buttonblock1"
          
        ),
        #image block of block1
        tags$div(
          class = "img",
          tags$img(src="000010.png",id = "mortality", style="width:100%;height:100%;")
          
        ),
        #second button block
        tags$div(
          class = "buttonblock2"
        )
        ),
      tags$h1("YOUR LIFE MAY BE IN DANGER"),
      tags$button(
                  class = "button",
                  tags$span("Explore Detail",onclick = "window.location.href = 
                  'https://lp01.idea.rpi.edu/shiny/lis17/beta.01/';")
                  )
    ),
    tags$div(
      id = "page1",
      # page1 start
      tags$div(
        class = "plot1",
        plotOutput("geo_cluster_kmean",width="100%",height="100%")
      ),
      tags$div(
        class = "plot2",
        plotOutput("mort_line",height = "100%", width = "100%")
      ),
      tags$div(
        class = "plot3",
        plotOutput("urban_dist_cluster", height = "100%", width = "100%")
      ),
      tags$div(
        class = "plot4",
        plotOutput("sd_boxplot",height = "100%", width = "100%")
      )
      
      # end of page1 
    )
    # main end
  ),
  
  includeScript(path = "~/data/AHRQ_Challenge/MortalityMinder/beta.02/inst/myscript.js")
)








server <- function(input, output, session){
  observeEvent(input$explore,{
    onclick("explore", runjs("window.open('https://lp01.idea.rpi.edu/shiny/lis17/beta.01/')"))
  })
  
  
  # Call Onclick
}


shinyApp(ui = ui, server = server)