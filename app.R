# Date: 2019/8/1
# Author: 
#   UI: Shengjin Li
#   Server: Yuxuan Wang
#   Graph: Ziyi Wang

# This version uses reactlog
# library(reactlog)
# options(shiny.reactlog = TRUE)

source("Source.R")
deps <- list("topojson.min.js", 
             htmlDependency(name = "d3-scale-chromatic",
                            version = "1.3.3",
                            src = list(href = "https://d3js.org/"),
                            script = "d3-scale-chromatic.v1.min.js")
)
#-----------------
state.list <- state.abb
names(state.list) <- state.name
state.list <- append(state.list, "United States", after = 0)

cause.list <- c("Deaths of Despair"="Despair","Cancer Deaths"="Cancer","Deaths by Assault"="Assault","Cardiovascular Disease"="Cardiovascular", "All Cause" = "All Cause")
cause.definitions <- c("Deaths of Despair are deaths due to suicide, overdose, substance abuse and poisonings"="Despair",
                       "Deaths due to Assault are deaths caused by injuries inflicted by another person with intent to injure or kill, by any means"="Assault",
                       "Cardiovascular Disease are deaths due to diseases of the circulatory systems such as heart disease and stroke"="Cardiovascular",
                       "Deaths due to Cancer deaths due to cancer and neoplasm"="Cancer")


n.clusters.state = 3
n.clusters.nation = 6
jscode <- "shinyjs.nextpage = function(){$('.fp-next').click();}"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  # include css
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  # navbar
  tags$div(
    class = "navbar",
    tags$div(
      class = "title",
      h1("MortalityMinder")
    ),
    
    tags$div(
      class = "prompt_text",
      "Select cause of death and state: "
    ),
    
    pickerInput(
      inputId = "state_choice",
      label = h4("State"), 
      choices = state.list,
      selected = "CA",
      width = "200px",
      options = list(
        `live-search` = TRUE,
        "dropup-auto" = FALSE
      )
    ),
    pickerInput(
      inputId = "death_cause",
      label = h4("Cause of Death"),
      choices = cause.list,
      width = "200px",
      choicesOpt = list(
        subtext = c("Self-Harm and some other causes"),
        "dropup-auto" = FALSE
      )
    )
    
    
  ),
  
  tags$div(
    id = "fullpage",
    tags$div(
      class = "section s1",
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        tags$div(
          class = "page2",
          uiOutput("national_map"),
          tags$div(
            class = "page2_col1",
            style = "height: auto;",
            tags$h3("Since 2010, Mortality Rates in the United States have stopped decreasing and are even increasing.",
                    style = "margin: 10px 10% 1px 15%; padding: 5px 5px;"),
            tags$p("MortalityMinder analyzes trends of premature death in the United States which are caused by:"),
            tags$ul(
              style = "font-weight: bold;",
              tags$li("Deaths of Despair",
                      style = "font-weight: inherit;"),
              tags$li("Cardiovascular Disease",
                      style = "font-weight: inherit;"),
              tags$li("Cancer",
                      style = "font-weight: inherit;"),
              tags$li("Assault Deaths",
                      style = "font-weight: inherit;"),
              tags$li("All Causes",
                      style = "font-weight: inherit;")
            ),
            tags$p("The mortality rate is the number of people age 25 to 64 per 100,000 that died prematurely in a given county during a three year period fora given cause and for a region:  county, state or nationwide."),
            
            tags$p("Pick the cause of death on the menu bar to see how mortality rates inthe United States have changed from 2000 to 2017."),
            
            tags$p("To understand why rates are changing, MortalityMinder analyzes factors that are related with increased mortality rates at the county level."),
            tags$p(tags$i("Click right and left to investigate more."))          
          ),
          tags$div(
            class = "vl"
          ),
          tags$div(
            class = "page2_col2",
            tags$div(
              class = "page2_col2_top",
              tags$div(
                class = "National_title",
                style = "padding-right: 20px; padding-left: 20px",
                uiOutput("textNationalTitle")
              ),
              tags$div(
                class = "explore_but",
                style = "padding-right: 20px; padding-left: 20px; text-align: center;",
                # tags$button(
                #   id = "play",
                #   "Stop"
                # ),
                tags$ul(
                  class = "ul_period",
                  tags$button(
                    id = "first_period",
                    class = "period_text",
                    "2000-2002"
                  ),
                  tags$button(
                    id = "second_period",
                    class = "period_text",
                    "2003-2005"
                  ),
                  tags$button(
                    id = "third_period",
                    class = "period_text",
                    "2006-2008"
                  ),
                  tags$button(
                    id = "forth_period",
                    class = "period_text",
                    "2009-2011"
                  ),
                  tags$button(
                    id = "fifth_period",
                    class = "period_text",
                    "2012-2014"
                  ),
                  tags$button(
                    id = "sixth_period",
                    class = "period_text",
                    "2015-2017"
                  ))
                
              )),
            tags$hr(),
            tags$div(
              class = "page2_col2_middle",
              style = "padding-right: 20px; padding-left: 20px",
              tags$img(
                id = "national_map_new",
                class = "landing_page_map",
                src = "Despair/1.png",
                #  style = "width:70%"
                style = "height:80%"
              )
            ),
            tags$div(
              class = "page2_col2_bottom",
              style = "padding-right: 20px; padding-left: 20px",
              uiOutput("textMortFactsTitle"),
              uiOutput("textMortFacts"),
              uiOutput("textMortFactsClosing")
            )
          )
        )
      ),
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        tags$div(
          class = "page1",
          tags$div(
            class = "col1",
            tags$div(
              class = "col1_top",
              tags$div(
                class = "col1_top_left",
                style = "padding-right: 20px; padding-left: 20px",
                tags$div(
                  title="The mortality rate used in the app is the number
                  of people per 100,000 that died prematurely in a given 
                  county during a three year period. A premature death is 
                  considered anyone that dies between the ages of 25 to 64 
                  as a result of the selected cause.",
                  tags$h3("Exploring Causes of Premature Death",  icon("info-circle"))
                ),
                uiOutput("textDescription")
              ),
              tags$div(
                tags$style(HTML(
                  "
                  #year_selector {
                  width: 100%;
                  text-align: center;
                  }
                  #year_selector .control-label {
                  width: 100%;
                  text-align: left;
                  font-size: 12px;
                  display: block;
                  }
                  .radio-inline {
                  padding: 0;
                  margin: 0 2px;
                  }
                  .radio-inline+.radio-inline {
                  margin: 0;
                  }
                  .radio-inline input[type=radio] {
                  display: none;
                  }
                  .radio-inline input[type=radio]:checked + span {
                  padding: 0 2px;
                  border: 2px solid black;
                  border-radius: 3px;
                  }
                  @media screen and (min-width : 1601px)
                  {
                  .radio-inline {
                  font-size: 16px;
                  }
                  }
                  @media screen and (max-width : 1600px)
                  {
                  .radio-inline {
                  font-size: 10px;
                  }
                  }
                  "
                )),
                class = "col1_top_right",
                leafletOutput("geo_mort_change2",width="100%",height="85%"), 
                radioButtons("year_selector", 
                             label = "Select years:",
                             selected = "2015-2017", 
                             choiceNames = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
                             choiceValues = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
                             inline = TRUE)
                )
                ),
            tags$div(
              class = "hr"
            ),
            tags$div(
              class = "col1_bot",
              tags$div(
                class = "col1_bot_left",leafletOutput("geo_cluster_kmean",width="100%",height="90%")
              ),
              tags$div(
                class = "col1_bot_right", 
                tags$div(
                  class="col1_bot_right_title",
                  uiOutput("textDeathTrends")
                ),
                plotOutput("mort_line",width="100%",height="90%")
                )
            )
            ),
          tags$div(
            class = "vl"
          ),
          tags$div(
            class = "col2",
            tags$div(
              class = "col2_title",
              uiOutput("textDeterminants")
            ),
            tags$div(
              class = "col2_plot",
              plotOutput("page1.bar.cor1",width="100%",height="90%", 
                         hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                         click = clickOpts("page1_bar_plot_click")),
              uiOutput("hover_info")
            )
          )
              )
              ),
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        tags$div(
          class = "page3",
          tags$div(
            class = "page3_col1",
            tags$div(
              class = "col1_title",
              uiOutput("textDeterminants2")
            ),
            plotOutput("determinants_plot1", height = "85%", width = "100%",
                       click = clickOpts("page2_bar_plot_click"))
          ),
          tags$div(
            class = "vl"
          ),
          tags$div(
            class = "page3_col2",
            
            tags$div(
              class = "page3_col2_top",
              plotOutput("determinants_plot2",width="100%",height="85%")
            ),
            tags$div(class = "hr"),
            tags$div(
              class = "page3_col2_bot",
              style = "position: relative",
              uiOutput("determinants_plot3_county_name"),
              plotOutput("determinants_plot3",width="100%",height="85%", click = clickOpts("determinants_plot3_click"))
            )
          ),
          tags$div(
            class = "vl"
          ),
          tags$div(
            class = "page3_col3",
            tags$div(
              pickerInput(
                inputId = "determinant_choice",
                label = "Selected Determinant: ",
                choices = chr.namemap.2019[intersect(colnames(chr.data.2019), rownames(chr.namemap.2019)),],
                selected = "Socio-Economic",
                width = "100%",
                inline = TRUE,
                options = list(
                  `live-search` = TRUE,
                  "dropup-auto" = TRUE
                )
              )
            ),
            tags$div(
              style = "padding: 20px; height: 50%",
              tags$br(),
              tags$h2(textOutput("determinant_title")),
              tags$h4(textOutput("determinant_text")),
              tags$h4(uiOutput("determinant_link"))
            ),
            tags$div(
              class = "col1_bot",
              leafletOutput("determinants_plot5"),
              tags$h5(tags$i("Social determinant geo-distribution"))
            )
          )
        )
        
      ),
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        tags$div(
          class = "page4",
          fluidRow(style = "max-height: 90vh; margin-left: 25px; overflow-y: auto;", 
                   column(3, tags$p("Project Overview",align="center"), tags$br(), #offset=1,
                          fluidRow(
                            # tags$p(tags$img(src="https://i.imgflip.com/t5jc4.jpg", width="75%", height="75%"),align="center"),
                            column(11, "Since 2010 the rate of increase in life expectancy in the United States (US) 
                                   has stagnated and even declined, reversing for the US the trend toward increased life
                                   expectancy that is still continuing in most nations. The goal of this project is 
                                   to develop an interactive tool, MortalityMinder, to explore trends in mortality, 
                                   and identify their associated community level social determinants.", offset=1), # Close column,
                            column(11, tags$p("AHRQ Contest Synopsis",align="center"), tags$br(),
                                   "The AHRQ Visualization Resources of Community-Level Social Determinants of Health Challenge 
                                   seeks tools that support visualizing such data clusters to enhance the research and analysis 
                                   of community-level health services.", tags$br(),
                                   "Challenge participants must develop visualization tools that can augment the insights drawn 
                                   from the analysis of medical expenditure and health care utilization data at the community 
                                   level. Tools must use publicly available and free SDOH data from at least three of the 
                                   following data sources: ", tags$br(),
                                   tags$ul(
                                     tags$li("Federal databases."),
                                     tags$li("State databases."),
                                     tags$li(
                                       "Other locally available data sources, such as SDOH data from voice, digital, and 
                                       social medical requests via service lines.")
                                     ), offset=1) # Close column
                                   ) # Close inner fluidRow
                            ), # Close outter column
                   column(3, tags$p("Methodology",align="center"), tags$br(),  offset=1,
                          fluidRow(
                            column(11, "MortalityMinder finds trends in Mortality Rates in the United States. 
                                   It looks at premature deaths, that is deaths in adults from 15 to 64 
                                   caused by: ", tags$br(),
                                   tags$ul(
                                     tags$li(tags$b("Deaths of Despair: "), 
                                             "deaths due to suicide, overdose, substance abuse and poisonings"),
                                     tags$li(tags$b("Assault: "), 
                                             "deaths due injuries inflicted by another person with intent to injure or kill, 
                                             by any means"),
                                     tags$li(tags$b("Cardiovascular Disease: "), 
                                             "diseases of the circulatory systems such as heart disease and stroke"),
                                     tags$li(tags$b("Cancer: "), 
                                             "deaths due to cancer and neoplasm"),
                                     tags$li(tags$b("All Cause: "), 
                                             "deaths due to any cause")
                                     ), tags$br(),
                                   "Machine learning and statistics methods are used for analysis and data visualization. 
                                   We use standard and advanced machine learning methods such as K-means and Cadre Modeling 
                                   to discover counties with different patterns of mortality over time and associated social 
                                   determinants using cluster or supervised clustering.",
                                   offset=1) # Close Column
                   ), tags$br(),# Close inner fluidRow,
                   tags$div(class="IDEA_Logo_Wrapper",
                            style="position:relative;width: 50%;left: 50%",
                            tags$img(
                              class="Idea_Logo",
                              src="IDEA_logo_500.png", 
                              width="100%", 
                              style="bottom: 0; left: 0;")
                   )
                          ), # Close column
                   column(3, 
                          tags$p("Additional Resources",align="center"), tags$br(), offset=1,
                          "Bennett, K. P., & Erickson, J. S. (2019). MortalityMinder: Exploring and Visualizing Social Determinants 
                          of Mortality. The Rensselaer Institute for Data Exploration and Applications, Rensselaer Polytechnic 
                          Institute. Retrieved from ", tags$br(),
                          tags$a(href="http://orion.tw.rpi.edu/~olyerickson/MortalityMinder_Phase1.pdf", "Mortality Minder Phase 1"), tags$br(),
                          "Erickson, J. S., & Bennett, K. P. (n.d.). Mortality Minder Github. Retrieved from ", tags$br(),
                          tags$a(href="https://github.com/TheRensselaerIDEA/MortalityMinder", "Mortality Minder Github"),tags$br(),
                          "CDC WONDER.", tags$br(),
                          tags$a(href="https://wonder.cdc.gov/", "Center for Disease Control and Prevention"),tags$br(),
                          "County Health Rankings Roadmaps. ", tags$br(),
                          tags$a(href="http://www.countyhealthrankings.org/", "County Health Rankings"),tags$br(),
                          "Small Area Health Insurance Estimates (SAHIE) Program.", tags$br(),
                          tags$a(href="https://www.census.gov/programs-surveys/sahie.html", "Small Area Health Insurance Estimates"),tags$br(),
                          "Agency for Healthcare Research and Quality, US Dept. of Health and Human Services.", tags$br(),
                          tags$a(href="https://www.ahrq.gov/sdoh-challenge/index.html", "AHRQ Challenge Page"),tags$br(),
                          tags$br(),
                          
                          tags$p("Download Data",align="center"), tags$br(),
                          downloadButton("downloadCDCData", "County Deathrate Data"), tags$br(),
                          downloadButton("downloadCHRData", "County Health Rankings (CHR) Factor Data"), tags$br(),
                          downloadButton("downloadFactorDesc", "Factor Descriptions"), tags$br(),
                          tags$br(),
                          tags$p("Download Current Results",align="center"), tags$br(),
                          downloadButton("downloadClusters", "Current State Clusters"), tags$br(),
                          downloadButton("downloadClusterTime", "Current State Clusters Through Time"), tags$br(),
                          downloadButton("downloadCorr", "Current Factor Correlations")
                   )
        )# Close outter fluidRow
        ) # Close page 4 
        )
      )
    ),
  
  tags$script(src = "jquery-ui.min.js"),
  tags$script(src = "fullpage.js"),
  tags$script(src = "jquery.ba-outside-events.js"),
  includeScript(path = "myscript.js")
  )

#------------------

#-----------------

server <- function(input, output, session) {
  county_choice <- reactiveVal()
  
  mort.rate <- reactive({
    county_choice(NULL)
    if(input$state_choice == "United States"){
      cdc.data %>% dplyr::filter(
        death_cause == input$death_cause,
        #state_abbr == input$state_choice,
        period == "2015-2017"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5
          #death_rate = cut(death_rate, bin.geo.mort("Despair"))
        ) %>%
        dplyr::select(county_fips, death_rate)
    }else {
      assign("state_map", readRDS(paste("../shape_files/", input$state_choice, ".Rds", sep = "")), envir = .GlobalEnv)
      cdc.data %>% dplyr::filter(
        death_cause == input$death_cause,
        state_abbr == input$state_choice,
        period == "2015-2017"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5
          #death_rate = cut(death_rate, bin.geo.mort("Despair"))
        ) %>%
        dplyr::select(county_fips, death_rate)
    }
  })
  
  # Cache of UNORDERED mortality trend cluster label calculation
  mort.cluster.raw <- reactive({
    
    # Variables:
    #   - county_fips
    #   - cluster
    
    if (input$state_choice == "United States"){
      # Currently hard-coded 6 clusters
      n.clusters <- n.clusters.nation
      cluster.counties(cdc.mort.mat(cdc.data, "US", input$death_cause),
                       cluster.method="kmeans",
                       cluster.num=n.clusters)
    } 
    else{
      state.data <- cdc.mort.mat(cdc.data, input$state_choice, input$death_cause)
      if (nrow(state.data) <= 6) {
        county_fips <- as.character(state.data$county_fips)
        cluster <- as.character(order(state.data["2015-2017"]))
        tibble(county_fips, cluster)
      }
      else {
        n.clusters <- n.clusters.state
        cluster.counties(state.data,
                         cluster.method="kmeans",
                         cluster.num=n.clusters)
      }
    }
  })
  
  # Cache of Weighed Avg by UNORDERED cluster
  mort.avg.cluster.raw <- reactive({
    
    # Variables:
    #   - period
    #   - cluster
    #   - death_rate
    #   - count
    
    # Notes:
    #   - The cluster labels are UNORDERED
    
    
    get.cluster.deathrate.during.time(
      mort.cluster.raw(), 
      cdc.data, 
      death.cause=input$death_cause
    )
  })
  
  # Cache of MAPPING from UNORDERED mortality trend label to ORDERED mortality trend label
  mort.cluster.map <- reactive({
    
    # Variables:
    #   - ord
    
    # Notes:
    #   - This is a mapping from raw cluster label to ORDERED cluster.
    #       Row names are the original cluster and `ord` are the reordered cluster
    
    get.cluster.order.map(mort.avg.cluster.raw(), time.period = "2015-2017")
  })
  
  # Cache of ORDERED mortality trend cluster label calculation
  mort.cluster.ord <- reactive({
    
    # Variables:
    #   - county_fips
    #   - cluster
    
    order.county.clusters(mort.cluster.raw(), mort.cluster.map())
  })
  
  # Cache of Weighed Avg by ORDERED cluster
  mort.avg.cluster.ord <- reactive({
    
    # Variables:
    #   - period
    #   - cluster
    #   - death_rate
    #   - count
    
    # Notes:
    #   - The cluster labels are ORDERED
    
    order.cluster.deathrate.during.time(mort.avg.cluster.raw(), mort.cluster.map())
  })
  
  # get unfiltered kendal cors
  kendall.cor <- reactive({
    
    kendall.cor.new <- mort.rate() %>% 
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit()
    
  })
  
  
  #Calculate the mean mortality rate for a state  for 2000-2002
  state.mean.2000_2002 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    state.mean <- mean(filtered.data$death_rate)
  })
    
  
  # Calculate national mean mortality for 2000-2002
  national.mean.2000_2002 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    national.mean <- mean(filtered.data$death_rate)
  })
  
  #Calculate the mean mortality rate for a state  for 2015-2017
  state.mean.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    state.mean <- mean(filtered.data$death_rate)
    
  })
  
  # Calculate national mean mortality for 2015-2017
  national.mean.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    national.mean <- mean(filtered.data$death_rate)
  })
  
  # finds states with lowest and highest death rates and returns them
  #   and their respective rates
  #
  # Returns a list in form (lowest death rate, lowest death rate state,
  #   highest death rate, highest death rate state)
  low.high.states.2015_2017 <- reactive({
    
    grouped.data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause,
        period == "2015-2017"
      ) %>%
      group_by(state_name) %>%
      summarise(death_rate = mean(death_rate))
    
    return(
      c(
        min(grouped.data$death_rate),
        grouped.data$state_name[which.min(grouped.data$death_rate)],
        max(grouped.data$death_rate),
        grouped.data$state_name[which.max(grouped.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the highest mortality rate in the state between 2000-2002
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  high.rate.county.2000_2002 <- reactive({
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    return(
      c(
        max(filtered.data$death_rate),
        filtered.data$county_name[which.max(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the highest mortality rate in the state between 2015-2017
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  high.rate.county.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    return(
      c(
        max(filtered.data$death_rate),
        filtered.data$county_name[which.max(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the lowest mortality rate in the state between 2000-2002
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  low.rate.county.2000_2002 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2000-2002"
    )
    
    return(
      c(
        min(filtered.data$death_rate),
        filtered.data$county_name[which.min(filtered.data$death_rate)]
      )
    )
  })
  
  # Identifying a county with the lowest mortality rate in the state between 2015-2017
  #  
  # Returns a list of length 2, where the item at index 1 is the death rate and the
  #   item at index 2 is the county name
  low.rate.county.2015_2017 <- reactive({
    
    filtered.data <- dplyr::filter(
      cdc.data,
      state_abbr == input$state_choice,
      death_cause == input$death_cause,
      period == "2015-2017"
    )
    
    return(
      c(
        min(filtered.data$death_rate),
        filtered.data$county_name[which.min(filtered.data$death_rate)]
      )
    )
  })
  
  #Extracting the national mean
  national.mean <- reactive({
    switch(input$death_cause,
           "Despair" = {
             death_rate <- c(28.929453, 33.665595, 37.821445, 40.081486, 43.900063, 55.084642)
           },
           "Assault" = {
             death_rate <- c(6.750937, 6.729051, 6.687417, 5.934990, 5.915201, 6.999898)
           }, 
           "Cancer" = {
             death_rate <- c(107.637100, 107.638200, 106.628310, 106.949100, 105.219690, 101.169700)
           },
           "Cardiovascular" = {
             death_rate <- c(96.830591, 95.807343, 92.915303, 90.702418, 91.232679, 93.598232)
           })
    
    
    nation.dataframe <- data.frame(
      period = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
      cluster = rep("National", 6),
      death_rate,
      count = rep(NA, 6))
  })
  
  #Extracting the national mean
  determinant.url <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"URL"))
  })

  
  # ----------------------------------------------------------------------
  # Functions for data download
  
  # Outputs cdc.data as a csv
  output$downloadCDCData <- downloadHandler(
    filename = function() {
      "cdc_data.csv"
    },
    content = function(file) {
      write.csv(cdc.data, file, row.names = FALSE)
    }
  )
  
  # Outputs chr.data.2019 as a csv
  output$downloadCHRData <- downloadHandler(
    filename = function() {
      "chr_data_2019.csv"
    },
    content = function(file) {
      write.csv(chr.data.2019, file, row.names = FALSE)
    }
  )
  
  # Outputs chr.namemap.2019 as a csv
  output$downloadFactorDesc <- downloadHandler(
    filename = function() {
      "chr_data_desc.csv"
    },
    content = function(file) {
      write.csv(SocialDeterminants, file, row.names = FALSE)
    }
  )
  
  # Outputs mort.cluster.ord as a csv
  output$downloadClusters <- downloadHandler(
    filename = function() {
      paste0(input$state_choice, "_", input$death_cause, "_clusters.csv")
    },
    content = function(file) {
      write.csv( mort.cluster.ord(), file, row.names = FALSE)
    }
  )
  
  # Outputs mort.avg.cluster.ord as a csv
  output$downloadClusterTime <- downloadHandler(
    filename = function() {
      paste0(input$state_choice, "_", input$death_cause, "_clusters_time_series.csv")
    },
    content = function(file) {
      write.csv( mort.avg.cluster.ord(), file, row.names = FALSE)
    }
  )
  
  # Outputs kendall.cor as a csv
  output$downloadCorr <- downloadHandler(
    filename = function() {
      paste0(input$state_choice, "_", input$death_cause, "_", input$determinant_choice , "_correlations.csv")
    },
    content = function(file) {
      write.csv( kendall.cor(), file, row.names = FALSE)
    }
  )
  
  # ----------------------------------------------------------------------
  
  
  output$national_map<-renderUI({
    if(input$death_cause == "Despair"){
      includeScript(path = "Despair.js")
    }
    else if(input$death_cause == "Cancer"){
      includeScript(path = "Cancer.js")
    }
    else if(input$death_cause == "Assault"){
      includeScript(path = "Assault.js")
    }
    else if(input$death_cause == "Cardiovascular"){
      includeScript(path = "Cardio.js")
    }
  })
  output$determinants_plot1 <- renderPlot({
    
    # Sort by kendall.cor
    kendall.cor.new <- kendall.cor() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, kendall_cor) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    #Only display the social determinants graph if there is any significant social determinant
    #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
    if(nrow(kendall.cor.new) > 0) {
      kendall.cor.new %>% 
        ggplot(
          aes(
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            y = kendall_cor, 
            color = DIR, 
            fill = DIR)
        ) + 
        
        # Lolipop chart
        geom_point(stat = 'identity', size = 12) + 
        geom_segment(
          size = 1,
          aes(
            y = 0, 
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            yend = kendall_cor, 
            #xend = reorder(chr_code, kendall_cor), 
            xend = chr_code, 
            color = DIR
          )
        ) +
        geom_text(
          aes(
            label = chr_code, 
            y = ifelse(DIR == "Protective", 0.1, -0.1),
            hjust = ifelse(DIR == "Protective", 0, 1)
          ), 
          color = "black", 
          size = 4
        ) +
        geom_text(
          aes(label = round(kendall_cor, 2)), 
          color = "black", 
          size = 3
        ) +
        
        # Coordinates
        coord_flip() + 
        scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
        
        # Themes
        geom_hline(yintercept = .0, linetype = "dashed") + 
        labs(
          title = "Most Related Factors",
          subtitle = "Kendall Correlation between Factors and Mortality Risk Cluster",
          caption = "Data Source:\n\t1.CDCWONDER Multi-Cause of Death\n\t2.County Health Ranking 2019",
          y = "Correlation",
          x = NULL,
          fill = "Relationship",
          color = "Relationship"
        ) + 
        theme_minimal() +
        theme.text() + 
        theme.background() + 
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          panel.grid.major.y = element_blank()
        ) + 
        theme(legend.position="top")
    }
    
    #Display something else when there are no significant SD
    else {
      
      # empty plot, then put text on it ?
      ggplot() + theme_void() +
        geom_text(aes(x = 0, y = 0, label="There are no significant social determinants."))
      
    }
  })
  
  update.county.fips <- function(value) {
    if (!is.na(value) & nchar(value) == 4) {
      return (
        paste("0", value, sep = "")
      )
    } else {
      return (value)
    }
  }
  
  output$determinants_plot2 <- renderPlot({
    
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>% 
      tidyr::drop_na()
    
    if (nrow(sd.select) <= 6){
      
      ggplot(sd.select, aes(x = cluster, y = VAR, fill = cluster)) + 
        geom_boxplot() +
        labs(y = input$determinant_choice, caption = "Plot will show only single values if the state has too few counties to cluster (6 or fewer). \n In these cases, the x-axis is individual counties rather than clusters.") + 
        theme.background() + 
        theme.text() + 
        theme(
          
          panel.grid = element_line(color = "grey"),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          
          axis.line.x = element_blank(), 
          axis.title.x = element_blank(),
          
          legend.position = "none"
        ) + ggtitle(paste(input$determinant_choice, "and Risk County Relationship"))+
        scale_fill_manual(
          name = "County",
          labels = sd.select$county_name,  
          values = colorRampPalette(
            c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
          )(max(sd.select$cluster))
        )
      
    } else{
      
      ggplot(sd.select, aes(x = cluster, y = VAR, fill = cluster)) + 
        geom_boxplot() +
        theme.background() + 
        theme.text() + 
        theme(
          
          panel.grid = element_line(color = "grey"),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          
          axis.line.x = element_blank(), 
          axis.title.x = element_blank(),
          
          legend.position = "none"
        ) + 
        labs(
          x = "Cluster",
          y = input$determinant_choice
         
        ) + ggtitle(paste(input$determinant_choice, "and Risk Cluster Relationship"))+
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
      
    }
    
  })
  
  
  output$determinants_plot3 <- renderPlot({
    
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>%
      tidyr::drop_na()
    
    if (nrow(sd.select) <= 6){
      
      dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() %>%
        
        
        ggplot(aes(x = death_rate, y = VAR)) + 
        geom_point(aes(fill = cluster)) + 
        labs(
          x = "Mortality Rate (2015-2017)",
          y = input$determinant_choice
        ) + ggtitle(paste(input$determinant_choice, "and Mortality Relationship")) +
        theme.line.mort() + 
        theme(legend.position = "top") + 
        guides(color = guide_legend(override.aes = list(shape = 15))) + 
        color.line.cluster(input$state_choice, max(sd.select$cluster)) + 
        scale_color_manual(
          name = "County",
          labels = sd.select$county_name,
          #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
          values = colorRampPalette(
            c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
            
          )(nrow(sd.select)),
          guide = guide_legend(reverse = T)
        )
      
    } else if(input$state_choice == "United States"){
      dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() %>%
        
        ggplot(aes(x = death_rate, y = VAR)) + 
        #geom_point(colour="black", shape=21, size = 3, alpha = .7,
        #aes(fill = cluster)) + 
        stat_density_2d(aes(alpha = ..level.., fill=cluster), geom = "polygon") +
        labs(
          x = "Mortality Rate (2015-2017)",
          y = input$determinant_choice
        ) + ggtitle(paste(input$determinant_choice, "and Mortality Relationship"))+
        theme.line.mort() + 
        theme(legend.position = "top") + 
        guides(color = guide_legend(override.aes = list(shape = 15))) + 
        color.line.cluster(input$state_choice, max(sd.select$cluster)) +
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
    } else {
      
#      browser()
      dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() %>%
        
        ggplot(aes(x = death_rate, y = VAR)) + 
        geom_point(colour="black", shape=21, size = 3, alpha = .7,
                   aes(fill = cluster)) + 
        #stat_density_2d(aes(alpha = ..level.., fill=cluster), geom = "polygon") +
        labs(
          x = "Mortality Rate (2015-2017)",
          y = input$determinant_choice
        ) + ggtitle(paste(input$determinant_choice, "and Mortality Relationship"))+
        theme.line.mort() + 
        theme(legend.position = "top") + 
        guides(color = guide_legend(override.aes = list(shape = 15))) + 
        color.line.cluster(input$state_choice, max(sd.select$cluster)) +
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
    }
  })

  # Geo-plot of selected determinant for selected county
  # Based on scatterplot
  output$determinants_plot5 <- renderLeaflet({
    
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    sd.select <- chr.data.2019 %>%
      dplyr::select(county_fips, VAR = sd.code) %>%
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>%
      dplyr::inner_join(geo.namemap, by = "county_fips") %>%
      tidyr::drop_na()
    
    if (nrow(sd.select) <= 6){
      
      dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() 

    } else if(input$state_choice == "United States"){
      dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() 

    } else {
      
      sd.data <- dplyr::filter(
        cdc.data,
        period == "2015-2017", 
        death_cause == input$death_cause
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na()
        
      # NOTE: The column we care about is now called VAR
      geo.sd.plot(input$state_choice, input$determinant_choice, sd.data, "2015-2017")
      
    }
    
    
# # Mortality Rate by County Period 2
#       if(input$state_choice == "United States"){
#         # mort.data <- dplyr::filter(
#         #   cdc.data,
#         #   death_cause == input$death_cause,
#         #   period == input$year_selector
#         # ) %>% 
#         #   dplyr::mutate(
#         #     # death_rate = death_num / population * 10^5,
#         #     death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
#         #   ) %>%
#         #   dplyr::select(county_fips, death_rate, period)
#         # 
#         # geo.plot("US", input$death_cause, mort.data, input$year_selector)
#       } else{
#         sd.data <- dplyr::filter(
#           cdc.data,
#           state_abbr == input$state_choice,
#           death_cause == input$death_cause,
#           period == input$year_selector
#         ) %>% 
#           dplyr::mutate(
#             # death_rate = death_num / population * 10^5,
#             death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
#           ) %>%
#           dplyr::select(county_fips, death_rate, period)
#         
#         geo.plot(input$state_choice, input$death_cause, sd.data, input$year_selector)
#       }

    
  })
  
  output$determinant_title <- renderText({
    input$determinant_choice
  })
  
  output$state_title <- renderText({
    if (input$state_choice == "United States") {
      "United States"
    }
    else {
      names(which(state.list == input$state_choice))
    }
  })
  
  output$determinant_text <- renderText({
    as.character(
      SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Definitions")
  })
  
  output$determinant_link <- renderUI({
    tagList(
      tags$a(
        "Click here for more information",
        href = determinant.url(),
        target="_blank"
      )
    )
  })
  
  output$determinants_plot4 <- renderPlot({
    
  })
  
  # Mortality Rate Trend Line Graph
  output$mort_line <- renderPlot({
    
    if (input$state_choice == "United States"){
      
      ggplot(
        mort.avg.cluster.ord(),
        aes(
          x = period, y = death_rate, 
          color = cluster, group = cluster
        )
      ) + 
        geom_line(size = 1.5) + 
        geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
        # labs.line.mort(input$state_choice, input$death_cause) + 
        scale_color_manual(
          values = theme.categorical.colors.accent(max(mort.cluster.ord()$cluster))) +
        theme.line.mort() + 
        guides(
          color = guide_legend(reverse = T)
        )
    } else {

      nclusters <- max(mort.cluster.raw()$cluster)
      if (input$death_cause != "All Cause") {
        total.data <- rbind(mort.avg.cluster.ord(), national.mean())
      } else {
        total.data <- mort.avg.cluster.ord()
      }
      
      
      line_plot <- ggplot(
        total.data,
        aes(
          x = period, y = death_rate, 
          color = cluster, group = cluster
        )
      ) + 
        geom_line(size = 1.5) + 
        geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
        # labs.line.mort(input$state_choice, input$death_cause) + 
        scale_color_manual(
          values = theme.categorical.colors.accent(nclusters)) +
        theme.line.mort() + 
        theme(legend.position = "left") + 
        guides(color = guide_legend(reverse = T)) +
        labs(fill = "Cluster \n Average", color = "Cluster \n Average") + 
        ylab("Mortality Rate (# per 100k)")
      
      if (is.null(county_choice())){
        line_plot 
      } else {
        drop.cols <- c('county_fips')
        county_data <- cdc.countymort.mat(cdc.data, input$state_choice, county_choice(), input$death_cause) %>%
          dplyr::select(-drop.cols) %>%
          tidyr::gather("period", "death_rate", "2000-2002":"2015-2017") %>%
          dplyr::mutate("county" = county_choice())
        line_plot + 
          geom_line(
            mapping = aes(x = period, y = death_rate, group = county, linetype=county_choice()),
            data = county_data, color = "black", size = 1.3
          ) +
          geom_point(
            mapping = aes(x = period, y = death_rate),
            data = county_data, color = "black", shape = 21, 
            fill = "white", inherit.aes = FALSE, size = 2
          ) +
          scale_linetype_manual(name = "County",
                                values = c("twodash"),
                                guide = guide_legend(override.aes = list(color = c("black")))
          )
      }
    }
    
  })
  
  # Textual description box (upper-left panel, Page 1)
  output$textDescription <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        paste0("Mortality rates for ",names(which(cause.list == input$death_cause)), " for the State of ", names(which(state.list == input$state_choice)))
      ),
      tags$h4(paste0(names(which(cause.definitions == input$death_cause)))),
      tags$h5(tags$i("Select year range to see statewide mortality rate distribution for that period. Mouse over maps to identify indiviual counties. Zoom map with mouse wheel or zoom buttons.")),
      NULL
    )
  })

  output$textMortFactsTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
    }
    else {
      location_str <- names(which(state.list == input$state_choice))
    }
    tagList(
      tags$h3(
        paste0("Premature Mortality Rates for ",
               names(which(cause.list == input$death_cause)), 
               " in ", 
               location_str,
               ":")
      )
    )
  })
  
  output$textMortFactsClosing <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(paste0(names(which(cause.definitions == input$death_cause))))
    )
  })
  
  # for a state or the US, creates the bulleted facts at the bottom of nationwide 
  #  page
  output$textMortFacts <- renderUI({
    if(input$state_choice == "United States") {
      # percent change for first bullet
      change_text <- "remained the same"
      
      percent_change <- round(
        abs(national.mean.2015_2017() - national.mean.2000_2002()) / national.mean.2000_2002() * 100,
        1
      )
      
      if (percent_change > 0) {
        change_text <- paste0("increased ", percent_change, "%")
      }
      else if (percent_change < 0) {
        change_text <- paste0("decreased ", percent_change, "%")
      }
      
      tagList(
        tags$ul(
          style = "font-size: 18px;",
          tags$li(paste0("have ", change_text, " from 2000 to 2017")),
          tags$li(paste0("range from ", 
                         round(as.numeric(low.high.states.2015_2017()[1]), 1),
                         " per 100k in ",
                         low.high.states.2015_2017()[2],
                         " to ",
                         round(as.numeric(low.high.states.2015_2017()[3]), 1),
                         " per 100k in ",
                         low.high.states.2015_2017()[4],
                         " 2015-2017")
          )
        )
      )
    }
    else {
      # percent change for first bullet
      change_text <- "remained the same"
      
      percent_change <- round(
        abs(state.mean.2015_2017() - state.mean.2000_2002()) / state.mean.2000_2002() * 100,
        1
      )
      
      if (percent_change > 0) {
        change_text <- paste0("increased ", percent_change, "%")
      }
      else if (percent_change < 0) {
        change_text <- paste0("decreased ", percent_change, "%")
      }
      
      # comparison wish national average
      
      comparison_text <- "the same as"
      
      if (national.mean.2015_2017() > state.mean.2015_2017()) {
        comparison_text <- "lower than"
      }
      else if (national.mean.2015_2017() < state.mean.2015_2017()) {
        comparison_text <- "greater than"
      }
      
      tagList(
        tags$ul(
          style = "font-size: 18px;",
          tags$li(paste0("have ", change_text, " from 2000 to 2017")),
          tags$li(paste0("were ", comparison_text, " the national mean in 2015-2017")),
          tags$li(paste0("range from ", 
                         round(as.numeric(low.rate.county.2015_2017()[1]), 1),
                         " per 100k in ",
                         low.rate.county.2015_2017()[2],
                         " to ",
                         round(as.numeric(high.rate.county.2015_2017()[1]), 1),
                         " per 100k in ",
                         high.rate.county.2015_2017()[2],
                         " 2015-2017")
                  )
        )
      )
    }
  })
  
  output$textMortFacts1 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h5("Mean Mortality Rate for 2000-2002:", round(state.mean.2000_2002(),2)),
      tags$h5("Mean Mortality Rate for 2015-2017:", round(state.mean.2015_2017(),2)),
      tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
      tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2)),
      tags$h5("Lowest Rate County for 2000-2002:", low.rate.county.2000_2002()),
      tags$h5("Lowest Rate County for 2015-2017:", low.rate.county.2015_2017()),
      tags$h5("National Mean for 2000-2002:", round(national.mean()[national.mean()$period == "2000-2002",]$death_rate,2)),
      tags$h5("National Mean for 2015-2017:", round(national.mean()[national.mean()$period == "2015-2017",]$death_rate,2))
    )
  })
  
  output$textNationalTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h1(
        paste0(names(which(cause.list == input$death_cause)), " Rates Over Time")
      ),
      tags$h5(tags$i("Click on time period to select national map for that period"))
    )
  })
  
  # Determinant Header (upper-right panel, Page 1)
  output$textDeterminants <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        paste0("Factors related to ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))), tags$div(
          title="Each factor is rated as Destructive, meaning 
          that it has a positive correlation with the 
          death rate; or Protective, meaning it has a 
          negative correlation with the death rate. 
          MortalityMinder shows those factors which have 
          the highest absolute correlation with mortality. 
          For more information on the method of determining
          correlation please navigate to...",
          icon("info-circle")
        )
      ),
      NULL
      )
  })

  # Death Trends Header (Page 2 lower middle)
  output$textDeathTrends <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        style = "padding-right: 20px; padding-left: 20px",
        paste0(names(which(cause.list == input$death_cause)), " trends for ", names(which(state.list == input$state_choice))), tags$div(
          title="This plot represents the average premature death trends for each cluster.",
          icon("info-circle")
        )
      ),
      NULL
    )
  })
  
  # Determinant Header (upper-left panel, Page 2)
  output$textDeterminants2 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        paste0("Factors related to ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))), tags$div(
          title="Each factor is rated as Destructive, meaning 
          that it has a positive correlation with the 
          death rate; or Protective, meaning it has a 
          negative correlation with the death rate. 
          MortalityMinder shows those factors which have 
          the highest absolute correlation with mortality. 
          For more information on the method of determining
          correlation please navigate to...",
          icon("info-circle")
        )
      ),
      NULL
      )
  })
  
  # Determinant Header (upper-center panel, Page 2)
  output$textDeterminants3 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        paste0("Distribution of '",input$determinant_choice, "' across ", names(which(cause.list == input$death_cause)), " clusters for ", names(which(state.list == input$state_choice))), tags$div(
          title="Help text for cluster distribution bar plots",
          icon("info-circle")
        )
      ),
      NULL
    )
  })
  
  
  # Mortality Rate Table
  output$table <- renderTable(width = "100%", {
    rate.table <- mort.avg.cluster.ord() %>%
      dplyr::select(cluster, period, death_rate) %>%
      tidyr::spread(key = period, value = death_rate) %>% 
      dplyr::select(cluster, `2000-2002`, `2015-2017`)
    
    count.table <- mort.avg.cluster.ord() %>% 
      dplyr::select(cluster, count) %>% 
      base::unique()
    
    dplyr::left_join(count.table, rate.table, by = "cluster") %>%
      dplyr::mutate(cluster = as.character(cluster)) %>%
      dplyr::arrange(desc(cluster)) %>%
      dplyr::rename(
        "Trend Grp." = "cluster",
        "Count" = "count"
      ) 
  })
  
  # Mortality Trend Cluster by County
  output$geo_cluster_kmean <- renderLeaflet({
    
    if(input$state_choice == "United States"){
      # draw.geo.cluster("US", input$death_cause, mort.cluster.ord(), 
      #                  max(mort.cluster.ord()$cluster))
    }else{
      draw.geo.cluster(input$state_choice, input$death_cause, mort.cluster.ord(), 
                       max(mort.cluster.ord()$cluster))
    }
    
  })
  
  
  # Mortality Trend Cluster by County
  # TODO: Replace this with a social determinant map!
  output$geo_cluster_kmean_2 <- renderLeaflet({
    
    if(input$state_choice == "United States"){
      # draw.geo.cluster("US", input$death_cause, mort.cluster.ord(),
      #                  max(mort.cluster.ord()$cluster))
    }else{
      draw.geo.cluster(input$state_choice, input$death_cause, mort.cluster.ord(),
                       max(mort.cluster.ord()$cluster))
    }
    
  })
  
  # Mortality Rate by County Period 1
  output$geo_mort_change1 <- renderLeaflet({
    if(input$state_choice == "United States"){
      mort.data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause,
        period == "2000-2002"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      
      geo.plot("US", input$death_cause, mort.data, "2000-2002")
      
    } else {
      
      mort.data <- dplyr::filter(
        cdc.data,
        state_abbr == input$state_choice,
        death_cause == input$death_cause,
        period == "2000-2002"
      ) %>%
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      geo.plot(input$state_choice, input$death_cause, mort.data, "2000-2002")
    }
    
  })
  
  # Mortality Rate by County Period 2
  output$geo_mort_change2 <- renderLeaflet({
    if(input$state_choice == "United States"){
      # mort.data <- dplyr::filter(
      #   cdc.data,
      #   death_cause == input$death_cause,
      #   period == input$year_selector
      # ) %>% 
      #   dplyr::mutate(
      #     # death_rate = death_num / population * 10^5,
      #     death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
      #   ) %>%
      #   dplyr::select(county_fips, death_rate, period)
      # 
      # geo.plot("US", input$death_cause, mort.data, input$year_selector)
    } else{
      mort.data <- dplyr::filter(
        cdc.data,
        state_abbr == input$state_choice,
        death_cause == input$death_cause,
        period == input$year_selector
      ) %>% 
        dplyr::mutate(
          # death_rate = death_num / population * 10^5,
          death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
        ) %>%
        dplyr::select(county_fips, death_rate, period)
      
      geo.plot(input$state_choice, input$death_cause, mort.data, input$year_selector)
    }
    
  })
  
  # Implementation of hover (08 Oct 2019)
  output$hover_info <- renderUI({
    req(input$plot_hover) # Same as if-not-NULL
    hover <- input$plot_hover

    #   Replaced with new definition (from above) 
    kendall.cor.new <- mort.rate() %>% 
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, kendall_cor) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    
    point <- nearPoints(kendall.cor.new, hover, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    #browser()
    # actual tooltip created as wellPanel
    # TODO: Change these variables based on `kendall.cor`
    wellPanel(
      style = style,
      p(HTML(paste0("<b>", point$chr_code, "</b><br/>",
                    "<i>", point$DIR, "</i>","<br/>",
                    SocialDeterminants[SocialDeterminants$Name == as.character(point$chr_code),]$Definition[[1]],
                    NULL
      )))
    )
    
  })
  
  output$determinants_plot3_county_name <- renderUI({
    req(input$determinants_plot3_click) # Same as if-not-NULL
    click <- input$determinants_plot3_click
    
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>%
      tidyr::drop_na()
    
      
    data <- dplyr::filter(
      cdc.data,
      period == "2015-2017", 
      death_cause == input$death_cause
    ) %>% 
      dplyr::select(county_fips, death_rate) %>% 
      dplyr::inner_join(sd.select, by = "county_fips") %>% 
      tidyr::drop_na()
    
    
    point <- nearPoints(data, click, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (click$x - click$domain$left) / (click$domain$right - click$domain$left)
    top_pct <- (click$domain$top - click$y) / (click$domain$top - click$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- click$range$left + left_pct * (click$range$right - click$range$left)
    top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 10, "px; top:", top_px - 30, "px; font-size: 7px")
    
    #browser()
    # actual tooltip created as wellPanel
    # TODO: Change these variables based on `kendall.cor`
    tags$div(
      style = style,
      h5(point$county_name)
    )
    
  })
  
  # Kendall Correlation Between Cluster and CHR-SD
  output$page1.bar.cor1 <- renderPlot({
    
    # Sort by kendall.cor
    kendall.cor.new <- kendall.cor() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, kendall_cor) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    # # Set currently selected determinant to most correlated determinant
    # max.cor.ind = which.max(abs(kendall.cor.new$kendall_cor))
    # input$determinant_choice = kendall.cor.new[max.cor.ind, "chr_code"]
    
    #Only display the social determinants graph if there is any significant social determinant
    #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
    if(nrow(kendall.cor.new) > 0) {
      kendall.cor.new %>% 
        ggplot(
          aes(
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            y = kendall_cor, 
            color = DIR, 
            fill = DIR)
        ) + 
        
        # Lolipop chart
        geom_point(stat = 'identity', size = 12) + 
        geom_segment(
          size = 1,
          aes(
            y = 0, 
            #x = reorder(chr_code, kendall_cor), 
            x = chr_code, 
            yend = kendall_cor, 
            #xend = reorder(chr_code, kendall_cor), 
            xend = chr_code, 
            color = DIR
          )
        ) +
        geom_text(
          aes(
            label = chr_code, 
            y = ifelse(DIR == "Protective", 0.1, -0.1),
            hjust = ifelse(DIR == "Protective", 0, 1)
          ), 
          color = "black", 
          size = 4
        ) +
        geom_text(
          aes(label = round(kendall_cor, 2)), 
          color = "black", 
          size = 3
        ) +
        
        # Coordinates
        coord_flip() + 
        scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
        
        # Themes
        geom_hline(yintercept = .0, linetype = "dashed") + 
        labs(
          title = "Most Related Factors",
          subtitle = "Kendall Correlation between Factors and Mortality Risk Cluster",
          caption = "Data Source:\n\t1.CDCWONDER Multi-Cause of Death\n\t2.County Health Ranking 2019",
          y = "Correlation",
          x = NULL,
          fill = "Relationship",
          color = "Relationship"
         ) + 
        theme_minimal() +
        theme.text() + 
        theme.background() + 
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          panel.grid.major.y = element_blank()
        ) + 
        theme(legend.position="top")
    }
    #Display something else when there are no significant SD
    else {
      
      # empty plot, then put text on it ?
      ggplot() + theme_void() +
        geom_text(aes(x = 0, y = 0, label="There are no significant social determinants."))
      
    }
  })
  
  
  highlight_county <- function(event){
    if (is.null(event))
      return()
    county_name <- sub(event$id, pattern = " [[:alpha:]]*$", replacement = "")
    
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) == 1){
      polygon <- state_map@polygons[[county_indices[[1]]]]
    } else {
      for (index in county_indices){
        current_polygon <- state_map@polygons[[index]]
        current_coords <- current_polygon@Polygons[[1]]@coords
        if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
          polygon = current_polygon
          break
        }
      }
    }
    
    cluster_proxy <- leafletProxy("geo_cluster_kmean")
    #remove any previously highlighted polygon
    cluster_proxy %>% clearGroup("highlighted_polygon")
    
    #add a slightly thicker red polygon on top of the selected one
    cluster_proxy %>% addPolylines(stroke = TRUE, 
                                   weight = 2,
                                   color="#000000",
                                   data = polygon,
                                   group="highlighted_polygon",
                                   dashArray = "4 2 4")
    
    change_proxy <- leafletProxy("geo_mort_change2")
    #remove any previously highlighted polygon
    change_proxy %>% clearGroup("highlighted_polygon")
    
    #add a slightly thicker red polygon on top of the selected one
    change_proxy %>% addPolylines(stroke = TRUE, 
                                  weight = 2,
                                  color="#000000",
                                  data = polygon,
                                  group="highlighted_polygon",
                                  dashArray = "4 2 4")
  }
  
  # click on geo cluster map shows county data on mort_line
  observe({
    event <- input$geo_cluster_kmean_shape_click
    highlight_county(event)
    county_choice(event$id)
  })
  
  observe({
    event <- input$geo_mort_change2_shape_click
    highlight_county(event)
    county_choice(event$id)
  })
  
  observe({
    event <- input$determinants_plot3_click
    req(event)
    
    geo.namemap$county_fips <- with_options(c(scipen = 999), str_pad(geo.namemap$county_fips, 5, pad = "0"))
    sd.code = chr.namemap.inv.2019[input$determinant_choice, "code"]
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>%
      tidyr::drop_na()
    
    
    data <- dplyr::filter(
      cdc.data,
      period == "2015-2017", 
      death_cause == input$death_cause
    ) %>% 
      dplyr::select(county_fips, death_rate) %>% 
      dplyr::inner_join(sd.select, by = "county_fips") %>% 
      tidyr::drop_na()
    
    
    point <- nearPoints(data, event, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    county_name = point$county_name
    
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) == 0) return(NULL)
    
    if (length(county_indices) == 1){
      polygon <- state_map@polygons[[county_indices[[1]]]]
    } else {
      # TODO
      polygon <- state_map@polygons[[county_indices[[1]]]]
      # for (index in county_indices){
      #   current_polygon <- state_map@polygons[[index]]
      #   current_coords <- current_polygon@Polygons[[1]]@coords
      #   if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
      #     polygon = current_polygon
      #     break
      #   }
      # }
    }
    
    cluster_proxy <- leafletProxy("determinants_plot5")
    #remove any previously highlighted polygon
    cluster_proxy %>% clearGroup("highlighted_polygon")
    
    #add a slightly thicker red polygon on top of the selected one
    cluster_proxy %>% addPolylines(stroke = TRUE, 
                                   weight = 2,
                                   color="#000000",
                                   data = polygon,
                                   group="highlighted_polygon",
                                   dashArray = "4 2 4")
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page1_bar_plot_click) # Same as if-not-NULL
    click <- input$page1_bar_plot_click
    
    #   Replaced with new definition (from above) 
    kendall.cor.new <- mort.rate() %>% 
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, kendall_cor) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    point <- nearPoints(kendall.cor.new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
    js$nextpage()
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page2_bar_plot_click) # Same as if-not-NULL
    click <- input$page2_bar_plot_click
    
    #   Replaced with new definition (from above) 
    kendall.cor.new <- mort.rate() %>% 
      dplyr::mutate(VAR = death_rate) %>%
      kendall.func(chr.data.2019) %>%
      dplyr::mutate(
        DIR = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        ),
        chr_code = chr.namemap.2019[chr_code, 1]
      ) %>% na.omit() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, kendall_cor) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    point <- nearPoints(kendall.cor.new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
  })
}

shinyApp(ui = ui, server = server)
