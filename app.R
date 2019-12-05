# Date: 2019/8/1
# Contributors: 
#   UI: Shengjin Li (prototype)
#   Server: Yuxuan Wang (prototype)
#   Visualizations: Ziyi Wang (prototype)
#   Fall 2019 Coding Team: Ross DeVito, Jose Figueroa, Xinyu Gu, Taras Kaminsky, Lilian Ngweta, 
#             Jiangshan Lin, Farukh Saidmuratov, Christina Van Hal, Ziyi Wang, Hongrui Zhang
#   Advisors: Kristin Bennett, John Erickson, Karan Bhanot
#   The Rensselaer Institute for Data Exploration & Applications (IDEA)

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

# Cause list with Assault
# cause.list <- c("Deaths of Despair"="Despair","Cancer Deaths"="Cancer","Deaths by Assault"="Assault","Cardiovascular Disease"="Cardiovascular", "All Cause" = "All Cause")
# cause.definitions <- c("\"Deaths of Despair\" are deaths due to suicide, overdose, substance abuse and poisonings"="Despair",
#                        "\"Deaths by Assault\" are deaths caused by injuries inflicted by another person with intent to injure or kill, by any means"="Assault",
#                        "\"Cardiovascular Disease\" are deaths due to diseases of the circulatory systems such as heart disease and stroke"="Cardiovascular",
#                        "\"Cancer Deaths\" are deaths due to cancer and neoplasm"="Cancer")

# Cause list with Assault commented out
cause.list <- c("Deaths of Despair"="Despair","Cancer Deaths"="Cancer","Cardiovascular Disease"="Cardiovascular", "All Cause" = "All Cause")
cause.definitions <- c("\"Deaths of Despair\" are deaths due to suicide, overdose, substance abuse and poisonings"="Despair",
                       "\"Cardiovascular Disease\" are deaths due to diseases of the circulatory systems such as heart disease and stroke"="Cardiovascular",
                       "\"Cancer Deaths\" are deaths due to cancer and neoplasm"="Cancer")

period.list <- c("2000-2002","2003-2005","2006-2008","2009-2011","2012-2014","2015-2017")

n.clusters.state = 3
n.clusters.nation = 6
jscode <- "shinyjs.nextpage = function(){$('.fp-next').click();}"

ui <- fluidPage(

##################### CSS Imports #####################  

  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("nextpage")),
  tags$head(includeCSS("custom_no_scroll.css")),
  tags$head(includeCSS("jquery-ui.min.css")),
  tags$head(includeCSS("fullpage.css")),
  tags$head(includeCSS("geoattr.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
##################### NAV BAR #####################
  tags$div(
    class = "navbar",

    tags$div(
      class = "title",
      tags$h1(
        # tags$img(
        #   src="RPIlogo_black.png"
        #   # height="30px"
        #   ),
        "MortalityMinder")
      ),
    tags$div(
      class = "nav_container",
    
    tags$div(
      class = "prompt_text",
      # "Select cause of death and state: "
      "Select State and Cause of Death:  "
    ),
    
    pickerInput(
      inputId = "death_cause",
      label = h4("Cause of Death"),
      choices = cause.list,
      choicesOpt = list(
        subtext = c("Self-Harm and some other causes"),
        "dropup-auto" = FALSE
      )
    ),
    pickerInput(
      inputId = "state_choice",
      label = h4("State"), 
      choices = state.list,
      selected = "PA",
      options = list(
        `live-search` = TRUE,
        "dropup-auto" = FALSE
      )
    )
    
    )
    
    
  ),
  
  tags$div(
    id = "fullpage",
    tags$div(
      class = "section s1",
      
##################### PAGE 1, NATIONWIDE ANALYSIS #####################

      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        # Div tag functions as outter "shell" to pull from fullpage.css
        # Each page is a row, of columns, of rows, etc.
        
        fluidRow(
          class = "page page1", # National Map Page
          uiOutput("national_map"),
          column(3, 
                  class="page1_col page1_col1", 
                 tags$div(
                   class = "page1_col1_heading",
                  tags$h3("Which county-level social and economic factors increase mortality in the United States?")
                  ),
                 tags$h4("MortalityMinder analyzes trends of premature death in the United States which are caused by:\n"),
                    tags$ul(
                      tags$li(tags$h4("Deaths of Despair")),
                      tags$li(tags$h4("Cardiovascular Disease")),
                      tags$li(tags$h4("Cancer")),
                      # tags$li(tags$h4("Assault Deaths")),
                      tags$li(tags$h4("All Causes"))
                       ), # End List
                      tags$h4("MortalityMinder is a four-view interactive presentation that examines county-level factors associated with midlife mortality trends.\n"), 
                      tags$h4("Pick state (or United States) and cause of death on the menu bar at the top of the page to see how mortality rates in the United States have changed from 2000 to 2017.\n"), 
                      tags$h5(tags$i("Click << and >> for State and Factor views")
                              ), 
                 # fluidRow(class="IDEA_Logo_Wrapper",
                          tags$img(
                            # class="rensselaer_logo",
                            src="RPIlogo.png", 
                            width="80%", 
                            style="bottom: 0; left: 0;")
                 # )
                 
          ), # End Column 1
          tags$div(
            class = "vl"
          ),
          column(8,
                fluidRow(
                  class = "page1_col page1_col2_top",
                  tags$div(
                    class = "National_title page1_title",
                    uiOutput("textNationalTitle"),
                    uiOutput("textMortFactsClosing")
                  )
                  ), # End of inner FluidRow (Column 2 top)

                fluidRow(class="page1_col page1_col2_middle",
                  fluidRow(
                    tags$ul(
                      class = "ul_period",
                      tags$button(
                        id = "first_period",
                        class = "period_text",
                        "2000-02"
                      ),
                      tags$button(
                        id = "second_period",
                        class = "period_text",
                        "2003-05"
                      ),
                      tags$button(
                        id = "third_period",
                        class = "period_text",
                        "2006-08"
                      ),
                      tags$button(
                        id = "forth_period",
                        class = "period_text",
                        "2009-11"
                      ),
                      tags$button(
                        id = "fifth_period",
                        class = "period_text",
                        "2012-14"
                      ),
                      tags$button(
                        id = "sixth_period",
                        class = "period_text",
                        style= "background-color: #565254; color: #f7f7f7;",
                        "2015-17"
                      )
                    ) # End List of buttons
                  ), # End Button Functionality
                  fluidRow(
                    class="page1_col2_graphics_row",
                  column(6,
                  class = "page1_col page1_col2_middle_left",
                  # tags$h3("National Plot Title"),
                  tags$div(class = "page1_title",
                    uiOutput("textNationwideTitle")
                  ), 
                  tags$div(class="NationalMapContainer",
                           style="position:relative;width: 100%;left: 0;",
                  tags$img(
                    id = "national_map_new",
                    class = "landing_page_map",
                    src = "Despair/1.png",
                    width="100%",
                    style = "bottom: 0; left:0;"
                    )
                  ) # End of Image DIV container
                  ), # End of Middle inner Column
                  column(6,
                         class = "page1_col page1_col2_middle_right",
                         tags$div(class = "page1_title",
                          uiOutput("textInfographicTitle")
                          ),
                         plotOutput("nation_state_infographic")
                  )
                )
                )
                , # End of inner Fluid Row (Column 2 Middle)
                fluidRow(
                  class = "page1_col page1_col2_bottom",
                  uiOutput("textMortFactsNew")

                  ) # Close inner FluidRow (Column 2 Bottom)
                ) #Close Column 2
              ) #Close Outter Row (National Map Page)

      ), # Close div tag "slide"
      
##################### PAGE 2, INDIVIDUAL STATE ANALYSIS #####################

      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        fluidRow(
          class = "page page2",
            column(7,
                   class="page2_col page2_col1",
                   fluidRow(
                     class="page2_col page2_col1_top",
                       column(4,
                              class = "page2_col page2_col1_top_left",
                              # tags$div(
                              #   title="The mortality rate used in MortalityMinder is the number of people per 100,000 that died prematurely in a given county during a three year period. A premature death is considered anyone that dies between the ages of 25 to 64 as a result of the selected cause.",
                              #   tags$h2("Exploring Causes of Premature Death",  icon("info-circle"))
                              #         ), # End of Heading Conrainer
                              uiOutput("textDescription")
                              
                             ), # End of inner Column (Column 1 Top Left)
                       column(8,
                              class = "page2_col page2_col1_top_right",
                              tags$div(
                                class="page2_col1_top_right_title",
                                uiOutput("textMortRates")
                                      ), # End of title div container
                              radioButtons("year_selector",
                                           #label = "Click on time period to select state map for that period",
                                           label = NULL,
                                           selected = "2015-2017",
                                           choiceNames = c("2000-02", "2003-05", "2006-08", "2009-11", "2012-14", "2015-17"),
                                           choiceValues = c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014", "2015-2017"),
                                           inline = TRUE),
                              leafletOutput("geo_mort_change2",width="82%",height="80%")
                              ) # End of inner Column (Column 1 top right)
                     
                   ), # End of inner FluidRow (Column1 Top)
                   tags$div(
                     class = "hr"
                           ),
                   fluidRow(
                     class = "page2_col page2_col1_bot",
                         column(5,
                           class = "page2_col page2_col1_bot_left",
                           tags$div(
                             class="page2_col1_bot_left_title",
                             uiOutput("textClusterGeo")
                                   ), # End of title div container
                             leafletOutput("geo_cluster_kmean",width="100%",height="80%")
                               ), # End of inner Column (Bottom Left)
                         column(5, 
                           class = "page2_col page2_col1_bot_right", 
                           tags$div(
                             class="page2_col1_bot_right_title",
                             uiOutput("textDeathTrends")
                                   ), # End of title div container
                             plotOutput("mort_line",width="100%",height="80%")
                                ) # End of inner Column (Bottom Right)
                     
                            ) #End of inner fluidRow (Column 1 Bottom)
                  ), # End of Column 1
            column(3,
              class = "page2_col page2_col2",
              tags$div(
                class = "page2_col2_title",
                uiOutput("textDeterminants")
                      ), # End of title container

              tags$div(
                class = "page2_col2_plot",
                plotOutput("page1.bar.cor1",width="100%",height="100%", 
                           # hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                           hover = hoverOpts("plot_hover"),
                           click = clickOpts("page1_bar_plot_click")),
                uiOutput("hover_info")
                      ) # End of plot div container
                  ) # End of Column 2
                )# End of FluidRow (Page1, State Analysis) 
      ), # End of slide div tag

##################### PAGE 3, INDIVIDUAL DETERMINANT ANALYSIS #####################
      
      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
        fluidRow(
          class = "page page3",
          column(3,
            class = "page3_col page3_col1",
            tags$div(
              class = "col1_title",
              uiOutput("textDeterminants2")
                    ), # End title div container
            plotOutput("determinants_plot1", height = "100%", width = "100%",
                       click = clickOpts("page2_bar_plot_click"))
                ), # End Column 1
          
          tags$div(
            class = "vl"
                  ),
          column(3,
            class = "page3_col page3_col2",
            
            fluidRow(
              class = "page3_col2_top",
              uiOutput("textBoxplotTitle"),
              plotOutput("determinants_plot2",width="100%",height="85%")
                    ), #End of Column 2 Top
            
            tags$div(class = "hr"),
            
            fluidRow(
              class = "page3_col2_bot",
              style = "position: relative",
              uiOutput("textScatterplotTitle"),
              uiOutput("determinants_plot3_county_name"),
              plotOutput("determinants_plot3",width="100%",height="85%",
                         click = clickOpts("determinants_plot3_click"), hover = hoverOpts("determinants_plot3_hover"))
            ) # End of Column 2 Bottom
          ), # End of Column 2
          tags$div(
            class = "vl"
                  ),
          
          column(3,
            class = "page3_col page3_col3",
            tags$div(
              tags$div(
                class = "prompt_text",
                "Select a factor:"              
              ),
              pickerInput(
                inputId = "determinant_choice",
                label = "Selected Determinant: ",
                choices = str_sort(chr.namemap.2019[intersect(colnames(chr.data.2019), rownames(chr.namemap.2019)),]),
                selected = "Socio-Economic",
                width = "100%",
                inline = TRUE,
                options = list(
                  `live-search` = TRUE,
                  "dropup-auto" = TRUE
                              ) # End of Options
                          ) # End of pickerInput
                    ), # End of pickerInput container
            
            fluidRow(
              class = "page3_col3_top",
              tags$br(),
              tags$p(htmlOutput("determinant_text")),
              tags$h5(uiOutput("determinant_link")),
              tags$h5(htmlOutput("determinant_original_source")),
              tags$h5(htmlOutput("determinant_corr")),
              tags$h5(htmlOutput("determinant_dir"))
                    ), # End of Column 3 top
            fluidRow(
              class = "page3_col3_bot",
              tags$div(
                tags$div(
                  class = "prompt_text",
                  uiOutput("textCountyPrompt")              
                ),
                uiOutput("county_selector")
              ), # End of pickerInput container
              fluidRow(
                class = "page3_col3_county_desc",
                uiOutput("county_desc")
              ),
              
              leafletOutput("determinants_plot5")
            ) # End of inner Column 3 bottom

            ) # End of Column 3
                ) # End of Fluid Row
      ), # End of Page 3
##################### PAGE 4, ABOUT PAGE #####################

      tags$div(
        class = "slide",
        tags$div(
          class = "nav_bar_blank"
        ),
          fluidRow(
                  class = "page page4",
                   column(3, tags$h4("ABOUT MORTALITYMINDER",align="center"),
                          fluidRow(
                            column(11, 
                                   tags$h5("Mortality rates are rising in the United State with significant 
                                           community and regional variations. MortalityMinder (MM) is a web-based 
                                           visualization tool that enables interactive exploration of social, 
                                           economic and geographic factors associated with premature mortality 
                                           among mid-life adults ages 25-64 across the United States. Using 
                                           authoritative data from the CDC and other sources, MM is a publicly-available, 
                                           open source, easily maintained tool designed to help health policy 
                                           decision makers in the public and private sectors identify and 
                                           address unmet healthcare needs, healthcare costs, and healthcare utilization.")), 
                            column(11, tags$h4("DATA UTILIZATION AND PREPARATION",align="center"), 
                                   tags$h5("MortalityMinder uses county-level mortality rates and social and 
                                           economic factors measurements available from well-known public portals.  
                                           Mortality rates  from 2000-2017 are obtained through the CDC WONDER portal, 
                                           the definitive source of mortality information in the United States. Social 
                                           factors data  for 2015-2017 are obtained through County Health Rankings (CHR), 
                                           an aggregate of county level data curated by the Robert Wood Johnson Foundation. 
                                           MM considers 168 factors from twenty (20) sources, including 
                                           datasets such as AHRQ, BRFSS, the Bureau of Labor Statistics, the FBI, and many 
                                           others.  This version of MM focuses on midlife deaths attributed to leading 
                                           causes of death including 'Deaths of Despair', 'CardioVascular', 'Cancer' and 
                                           'All Causes',  but the approach can be readily generalized to any health problem 
                                           of interest."), 
                                   tags$h5("Age-specific mortality rates were calculated in three-year chunks to 
                                           allow more robust estimation by considering multiple years. MM aims to capture 
                                           the actual experience of mortality by Americans in a community, so our analysis 
                                           does not age-adjust. In the future, age-adjustment could easily be added to MM. 
                                           To provide for more complete data for effective visualizations, county mortality 
                                           rates that are suppressed to preserve privacy by CDC WONDER were imputed using 
                                           the Amelia package in R. Multiple imputation could be added to the analysis 
                                           in the future. Details of data sources and preparation are available at ",
                                           a("the MortalityMinder github wiki.",
                                             href="https://github.com/TheRensselaerIDEA/MortalityMinder/wiki"
                                             , target="_blank"))),
                            column(11, tags$h4("DOWNLOAD SOURCE DATA",align="center"),
                                   downloadButton("downloadCDCData", "County Deathrate Data"), tags$br(),
                                   downloadButton("downloadCHRData", "County Health Rankings (CHR) Factor Data"), tags$br(),
                                   downloadButton("downloadFactorDesc", "Factor Descriptions"), tags$br(),
                                   tags$h4("DOWNLOAD CURRENT RESULTS",align="center"), 
                                   downloadButton("downloadClusters", "Current State Clusters"), tags$br(),
                                   downloadButton("downloadClusterTime", "Current State Clusters Through Time"), tags$br(),
                                   downloadButton("downloadCorr", "Current Factor Correlations"))
                          ) # Close row
                   ), #close column
                   column(3, tags$h4("INNOVATION",align="center"),   offset=1,
                          fluidRow(
                            column(11, tags$h5("MortalityMinder enables users to dynamically investigate county and regional 
                                               variations within mortality trends at the national, state, and local levels 
                                               and the social determinants and other factors associated with these trends.  It 
                                               then prioritizes and visualizes the most significant factors associated with 
                                               higher risks for each cause of death , MM allows the user to explore 
                                               individual factors include its relation to the selected cause at a county level 
                                               for each state, and the distribution of those factors within each state."), 
                                   tags$h5("MM allows the user to visually explore the growth of mortality rates in the United 
                                           States using county level three-year averages from 2000-2017, providing 
                                           both national and state views. For each cause of death, MM clusters counties in 
                                           each state, categorizing each county into high/medium/low mortality clusters; 
                                           visualizes the geographic distribution of those clusters; and illustrates the 
                                           mortality trends of those clusters and user-selected counties over time.  MM does
                                           a similar analysis for the entire United States, only using six clusters."),
                                   tags$h5("The app determines the correlation of social factors and visualizes the most 
                                           significant, highlighting whether those factors are protective or destructive. 
                                           Users can select individual factors to explore more deeply at the state level."),
                                   tags$h5("Demonstrations and user testing by our advisory panel of health care experts and 
                                           other testers have shown that MortalityMinder is compelling and highly engaging. The 
                                           page-oriented organization of the app lends itself to user-driven investigation and 
                                           storytelling, like a highly interactive slideshow.")),
                            column(11, tags$h4("INSIGHTS",align="center"), 
                                   tags$h5("MortalityMinder provides striking insights into the disturbing rise in 'Deaths 
                                           of Despair' due to suicide, overdose, and self-harm. The app was inspired by 
                                           'Mortality and morbidity in the 21st century' (Case and Dean, 2017) which described 
                                           the disturbing rise in mortality rates by 'deaths of despair.'  The app highlights 
                                           great disparities in mortality rates between counties across and within 
                                           states with both common and distinct associated factors. Such an insight can be used to 
                                           create regional specific interventions and determine best practices."), 
                                   tags$h5("MortalityMinder further provides a county-level confirmation of the trends and 
                                          hypothesized causes  in 'Life Expectancy and Mortality Rates in the United States, 
                                          1959-2017' (Woof and Schoomaker 2019). MM shows, however, that rates due to other causes 
                                          including cancer, cardiovascular disease and all causes are also on the rise. MM confirms the midlife 
                                          mortality rate increases in (Woolf and Schoomaker), but it provides much greater insight 
                                          into community level variations and their associated factors that may be actionable.")) 
                   ), 
                   fluidRow(class="IDEA_Logo_Wrapper",
                            tags$img(
                              class="Idea_Logo",
                              src="IDEA_logo_500.png", 
                              width="100%", 
                              style="bottom: 0; left: 0;")
                   )
                   ), # Close column
                   column(3, tags$h4("DESIGN AND DEVELOPMENT",align="center"),  offset=1,
                          fluidRow(
                            column(11, tags$h5("The focus of MortalityMinder development July-December 2019 
                                               was on data source identification for multiple causes, beyond 
                                               our Phase 1 example of 'Deaths of Despair'; social factor selection;
                                               refinement of our analytic techniques; expansion of our visualization 
                                               choices; implementation of our interactive, web-based framework; and 
                                               refinement of the user experience, with input from our external experts, 
                                               student graphic designers, and a formal usability study."), 
                                   tags$h5("The team's overarching development objective, beyond analytical credibility, 
                                           was to provide a compelling and engaging tool for health policy and other 
                                           decision makers. MortalityMinder can be used as an investigative tool, but 
                                           may also be used for interactive storytelling, e.g. for lectures on mortality 
                                           across the United States.")),
                            column(11, tags$h4("IMPLEMENTATION AND DEPLOYMENT",align="center"), 
                                   tags$h5("MortalityMinder is currently published via two publicly-accessible web locations. 
                                           Our open-source R code is freely available via a github repository. Source 
                                           data and generated results may be downloaded from within the app. MM is 
                                           coded using the R language and environment for statistical computing and graphics, 
                                           incorporating best practices and using well-known packages whenever possible. 
                                           Data Loader scripts enable additional years and types of data to be easily incorporated.  
                                           The result is a robust, extensible package that can be maintained and grown over 
                                           time as either an open source package or within organizations such as AHRQ."),
                                   tags$h5("MM can be run from the public web locations; no user installation 
                                           is required to test the application. Alternatively, the ",
                                           a("github repository",
                                             href="https://github.com/TheRensselaerIDEA/MortalityMinder/", target="_blank"), 
                                           "may be cloned and the MM then run immediately in the user's RStudio environment, 
                                           either on a server or on a personal machine."), 
                                   tags$h5("MM utilizes the R Shiny platform for web interactivity; most of the visualizations
                                           presented in MM are generated in real time based on data loaded when the app is launched. 
                                           Data analysts and software engineers familiar with the R language and reactive coding 
                                           via Shiny will have no problem customizing the code as required. MM's 
                                           overall web layout utilizes a standard Javascript UI framework which is easily 
                                           customized to modify the overall style of the app.")),
                          column(11, tags$h4("ACKNOWLEDGEMENTS", align = "center"), 
                                 tags$h5("MortalityMinder was created by undergraduate participants in the Data INCITE Lab at Rensselaer 
                                    Polytechnic Institute with support from the United Health Foundation and the 
                                    Rensselaer Institute for Data Exploration and Applications. MortalityMinder was directed by 
                                    Kristin P. Bennett and John S. Erickson."),
                                 tags$h5("The MortalityMinder Team would like to thank our team of external mentors, including
                                         Anne Yau, United Health Foundation; Dan Fabius, Continuum Health; Melissa Kamal, New York State
                                         Department of Health; and Tom White, Capital District Physicians' Health Plan (CDPHP)."),
                                 # tags$h5("The MortalityMinder Team would also like to thank Jim Hendler, Director, The Rensselaer IDEA,
                                 #    and Curt Breneman, Dean of the School of Science at Rensselaer for their support
                                 #    and encouragement."),
                                 tags$h5("Please send questions and comments about MortalityMinder to: erickj4@rpi.edu.")
                                 ),
                          column(11, tags$h4("LINKS", align = "center"), 
                                 tags$h5(a("MortalityMinder GitHub Repository (public)",
                                             href="https://github.com/TheRensselaerIDEA/MortalityMinder/", target="_blank")),
                                 tags$h5(a("MortalityMinder GitHub Wiki (public)",
                                           href="https://github.com/TheRensselaerIDEA/MortalityMinder/wiki", target="_blank"))
                          )
                   ) # Close inner fluidRow
          )
        ) # Close outter fluidRow
        ) # Close Page 4
      )
    ),
  tags$script(src = "jquery-ui.min.js"),
  tags$script(src = "fullpage.js"),
  tags$script(src = "jquery.ba-outside-events.js"),
  includeScript(path = "myscript.js")
  )



##################### Server Code #####################

server <- function(input, output, session) {
  county_choice <- reactiveVal()
  
  mort.rate <- reactive({
    county_choice(NULL)
    assign("county_polygon", NULL, envir = .GlobalEnv)
    assign("page1_period_choice", 6, envir = .GlobalEnv)
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
  
  mort.rate.original <- reactive({
    county_choice(NULL)
    assign("county_polygon", NULL, envir = .GlobalEnv)
    assign("page1_period_choice", 6, envir = .GlobalEnv)
    if(input$state_choice == "United States"){
      cdc.unimputed.data %>% dplyr::filter(
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
      cdc.unimputed.data %>% dplyr::filter(
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
    
    kendall.cor.new <- mort.rate.original() %>% 
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
  
  
  # Return the mean mortality rate for a state  for 2000-2002
  state.mean.2000_2002 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                    State == names(state.list)[which(state.list == input$state_choice)],
                    Cause == input$death_cause,
                    Years == '2000-2002')$Crude.Rate)
  })
    
  
  # Calculate national mean mortality for 2000-2002
  national.mean.2000_2002 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == 'United States',
                             Cause == input$death_cause,
                             Years == '2000-2002')$Crude.Rate)
  })
  
  #Calculate the mean mortality rate for a state  for 2015-2017
  state.mean.2015_2017 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == names(state.list)[which(state.list == input$state_choice)],
                             Cause == input$death_cause,
                             Years == '2015-2017')$Crude.Rate)
  })
  
  # Calculate national mean mortality for 2015-2017
  national.mean.2015_2017 <- reactive({
    as.numeric(dplyr::filter(state_natl_death_rates,
                             State == 'United States',
                             Cause == input$death_cause,
                             Years == '2015-2017')$Crude.Rate)
  })
  
  # finds states with lowest and highest death rates and returns them
  #   and their respective rates
  #
  # Returns a list in form (lowest death rate, lowest death rate state,
  #   highest death rate, highest death rate state)
  low.high.states.2015_2017 <- reactive({
    
    grouped.data <- dplyr::filter(
        state_natl_death_rates,
        State != 'United States',
        State != 'District of Columbia',
        Cause == input$death_cause,
        Years == "2015-2017"
      )
    
    return(
      c(
        min(as.numeric(grouped.data$Crude.Rate)),
        grouped.data$State[which.min(as.numeric(grouped.data$Crude.Rate))],
        max(as.numeric(grouped.data$Crude.Rate)),
        grouped.data$State[which.max(as.numeric(grouped.data$Crude.Rate))]
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
           },
           "All Cause" = {
             death_rate <- c(366.07178, 373.10366, 373.65807, 373.40143, 379.60383, 395.93077)
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
  
  determinant.source <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Source"))
  })

  determinant.source_url <- reactive({
    return(as.character(
      SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Source_url"))
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
  output$county_selector <- renderUI({
    if (input$state_choice != "United States") {
      pickerInput('county_drop_choice', 
                  'County', 
                  geo.namemap[geo.namemap$state_abbr == input$state_choice,]$county_name,
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(size = 15,
                                          maxOptions = 1)
      )
    }
  })
  
  rv_county_drop_choice <- reactive({})
  
  county_event <- observeEvent(input$county_drop_choice, {
    rv_county_drop_choice <- input$county_drop_choice 
    county_choice(paste0(rv_county_drop_choice, " County"))
  },
  ignoreInit = TRUE
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
    else if(input$death_cause == "All Cause"){
      includeScript(path = "All.js")
    }
  })
  
  output$determinants_plot1 <- renderPlot({
    
    # Sort by kendall.cor
    # TODO: Cache this; don't re-generate! 
    kendall.cor.new <- kendall.cor() %>% 
      dplyr::filter(kendall_p < 0.1) %>% 
      dplyr::arrange(desc(kendall_cor)) %>% 
      dplyr::top_n(15, abs(kendall_cor)) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    assign("kendall_cor_new", kendall.cor.new, envir = .GlobalEnv)
    
    #Only display the social determinants graph if there is any significant social determinant
    #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
    # browser()
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
          color = "#565254", 
          size = 4
        ) +
        geom_text(
          aes(label = round(kendall_cor, 2)), 
          color = "#565254", 
          size = 3
        ) +
        
        # Coordinates
        coord_flip() + 
        scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
        
        # Themes
        geom_hline(yintercept = .0, linetype = "dashed") + 
        labs(
          # title = "Most Associated Factors",
          # subtitle = "Kendall Correlation between Factors and Mortality Risk Cluster\nClick dot for details",
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA",
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
  }, bg="transparent")
  
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
    
    res <- cdc.unimputed.data %>% dplyr::filter(period == "2015-2017",
                                        death_cause == input$death_cause,
                                        state_abbr == input$state_choice,
                                        death_num != 0.5)

    res <- dplyr::inner_join(mort.cluster.ord(), res, by = 'county_fips')
    
    sd.select <- chr.data.2019 %>% 
      dplyr::select(county_fips, VAR = sd.code) %>% 
      dplyr::right_join(mort.cluster.ord(), by = "county_fips") %>% 
      dplyr::inner_join(geo.namemap, by = "county_fips") %>% 
      tidyr::drop_na()
    
    if (nrow(sd.select) <= 6){
      
      ggplot(sd.select, aes(x = cluster, y = VAR, fill = cluster)) + 
        geom_boxplot() +
        labs(y = input$determinant_choice, 
             caption = "Plot will show only single values if the state has too few counties to cluster (6 or fewer). \n In these cases, the x-axis is individual counties rather than clusters.") + 
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
        # ggtitle(paste(input$determinant_choice, "and Risk County Relationship"))+
        scale_fill_manual(
          name = "County",
          labels = sd.select$county_name,  
          values = colorRampPalette(
            c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
          )(max(sd.select$cluster))
        )
      
    } else if (input$state_choice == "United States") {
      
      sd.select$cluster[sd.select$cluster == 1] <- "1: Low"
      sd.select$cluster[sd.select$cluster == 6] <- "6: High"
      
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
          rect = element_blank(),
          legend.position = "none"
        ) + 
        labs(
          x = "Cluster",
          y = input$determinant_choice,
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
         
        ) + 
        # ggtitle(paste(input$determinant_choice, "and Risk Cluster Relationship"))+
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
      
    } else {
      
      sd.select$cluster[sd.select$cluster == 1] <- "1: Low"
      sd.select$cluster[sd.select$cluster == 2] <- "2: Medium"
      sd.select$cluster[sd.select$cluster == 3] <- "3: High"
      
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
          rect = element_blank(),
          legend.position = "none"
        ) + 
        labs(
          x = "Cluster",
          y = input$determinant_choice,
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
          
        ) + 
        # ggtitle(paste(input$determinant_choice, "and Risk Cluster Relationship"))+
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
    }
    
  }, bg = "transparent")
  
  
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
        cdc.unimputed.data,
        period == "2015-2017", 
        death_cause == input$death_cause,
        death_num != 0.5
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na() %>%
        
        
        ggplot(aes(x = death_rate, y = VAR)) + 
        geom_point(aes(fill = cluster)) + 
        labs(
          x = "Midlife Mortality Rate (2015-2017)",
          y = input$determinant_choice,
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        ) + 
        # ggtitle(paste(input$determinant_choice, "and Mortality Relationship")) +
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
        #geom_point(colour="#565254", shape=21, size = 3, alpha = .7,
        #aes(fill = cluster)) + 
        stat_density_2d(aes(alpha = ..level.., fill=cluster), geom = "polygon") +
        labs(
          x = "Midlife Mortality Rate (2015-2017)",
          y = input$determinant_choice,
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        ) + 
        # ggtitle(paste(input$determinant_choice, "and Mortality Relationship"))+
        theme.line.mort() + 
        theme(legend.position = "top") + 
        guides(color = guide_legend(override.aes = list(shape = 15))) + 
        color.line.cluster(input$state_choice, max(sd.select$cluster)) +
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
    } else {
      
      data <- dplyr::filter(
        cdc.unimputed.data,
        period == "2015-2017", 
        death_cause == input$death_cause,
        death_num != 0.5
      ) %>% 
        dplyr::select(county_fips, death_rate) %>% 
        dplyr::inner_join(sd.select, by = "county_fips") %>% 
        tidyr::drop_na()

      data$cluster[data$cluster == 1] <- "1: Low"
      data$cluster[data$cluster == 2] <- "2: Medium"
      data$cluster[data$cluster == 3] <- "3: High"
      
      plot <- data %>%  
        ggplot(aes(x = death_rate, y = VAR)) + 
        geom_point(colour="#565254", shape=21, size = 3, alpha = .7,
                   aes(fill = cluster)) + 
        #stat_density_2d(aes(alpha = ..level.., fill=cluster), geom = "polygon") +
        labs(
          x = "Midlife Mortality Rate (2015-2017)",
          y = input$determinant_choice,
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        ) + 
        # ggtitle(paste(input$determinant_choice, "and Mortality Relationship"))+
        theme.line.mort() + 
        theme(legend.position = "top") + 
        guides(color = guide_legend(override.aes = list(shape = 15))) + 
        color.line.cluster(input$state_choice, max(sd.select$cluster)) +
        scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
      
      if (is.null(county_choice())){
        plot
      }else{
        county_data <- dplyr::filter(
                        data,
                        county_name == substr(county_choice(), 0, nchar(county_choice())-7)
                      )
        
        if (nrow(county_data) == 0) {
          plot + xlab("Midlife Mortality Rate (2015-2017)\nCould not plot county as data suppressed by CDC")
        } else {
          plot + 
            geom_point(
              mapping = aes(x = death_rate, y = VAR, group = county_name, shape = county_choice()),
              data = county_data, color="#565254", size = 5, alpha = .7, inherit.aes = FALSE
            ) + 
            scale_shape_manual(name = "County",
                               values = c(18), 
                               guide = guide_legend(override.aes = list(color = c("#565254")))
            )
        }
      }
    }
  }, bg = "transparent")

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
    
      if(input$state_choice == "United States"){
      # If "United States" suppress plot
      # sd.data <- dplyr::filter(
      #   cdc.data,
      #   period == "2015-2017", 
      #   death_cause == input$death_cause
      # ) %>% 
      #   dplyr::select(county_fips, death_rate) %>% 
      #   dplyr::inner_join(sd.select, by = "county_fips") %>% 
      #   tidyr::drop_na() 
      #   
      #   geo.sd.plot("US", input$determinant_choice, sd.data, "2015-2017")
        
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
  

  output$determinant_text <- renderUI({
    reason_text <- ""
    if (!is.na(SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Reason")) {
      reason_text <- as.character(
        SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Reason"
      )
    }
    
    tagList(
      tags$h3(
        as.character(
          SocialDeterminants[SocialDeterminants$Name == input$determinant_choice,]$"Definitions")
      ),
      tags$h4(reason_text)
    )
  })
  
  output$determinant_link <- renderUI({
    tagList(tags$h4(
      "Text Source: ",
      tags$a(
        "County Health Rankings",
        href = determinant.url(),
        target="_blank"
      )
    )
    )
  })
  
  output$determinant_original_source <- renderUI({
    tagList(tags$h4(
      "Data Source: ",
      tags$a(
        determinant.source(),
        href = determinant.source_url(),
        target="_blank"
      )
    )
    )
  })
  
  output$determinant_corr <- renderText({
    if (nrow(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]) == 0) {
      return("")
    }
    
    if (kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_cor >= 0) {
      return(paste0("Kendal Correlation with ",
                    input$death_cause,
                    " mortality: <span style=\"color:	#f8766d\"> <strong> ",
                    round(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_cor, 4),
                    "</strong> </span>"))
    }
    else {
      return(paste0("Kendal Correlation with ",
                    input$death_cause,
                    " mortality: <span style=\"color: #00bfc4\"> <strong>",
                    round(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_cor, 4),
                    "</strong> </span>"))
    }
  })
  
  output$determinant_dir <- renderText({
    if (nrow(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]) == 0) {
      return("")
    }
    
    if (kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_p > .05) {
      return(paste0("<strong>No</strong> statistically significant ",
                    " relationship with mortality (p-value = ",
                    signif(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_p, 2),
                    ")"))
    }
    else if (kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_cor >= 0) {
      return(paste0("Statistically significant <strong> <span style=\"color:	#f8766d\">",
                      tolower(as.character(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$DIR)),
                      "</span> </strong> relationship with mortality (p-value = ",
                      signif(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_p, 2),
                      ")"))
    }
    else {
      return(paste0("Statistically significant <strong> <span style=\"color:	#00bfc4\">",
                    tolower(as.character(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$DIR)),
                    "</span> </strong> relationship with mortality (p-value = ",
                    signif(kendall.cor()[kendall.cor()$chr_code == input$determinant_choice,]$kendall_p, 2),
                    ")"))
    }
  })
  
  # Gives information about county population and urbanness
  output$county_desc <- renderUI({
    
    # when app starts up there is initially no selection
    if (is.null(input$county_drop_choice)) {
      return()
    }
    
    county.data.00.02 <- dplyr::filter(
      cdc.unimputed.data,
      county_name == input$county_drop_choice,
      death_cause == input$death_cause,
      state_abbr == input$state_choice,
      period == "2000-2002",
      death_num != 0.5
    )
    county.data.15.17 <- dplyr::filter(
      cdc.unimputed.data,
      county_name == input$county_drop_choice,
      death_cause == input$death_cause,
      state_abbr == input$state_choice,
      period == "2015-2017",
      death_num != 0.5
    )
    
    if (nrow(county.data.15.17) == 0 | nrow(county.data.00.02) == 0) {
      return(
        tagList(
          tags$h5(paste0(
            "Data suppressed for ", input$county_drop_choice, ", ", input$state_choice, " by CDC")
          )
        )
      )
    }
    
    # pop change
    pop.00.02 <- county.data.00.02$population
    pop.15.17 <- county.data.15.17$population
    
    pop.change.text <- "Population has remained relatively constant since 2002"
    
    if (pop.00.02 > pop.15.17) {
      pop.change.text <- paste0("Population fell by ", 
                                formatC(pop.00.02 - pop.15.17, format="d", big.mark=","),
                                " (",
                                round((pop.00.02 - pop.15.17) / pop.00.02 * 100, 2),
                                "%) from 2002 to 2017")
    }
    if (pop.00.02 < pop.15.17) {
      pop.change.text <- paste0("Population rose by ",
                                formatC(pop.15.17 - pop.00.02, format="d", big.mark=","),
                                " (",
                                round((pop.15.17 - pop.00.02) / pop.00.02 * 100, 2),
                                "%) from 2002 to 2017")
    }
    
    # death rate change
    dr.00.02 <- county.data.00.02$death_rate
    dr.15.17 <- county.data.15.17$death_rate
    
    dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                             " mortality rate has remained relatively constant since 2002 at ",
                             round(dr.15.17, 2),
                             "per 100k people")
    
    if (dr.00.02 > dr.15.17) {
      dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                                " mortality rate fell by ",
                                round((dr.00.02 - dr.15.17) / dr.00.02 * 100, 2),
                                "% from 2002 to 2017 from ",
                                round(dr.00.02, 2), " to ",  round(dr.15.17, 2))
    }
    if (dr.00.02 < dr.15.17) {
      dr.change.text <- paste0("For ", county.data.15.17$county_name, " County, the ", tolower(input$death_cause), 
                                " mortality rate rose by ",
                                round((dr.15.17 - dr.00.02) / dr.00.02 * 100, 2),
                                "% from 2002 to 2017 from ",
                                round(dr.00.02, 2), " to ",  round(dr.15.17, 2))
    }
    
    return(
      tagList(
        # tags$h5(paste0(
        #   county.data.15.17$county_name, ", ", county.data.15.17$state_abbr,
        #   " is a ", tolower(county.data.15.17$urban_2013), " area with a population of ",
        #   formatC(pop.15.17, format="d", big.mark=","))
        # ),
        # tags$h5(pop.change.text),
        # TODO: Add determinant change!
        tags$h5(dr.change.text)
      )
    )
  })
  
  output$determinants_plot4 <- renderPlot({
    
  })
  
  # Mortality Rate Trend Line Graph
  output$mort_line <- renderPlot({
    
    if (input$state_choice == "United States"){
      
      total.data <- mort.avg.cluster.ord()
      total.data$cluster[total.data$cluster == 1] <- "1: Low"
      total.data$cluster[total.data$cluster == 6] <- "6: High"
      
      ggplot(
        total.data,
        aes(
          x = period, y = death_rate, 
          color = cluster, group = cluster
        )
      ) + 
        geom_line(size = 1.5) + 
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        # labs.line.mort(input$state_choice, input$death_cause) + 
        scale_color_manual(
          values = theme.categorical.colors.accent(max(mort.cluster.ord()$cluster))) +
        theme.line.mort() + 
        theme(legend.position = "left") + 
        ylab("Average Midlife deaths per 100,000") +
        labs(
          fill = "Cluster", 
          color = "Cluster",
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        ) +
        guides(
          color = guide_legend(reverse = T)
        )
    } else {

      nclusters <- max(mort.cluster.raw()$cluster)
      total.data <- rbind(mort.avg.cluster.ord(), national.mean())
      
      if (input$state_choice == "DE") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        

        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Kent", "New Castle", "Sussex", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife deaths per 100,000")
        line_plot
        
      } else if (input$state_choice == "HI") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        
        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Kalawao", "Honolulu" , "Maui", "Hawaii", "Kauai", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife deaths per 100,000")
        line_plot
        
      } else if (input$state_choice == "RI") {
        
        exceptions.data <- cdc.data %>%
          dplyr::filter(death_cause == input$death_cause) %>%
          dplyr::right_join(mort.cluster.raw(), by = "county_fips")
        exceptions.data$cluster <- exceptions.data$county_name
        exceptions.data <- exceptions.data %>%
          dplyr::group_by(period, cluster) %>%
          dplyr::summarise(
            death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
            count = n()
          ) %>%
          dplyr::ungroup()
        exceptions.data <- rbind(exceptions.data, national.mean())
        extras.color <- rbind(mort.avg.cluster.ord(), national.mean())
        colnames(extras.color)[2] <- "cluster.num"
        exceptions.data <- cbind(exceptions.data, extras.color$cluster.num)
        colnames(exceptions.data)[5] <- "cluster.num"
        
        line_plot <- ggplot(
          exceptions.data,
          aes(
            x = period, y = death_rate, 
            color = cluster.num, group = cluster.num
          )
        ) + 
          geom_line(size = 1.5) + 
          geom_point(color = "black", shape = 21, fill = "white", size = 2) + 
          # labs.line.mort(input$state_choice, input$death_cause) + 
          scale_color_manual(
            values = c(theme.categorical.colors(nclusters), "#0571b0"), labels = c("Bristol", "Washington", "Newport", "Kent", "Providence", "National")) +
          theme.line.mort() + 
          theme(legend.position = "left") + 
          guides(color = guide_legend(reverse = T)) +
          labs(fill = "Counties and \n National Average", 
               color = "Counties and \n National Average",
               caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
          ) + 
          ylab("Average Midlife deaths per 100,000")
        line_plot 
        
      } else {

        total.data$cluster[total.data$cluster == 1] <- "1: Low"
        total.data$cluster[total.data$cluster == 2] <- "2: Medium"
        total.data$cluster[total.data$cluster == 3] <- "3: High"
        # total.data$cluster <- as_factor(total.data$cluster)

        # total.data$cluster_label[total.data$cluster == "1"] <- "Low"
        # total.data$cluster_label[total.data$cluster == "2"] <- "Medium"
        # total.data$cluster_label[total.data$cluster == "3"] <- "High"
        # total.data$cluster_label[total.data$cluster == "National"] <- "National"
        # total.data$cluster_label <- as_factor(total.data$cluster_label)

      line_plot <- ggplot(
        total.data,
        aes(
          x = period, y = death_rate, color = cluster,
          group = cluster
        )
      ) + 
        geom_line(size = 1.5) + 
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        # labs.line.mort(input$state_choice, input$death_cause) + 
        scale_color_manual(
          values = theme.categorical.colors.accent(nclusters)) +
        theme.line.mort() + 
        theme(legend.position = "left") + 
        guides(color = guide_legend(reverse = T)) +
        labs(fill = "Cluster", 
             color = "Cluster",
             caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        ) + 
        ylab("Average Midlife Deaths per 100,000") 
      
      if (is.null(county_choice())){
        line_plot
      } else {
        drop.cols <- c('county_fips')
        county_data <- cdc.countymort.mat(cdc.data, input$state_choice, county_choice(), input$death_cause)
        
        canShow <- dplyr::inner_join(county_data, cdc.unimputed.data, by = 'county_fips') %>% 
          dplyr::filter(
            death_cause == input$death_cause
          )
        
        if (any(canShow$death_num == 0.5) | nrow(county_data) == 0) {
          line_plot + xlab("period\nCould not plot county as data suppressed by CDC")
        } else {
            county_data <- county_data %>%
              dplyr::select(-drop.cols) %>%
              tidyr::gather("period", "death_rate", "2000-2002":"2015-2017") %>%
              dplyr::mutate("county" = county_choice())
            line_plot + 
              geom_line(
                mapping = aes(x = period, y = death_rate, group = county, linetype=county_choice()),
                data = county_data, color = "#565254", size = 1.3
              ) +
              geom_point(
                mapping = aes(x = period, y = death_rate),
                data = county_data, color = "#565254", shape = 21, 
                fill = "#f7f7f7", inherit.aes = FALSE, size = 2
              ) +
              scale_linetype_manual(name = "County",
                                    values = c("twodash"),
                                    guide = guide_legend(override.aes = list(color = c("#565254")))
              )
        }
      }
      }
    }
    
  },bg="transparent")
  
  generate_text <- function(name, diff_pct){
    change_text <- paste0("The mortality rate \nhas ")
    
    if (diff_pct > 0) {
      change_text <- paste0(change_text, "increased ")
    }
    else {
      change_text <- paste0(change_text, "decreased ")
    }
    
    change_text <- paste0(change_text, "in \n", name, " by ", abs(round(diff_pct,1)), "%")
    change_text
  }
  
  generate_label_data <- function(state_data, nation_data, state_begin, state_end, nation_begin, nation_end, state_x, state_y, nation_x, nation_y){
    state_data <- state_data %>% 
      mutate(label = "") %>% 
      rename(x = period)
    nation_data <- nation_data %>% 
      mutate(label = "") %>% 
      rename(x = period)
    rbind(generate_label_data_single(state_data, input$state_choice, state_begin, state_end, state_x, state_y),
          generate_label_data_single(nation_data, "United States", nation_begin, nation_end, nation_x, nation_y))
  }
  
  generate_label_data_single <- function(data, name, begin, end, label_x, label_y){
    label_data = data.frame(data)
    label_data$x[label_data$x=="2000-2002"] <- 1
    label_data$x[label_data$x=="2003-2005"] <- 2
    label_data$x[label_data$x=="2006-2008"] <- 3
    label_data$x[label_data$x=="2009-2011"] <- 4
    label_data$x[label_data$x=="2012-2014"] <- 5
    label_data$x[label_data$x=="2015-2017"] <- 6
    label_data$x <- as.numeric(as.character(label_data$x))
    n <- 10
    d_max <- 0
    for (i in 1:5){
      r1 <- label_data[label_data$x==i,]
      r2 <- label_data[label_data$x==i+1,]
      for (j in 1:n-1){
        for (d in 0:d_max){
          label_data <- rbind(label_data, data.frame("x" = i+j/n,
                                                     "death_rate" = r2$death_rate*j/n+r1$death_rate*(n-j)/n-d,
                                                     "label" = c(""),
                                                     "group" = c(name)))
          label_data <- rbind(label_data, data.frame("x" = i+j/n,
                                                     "death_rate" = r2$death_rate*j/n+r1$death_rate*(n-j)/n+d,
                                                     "label" = c(""),
                                                     "group" = c(name)))
        }
      }
    }
    mort_diff <- (end - begin) / begin * 100
    mort_text <- generate_text(name, mort_diff) 
    
    rbind(label_data,
          data.frame("x" = label_x, 
                     "death_rate" = label_y,
                     "label" = mort_text,
                     "group" = name))
  }
  
  draw_reference <- function(line_plot, l_start, l_end, r_start, r_end){
    line_plot <- draw_reference_single(line_plot, l_start, l_end, 1, l_end)
    draw_reference_single(line_plot, r_start, r_end, 6, r_start)
  }
  
  draw_reference_single <- function(line_plot, start, end, x, y){
    line_plot +
      geom_segment(aes(x='2000-2002', xend='2015-2017', y=y, yend=y),
                   color = '#565254', linetype=2) +
      geom_segment(aes(x=x, xend=x, y=start, yend=end),
                   color = '#565254', linetype=1, arrow = arrow(length=unit(0.4,"cm")))
  }
  
  add_reference_point <- function(label_data, l_start, l_end, r_start, r_end){
    label_data <- add_reference_point_single(label_data, l_start, l_end, 1, "United States")
    add_reference_point_single(label_data, r_start, r_end, 6, "United States")
  }
  
  
  add_reference_point_single <- function(label_data, start, end, x, name){
    rbind(label_data, data.frame("x" = rep(c(x), times = 6),
                                 "death_rate" = seq(start, end, length.out = 6),
                                 "label" = rep(c(""), times = 6),
                                 "group" = rep(c(name), times = 6)))
  }
  
  # Mortality Rate Trend Line Graph
  output$nation_state_infographic <- renderPlot({

    u <- 0.65
    v <- 1 - u
    if (is.null(input$page1_period)){
      period_choice = 6
    } else {
      period_choice = input$page1_period
    }
    
    if (input$state_choice == "United States"){
      
      nation_data <- dplyr::filter(
        cdc.data,
        death_cause == input$death_cause
      ) %>% 
        drop_na() %>%
        group_by(period) %>% 
        summarise(population = sum(population), death_num = sum(death_num)) %>%
        mutate(death_rate = death_num/population*100000, group = "United States") %>%
        select(period, death_rate, group)
      
      nation_begin <- nation_data[nation_data$period=="2000-2002",]$death_rate
      nation_end <- nation_data[nation_data$period=="2015-2017",]$death_rate
      nation_hi <- max(nation_begin, nation_end)
      nation_lo <- min(nation_begin, nation_end)
      hi <- max(nation_data$death_rate)
      lo <- min(nation_data$death_rate)
      range <- hi - lo
      
      line_plot <- ggplot(
        nation_data,
        aes(
          x = period, y = death_rate, 
          group = group, color = color
        )
      ) + 
        geom_line(size = 1.5, color = theme.cat.accent.color()) + 
        geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
        theme.line.mort() + 
        theme(legend.position = "bottom", legend.title = element_blank()) + 
        ylab("Midlife deaths per 100,000") + 
        geom_segment(aes(x=period_choice, xend=period_choice, y=lo, yend=hi), color = '#38761D', linetype=2) +
        geom_polygon(data = data.frame(
          x = c(period_choice-0.1, period_choice+0.1, period_choice, period_choice-0.1), 
          y = c(hi+range*0.1, hi+range*0.1, hi, hi+range*0.1)), 
          aes(x = x, y = y), inherit.aes = FALSE, fill = '#38761D')
      
      nation_data <- nation_data %>% 
        mutate(label = "") %>% 
        rename(x = period)
      label_data <- generate_label_data_single(nation_data, "United States", nation_begin, nation_end, 1, v*nation_lo+u*nation_hi)
      label_data <- add_reference_point_single(label_data, nation_begin, nation_end, 1, "United States")
      line_plot <- draw_reference_single(line_plot, nation_begin, nation_end, 1, nation_end)
      line_plot <-line_plot +
        coord_cartesian(clip = "off") +
        geom_label_repel(data = label_data, mapping = aes(x = x, y = death_rate, label = label), 
                         fill = theme.cat.accent.color(),
                         inherit.aes = FALSE,
                         segment.colour = "#565254",
                         color = "#f7f7f7",
                         #hjust = "inward", vjust = "inward",
                         #point.padding = 0.5,
                         direction = "both",
                         xlim = c(1.5, 5.5),
                         show.legend = FALSE) +
        labs(
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        )
      
    } else {
      state_data <- dplyr::filter(
                      cdc.data,
                      state_abbr == input$state_choice,
                      death_cause == input$death_cause
                    ) %>% 
                    drop_na() %>%
                    group_by(period) %>% 
                    summarise(population = sum(population), death_num = sum(death_num)) %>%
                    mutate(death_rate = death_num/population*100000, group = input$state_choice) %>%
                    select(period, death_rate, group)
      
      state_begin <- state_data[state_data$period=="2000-2002",]$death_rate
      state_end <- state_data[state_data$period=="2015-2017",]$death_rate
      state_hi <- max(state_begin, state_end)
      state_lo <- min(state_begin, state_end)
      
      
      nation_data <- dplyr::filter(
                      cdc.data,
                      death_cause == input$death_cause
                    ) %>% 
                      drop_na() %>%
                      group_by(period) %>% 
                      summarise(population = sum(population), death_num = sum(death_num)) %>%
                      mutate(death_rate = death_num/population*100000, group = "United States") %>%
                      select(period, death_rate, group)

      nation_begin <- nation_data[nation_data$period=="2000-2002",]$death_rate
      nation_end <- nation_data[nation_data$period=="2015-2017",]$death_rate
      nation_hi <- max(nation_begin, nation_end)
      nation_lo <- min(nation_begin, nation_end)
      
      data <- bind_rows(state_data, nation_data)
      hi <- max(data$death_rate)
      lo <- min(data$death_rate)
      range <- hi - lo 
      ylim <- c(lo - 0.1 * range, hi + 0.1 * range)
      
      colors <- c("placeholder" = '#D95F02', "United States"=theme.cat.accent.color())
      names(colors) <- c(input$state_choice, "United States")
      
      line_plot <- ggplot(
                      data,
                      aes(
                        x = period, y = death_rate, 
                        color = group, group = group
                      )
                    ) + 
                      geom_line(size = 1.5) +
                      geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
                      theme.line.mort() + 
                      theme(legend.position = "bottom", legend.title = element_blank()) + 
                      ylab("Midlife deaths per 100,000") +
                      ylim(ylim) + 
                      scale_color_manual(values = colors) + 
                      geom_segment(aes(x=period_choice, xend=period_choice, y=lo, yend=hi), color = '#38761D', linetype=2) +
                      geom_polygon(data = data.frame(
                                              x = c(period_choice-0.1, period_choice+0.1, period_choice, period_choice-0.1), 
                                              y = c(hi+range*0.1, hi+range*0.1, hi, hi+range*0.1)), 
                                   aes(x = x, y = y), inherit.aes = FALSE, fill = '#38761D')
      
      if (xor(nation_end < state_end, state_end < state_begin)){
        label_data <- generate_label_data(state_data, nation_data, state_begin, state_end, nation_begin, nation_end,
                                          1, u*state_hi+v*state_lo, 6, u*nation_lo+v*nation_hi)
        label_data <- add_reference_point(label_data, state_begin, state_end, nation_begin, nation_end)
        line_plot <- draw_reference(line_plot, state_begin, state_end, nation_begin, nation_end)
      } else {
        label_data <- generate_label_data(state_data, nation_data, state_begin, state_end, nation_begin, nation_end, 
                                          6, v*state_hi+u*state_lo, 1, v*nation_lo+u*nation_hi)
        label_data <- add_reference_point(label_data, nation_begin, nation_end, state_begin, state_end)
        line_plot <- draw_reference(line_plot, nation_begin, nation_end, state_begin, state_end)
      }
      
      line_plot <- line_plot +
                      coord_cartesian(clip = "off") +
                      geom_label_repel(data = label_data, 
                                       mapping = aes(x = x, y = death_rate, label = label, fill = group),
                                       segment.colour = "#565254",
                                       color = "#f7f7f7",
                                       inherit.aes = FALSE,
                                       #hjust = "inward", vjust = "inward",
                                       #point.padding = 0.5,
                                       direction = "both",
                                       xlim = c(1.5, 5.5),
                                       ylim = ylim,
                                       show.legend = FALSE) + 
                      scale_fill_manual(values = colors) +
        labs(
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA"
        )
                                       
                      #geom_point(data = label_data, mapping = aes(x = x, y = death_rate), color = '#565254')
    }
    assign("page1_infographic", line_plot, envir = .GlobalEnv)
    line_plot
  }, bg="transparent")

  
  # Textual description box (upper-left panel, Page 1)
  output$textDescription <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h2(
        paste0("State View: ", names(which(cause.list == input$death_cause)), " in the State of ", names(which(state.list == input$state_choice)), " and their Associated Disparities")
      ),
      tags$h4(paste0(names(which(cause.definitions == input$death_cause)))),
      tags$h4("Counties are grouped into disparate risk clusters within a state based on their midlife mortality rate trends."),
      tags$h4("The top map shows how counties are grouped into disparate risk clusters within a state based on their midlifemortality rate trends. The lower map shows the risk cluster of each county. The line graph compares the average mortality rates per year for each risk cluster  with the national mean (blue)."),
      tags$h4("Darker colors indicate increased mortality risk. Hover to see information and definitions.  Click on maps to see county names and mortality rates.  Zoom maps with buttons or mouse. Click on right or onto learn more. "),
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
        paste0("Midlife Mortality Rates for ",
               names(which(cause.list == input$death_cause)), 
               " in ", 
               location_str,
               ":")
      )
    )
  })

  output$textMortFactsNew <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        title ="Midlife mortality trates are obtained from the Detailed Mortality Online Mortality Database at https://wonder.cdc.gov/.  Separate crude death rates are queried  for adults 25 to 64 at the county, state, and nationwide levels for each cause of death.  Rates are not age adjusted. Unreliable or missing rates are imputed. See Project Overview for details.",
        paste0("Midlife Mortality Rate: Deaths per 100,000 for people ages 25-to 64 due to ",
               names(which(cause.list == input$death_cause)), 
               " for three year periods for counties (left) and state and nation (right) . Darker colors indicate higher rates. "
               ), icon("info-circle")
      ),
      HTML("<h5>Data Source: CDC WONDER<br>Analysis: The Rensselaer Institute for Data Exploration and Applications 
           (<a href='http://idea.rpi.edu' target=_Blank>The Rensselaer IDEA</a>)</h5>")
    )
  })
  
  output$textInfographicTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          paste0("Midlife Mortality Rates for ",
                 names(which(cause.list == input$death_cause)), 
                 " in ", 
                 location_str)
        )
      )
    }
    else {
      location_str <- names(which(state.list == input$state_choice))
      tagList(
        tags$h3(
          paste0("Midlife Mortality Rates for ",
                 names(which(cause.list == input$death_cause)), 
                 " in ", 
                 location_str,
                 " vs. United States")
        )
      )
    }
  })
  
  output$textMortFactsClosing <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(paste0(names(which(cause.definitions == input$death_cause))))
    )
  })
  
  # For a state or the US, creates the bulleted facts at the bottom of nationwide page
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
          tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017"))),
          tags$li(tags$h4(paste0("Range from ", 
                         round(as.numeric(low.high.states.2015_2017()[1]), 1),
                         " per 100k people in ",
                         low.high.states.2015_2017()[2],
                         " to ",
                         round(as.numeric(low.high.states.2015_2017()[3]), 1),
                         " per 100k people in ",
                         low.high.states.2015_2017()[4],
                         " 2015-2017"))
          )
        )
      )
    }
    else {
      # percent change for first bullet
      change_text <- "remained the same"
      vals_text <- paste0("at ", round(state.mean.2015_2017(), 1), " per 100k people")
      
      percent_change <- round(
        (state.mean.2015_2017() - state.mean.2000_2002()) / state.mean.2000_2002() * 100,
        1
      )
      
      if (percent_change > 0) {
        change_text <- paste0("increased by ", abs(percent_change), "%")
        vals_text <- paste0("rising from ", round(state.mean.2000_2002(), 1), 
                            " to ", round(state.mean.2015_2017(), 1))
      }
      else if (percent_change < 0) {
        change_text <- paste0("decreased by ", abs(percent_change), "%")
        vals_text <- paste0("falling from ", round(state.mean.2000_2002(), 1), 
                            " to ", round(state.mean.2015_2017(), 1))
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
          #style = "font-size: 18px;",
          tags$li(tags$h4(paste0("Have ", change_text, " from 2000 to 2017, ", vals_text, " per 100k people"))),
          tags$li(tags$h4(paste0("Were ", comparison_text, " the national mean in 2015-2017 of ",
                                 round(national.mean.2015_2017(), 2), " per 100k people"))),
          tags$li(tags$h4(paste0("Range from ", 
                         round(as.numeric(low.rate.county.2015_2017()[1]), 1),
                         " per 100k people in ",
                         low.rate.county.2015_2017()[2],
                         " to ",
                         round(as.numeric(high.rate.county.2015_2017()[1]), 1),
                         " per 100k people in ",
                         high.rate.county.2015_2017()[2],
                         "from 2015-2017")
                  ))
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
        paste0("Nationwide View: ",names(which(cause.list == input$death_cause)), " Rates Over Time")
      )
    )
  })

  output$textNationwideTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if (is.null(input$page1_period)){
      period_choice = 6
    } else {
      period_choice = input$page1_period
    }
    
    tagList(
      tags$h3(
        paste0("Nationwide ",names(which(cause.list == input$death_cause)), " Rates for ", period.list[period_choice])
      )
    )
  })
  
  # Determinant Header (upper-right panel, Page 1)
  output$textDeterminants <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          style = "padding-right: 20px; padding-left: 20px",
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk cluster; or Protective, meaning it has a negative correlation with the risk cluster. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk clusters. For more information on the method of determining correlation please see Project Overview.", 
          paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", location_str),
          icon("info-circle")
        ),
        tags$h6("Kendall Correlation between factors and mortality risk clusters. Destructive factors are positively correlated; protective factors are negatively correlated. Click on dot to explore a factor in more detail."),
        NULL
      )
    }
    else {
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk cluster; or Protective, meaning it has a negative correlation with the risk cluster. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk clusters. For more information on the method of determining correlation please see Project Overview.", 
        paste0("Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))),
        icon("info-circle")
      ),
      tags$h6("Kendall Correlation between factors and mortality risk clusters. Destructive factors are positively correlated; protective factors are negatively correlated. Click on dot to explore a factor in more detail."),
      NULL
    )
    }
  })

  # Death Trends Header (Page 2 lower middle)
  output$textDeathTrends <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          style = "padding-right: 20px; padding-left: 20px",
          title="This plot represents the average midlife death trends for each cluster. The blue line represents the national average.  Click on a map to see the line for a specific county. If a state has 6 or fewer counties, the average for each county is shown.",
          paste0(names(which(cause.list == input$death_cause)), " Trends for ", location_str), 
          icon("info-circle")
        ),
        tags$h6("The average midlife death trends for each cluster; the national average is shown in blue."),
        NULL
      )
    }
    else {
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        title="This plot represents the average midlife death trends for each cluster. The blue line represents the national average.  Click on a map to see the line for a specific county. If a state has 6 or fewer counties, the average for each county is shown.",
        paste0(names(which(cause.list == input$death_cause)), " Trends for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      tags$h6("The average midlife death trends for each cluster; the national average is shown in blue. Click on any map to see the trend for a specific county."),
      NULL
    )
    }
  })

  # Mortality Rates Header (Page 2 lower middle)
  output$textMortRates <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(tags$h3(
        title="This plot represents the distribution of midlife mortality rates (ages 25-64) for the selected state.",
        paste0(names(which(cause.list == input$death_cause)), " Midlife Mortality Rates for ",
              location_str, " for ", input$year_selector),
        icon("info-circle")
      ),
      tags$h6("The geographic distribution of midlife mortality rates (ages 25-64) for ",location_str),
      NULL
      )
    }
    else {
    tagList(
      tags$h3(
        title="This plot represents the distribution of midlife mortality rates (ages 25-64) for the selected state.",
        paste0(names(which(cause.list == input$death_cause)), " Midlife Mortality Rates for ", names(which(state.list == input$state_choice))," for ",input$year_selector), 
        icon("info-circle")
      ),
      tags$h6("The geographic distribution of midlife mortality rates (ages 25-64) for ",names(which(state.list == input$state_choice))),
      NULL
    )
    }
  })

  # Cluster geo Header (Page 2 lower middle)
  output$textClusterGeo <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if (input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          style = "padding-right: 20px; padding-left: 20px",
          title="This plot represents the geographic distribution of risk clusters for the selected state.",
          paste0(names(which(cause.list == input$death_cause)), " Risk Clusters for ",location_str), 
          icon("info-circle")
        ),
        tags$h6("Geographic distribution of risk clusters for ",location_str,". Darker clusters indicate higher mortality risk."),
        NULL
      )
    }
    else {
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        title="This plot represents the geographic distribution of clusters for the selected state.",
        paste0(names(which(cause.list == input$death_cause)), " Risk Clusters for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      tags$h6("Geographic distribution of risk clusters for ",names(which(state.list == input$state_choice)),". Darker clusters indicate higher mortality risk."),
      NULL
    )
    }
  })

  # Determinant geo Header (Page 2 lower middle)
  output$textSDGeo <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
    }
    else {
    tagList(
      # tags$h3(
      #   style = "padding-right: 20px; padding-left: 20px",
      #   title="This plot represents the geographic distribution of the selected factor for the selected state.",
      #   paste0(input$determinant_choice, " Distribution for ", names(which(state.list == input$state_choice))), 
      #   icon("info-circle")
      # ),
      tags$h4("Geographic distribution of ",input$determinant_choice," for ", names(which(state.list == input$state_choice)),". Darker colors indicate higher values."),
      NULL
    )
  }
  })
  
  # Determinant geo Header (Page 2 lower middle)
  output$textCountyPrompt <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      # No prompt if United States
    }
    else {
      tagList(
        tags$h3(
          style = "margin-top: 0; padding-right: 20px; padding-left: 20px",
          paste0("Geographic distribution of ",input$determinant_choice," for ", names(which(state.list == input$state_choice)),". Darker colors indicate higher values. Select from the drop-down for county details or click the map.")
        ),
        # tags$h6("Geographic distribution of ",input$determinant_choice," for ", names(which(state.list == input$state_choice)),". Darker colors indicate higher values."),
        NULL
      )
    }
  })
  
  
  # Determinant Header (upper-left panel, Page 2)
  output$textDeterminants2 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h3(
          style = "padding-right: 20px; padding-left: 20px",
          title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk cluster; or Protective, meaning it has a negative correlation with the risk cluster. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk clusters. For more information on the method of determining correlation please see Project Overview.",
          paste0("Factor View: Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", location_str), 
          icon("info-circle")
        ),
        tags$h6("Kendall Correlation between factors and mortality risk clusters. Destructive factors are positively correlated; protective factors are negatively correlated. Click on dot to explore a factor in more detail."),
        NULL
      )
    }
    else {
    tagList(
      tags$h3(
        style = "padding-right: 20px; padding-left: 20px",
        title="Each factor is rated as Destructive, meaning that it has a positive correlation with the risk cluster; or Protective, meaning it has a negative correlation with the risk cluster. MortalityMinder shows those factors which have the highest absolute correlation with mortality risk clusters. For more information on the method of determining correlation please see Project Overview.",
        paste0("Factor View: Factors Associated with ",names(which(cause.list == input$death_cause)), " for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      tags$h6("Kendall Correlation between factors and mortality risk clusters. Destructive factors are positively correlated; protective factors are negatively correlated. Click on dot to explore a factor in more detail."),
      NULL
    )
    }
  })
  
  # Determinant Header (upper-center panel, Page 2)
  output$textDeterminants3 <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h2(
        style = "padding-right: 20px; padding-left: 20px",
        title="Help text for cluster distribution bar plots",
        paste0("Distribution of '",input$determinant_choice, "' across ", names(which(cause.list == input$death_cause)), " clusters for ", names(which(state.list == input$state_choice))), 
          icon("info-circle")
      ),
      NULL
    )
  })

  # Boxplot Header (upper-center panel, Page 3)
  output$textBoxplotTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h2(
          style = "padding-right: 20px; padding-left: 20px",
          title="Boxplot shows the distribution of the factor within each cluster. The middle line is the median. For destructive factors, boxes will shift up for higher risk clusters. For protective factors, boxes will shift down for high risk clusters.",
          paste0(input$determinant_choice, " and Risk Cluster Relationship for ", location_str), 
          icon("info-circle")
        ),
        NULL
      )
    }
    else {
    tagList(
      tags$h2(
        style = "padding-right: 20px; padding-left: 20px",
        title="Boxplot shows the distribution of the factor within each cluster. The middle line is the median. For destructive factors, boxes will shift up for higher risk clusters. For protective factors, boxes will shift down for high risk clusters.",
        paste0(input$determinant_choice, " and Risk Cluster Relationship for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      NULL
    )
  }
  })

  # Scatterplot Header (lower-center panel, Page 3)
  output$textScatterplotTitle <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    if(input$state_choice == "United States") {
      location_str <- "the United States" 
      tagList(
        tags$h2(
          style = "padding-right: 20px; padding-left: 20px",
          title="Plot of mortality rate versus factor. Each dot represents a county colored by its risk group. For destructive factors, counties will shift up as risk increases. For protective factors, counties will shift down  as risk decreases. Click on a county to see its name and where it is located on the map.",
          paste0(input$determinant_choice, " and Mortality Relationship for ", location_str), 
          icon("info-circle")
        ),
        NULL
      )
    }
    else {
    tagList(
      tags$h2(
        style = "padding-right: 20px; padding-left: 20px",
        title="Plot of mortality rate versus factor. Each dot represents a county colored by its risk group. For destructive factors, counties will shift up as risk increases. For protective factors, counties will shift down  as risk decreases. Click on a county to see its name and where it is located on the map.",
        paste0(input$determinant_choice, " and Mortality Relationship for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
      ),
      NULL
    )
  }
  })
  
  # Determinant geo Header (upper-center panel, Page 2)
  output$textDeterminantsGeo <- renderUI({
    # We reference state.list, cause.list and cause.definitions defined above
    
    tagList(
      tags$h4(
        style = "padding-right: 20px; padding-left: 20px",
        title="Geographic distribution of selected determinant across selected state",
        paste0("Distribution of '",input$determinant_choice, "' for ", names(which(state.list == input$state_choice))), 
        icon("info-circle")
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
      kmean.us.plot(input$death_cause)
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
      geo.us.plot(input$death_cause, input$year_selector)
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
      HTML(paste0("<b>", point$chr_code, "</b>", "<br/>",
                    "<i>", point$DIR, "</i>","<br/>",
                    SocialDeterminants[SocialDeterminants$Name == as.character(point$chr_code),]$Definitions[[1]],
                    NULL
      ))
    )
    
  })
  
  output$determinants_plot3_county_name <- renderUI({
    req(input$determinants_plot3_hover) # Same as if-not-NULL
    hover <- input$determinants_plot3_hover
    
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
    
    
    point <- nearPoints(data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); pointer-events:none;",
                    "left:", hover$coords_css$x+5, "px; top:", hover$coords_css$y+10, "px; font-size: 7px")
    
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
      dplyr::top_n(15, abs(kendall_cor)) %>% 
      dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
    
    # # Set currently selected determinant to most correlated determinant
    # max.cor.ind = which.max(abs(kendall.cor.new$kendall_cor))
    # input$determinant_choice = kendall.cor.new[max.cor.ind, "chr_code"]
    
    #Only display the social determinants graph if there is any significant social determinant
    #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
    if(nrow(kendall.cor.new) > 0) {
      updatePickerInput(session, "determinant_choice", selected = kendall.cor.new$chr_code[[1]])
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
          color = "#565254", 
          size = 4
        ) +
        geom_text(
          aes(label = round(kendall_cor, 2)), 
          color = "#565254", 
          size = 3
        ) +
        
        # Coordinates
        coord_flip() + 
        scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
        
        # Themes
        geom_hline(yintercept = .0, linetype = "dashed") + 
        labs(
          # title = "Most Associated Factors",
          # subtitle = "Kendall Correlation between Factors and Mortality Risk Cluster\nClick dot for details",
          caption = "Data Sources:\n\t1.CDC WONDER Multi-Cause of Death\n\t2.County Health Ranking 2019\nAnalysis: The Rensselaer IDEA",
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
  }, bg = "transparent")
  
  draw_border <- function(plot.name, border){
    proxy <- leafletProxy(plot.name)
    #remove any previously highlighted polygon
    proxy %>% clearGroup("highlighted_polygon")
    
    #add a slightly thicker red polygon on top of the selected one
    proxy %>% addPolylines(stroke = TRUE, 
                                   weight = 4,
                                   color="black",
                                   data = border,
                                   group="highlighted_polygon",
                                   dashArray = "4 2 4")
  }
  
  highlight_county <- function(event){
    county_name <- sub(event$id, pattern = " [[:alpha:]]*$", replacement = "")
    
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) == 0){
      for (current_polygons in state_map@polygons){
        for (current_polygon in current_polygons@Polygons){
          current_coords <- current_polygon@coords
          if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
            assign("county_polygon", current_polygons, envir = .GlobalEnv)
            break
          }
        }
      }
    }else if (length(county_indices) == 1){
      assign("county_polygon", state_map@polygons[[county_indices[[1]]]], envir = .GlobalEnv)
    } else {
      for (index in county_indices){
        current_polygon <- state_map@polygons[[index]]
        current_coords <- current_polygon@Polygons[[1]]@coords
        if (sp::point.in.polygon(c(event$lng), c(event$lat), current_coords[,1], current_coords[,2])){
          assign("county_polygon", current_polygon, envir = .GlobalEnv)
          break
        }
      }
    }
    draw_border("geo_cluster_kmean", county_polygon)
    draw_border("geo_mort_change2", county_polygon)
    draw_border("determinants_plot5", county_polygon)
  }
  
  # click on geo cluster map shows county data on mort_line
  observe({
    event <- input$geo_cluster_kmean_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(event$id)
    updatePickerInput(session, "county_drop_choice", selected = gsub(" County", "", event$id))
  })
  
  observe({
    event <- input$geo_mort_change2_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(event$id)
    updatePickerInput(session, "county_drop_choice", selected = gsub(" County", "", event$id))
  })
  
  observe({
    event <- input$determinants_plot5_shape_click
    if (is.null(event))
      return()
    highlight_county(event)
    county_choice(paste0(event$id," County"))
    updatePickerInput(session, "county_drop_choice", selected = event$id)
  })
  
  observe({
    county_name <- sub(county_choice(), pattern = " [[:alpha:]]*$", replacement = "")
    req(county_name)
    county_indices <- which(state_map@data$NAME %in% c(county_name))
    
    if (length(county_indices) != 1){
      all.county = state_map@data$NAME
      highest.score = - Inf
      polygon = NULL
      for (index in seq(1, length(all.county))){
        county = all.county[[index]]
        curr.score =  stringdist::stringsim(county, county_name)
        if (curr.score > highest.score){
          highest.score = curr.score
          polygon = state_map@polygons[[index]]
        }
      }
    } else {
      polygon <- state_map@polygons[[county_indices[[1]]]] 
    }
    
    draw_border("geo_cluster_kmean", polygon)
    draw_border("geo_mort_change2", polygon)
    draw_border("determinants_plot5", polygon)
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
    county_choice(paste0(county_name, " County"))
    
    updatePickerInput(session, "county_drop_choice", selected = point$county_name)
    
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page1_bar_plot_click) # Same as if-not-NULL
    click <- input$page1_bar_plot_click
    
    js$nextpage()
    
    point <- nearPoints(kendall_cor_new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
  })
  
  # click on bar plot triggers page change
  observe({
    req(input$page2_bar_plot_click) # Same as if-not-NULL
    click <- input$page2_bar_plot_click
    
    point <- nearPoints(kendall_cor_new, click, threshold = 50, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)

    updatePickerInput(session, "determinant_choice", selected = point$chr_code)
  })
}

shinyApp(ui = ui, server = server)

