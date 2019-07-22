init <- function() {
  setwd("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Librarian.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Loader_CHR2019.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Loader_CDC.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Loader_GEO.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Analyzer_PCA.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Analyzer_Kmeans.R")
  source("~/data/AHRQ_Challenge/App_Shiny_Beta.01/init/Analyzer_Correlation.R")
}
init()


ui <- fluidPage(
  # include css
  tags$head(includeCSS("~/data/AHRQ_Challenge/App_Shiny_Beta.01/www/custom.css")),
  tags$head(
    tags$script(src="jquery-3.4.1.min.js"),
    tags$script("$.noConflict(true);")),
  
  # navbar
  tags$div(
    class = "navbar",
    tags$a(href="#page1","Mortality Overview"),
    tags$a(href="#page2", "Social Determinants")
  ),
  
  tags$div(
    class = "drop",
    dropdown(
      tags$h4("Mortality Data Selection"),
      tags$div(
        class = "parameters",
        tags$ul(
          tags$li(
            class = "state",
            pickerInput(
              inputId = "state_choice",
              label = h4("State"), 
              choices = state.abb,
              selected = "NY",
              options = list(
                `live-search` = TRUE
              )
            )
          ),
          tags$li(
            class = "death_cause",
            pickerInput(
              inputId = "death_cause",
              label = h4("Cause of Death"), 
              choices = c("Despair","Cancer","Assault"),
              choicesOpt = list(
                subtext = c("Self-Harm")
              )
            )
          ),
          tags$li(
            class = "cluster",
            pickerInput(
              inputId = "cluster_choice",
              label = h4("Advanced settings"), 
              choices = c("k-means","k-means+","DBScan"),
              choicesOpt = list(
                subtext = c("good when easy data","good when hard data","good when stable data")
              )
            )
          )
        ),
        
        sliderTextInput(
          inputId = "mort_period_choice",
          width = "100%",
          label = "Time Period:", 
          grid = TRUE,
          force_edges = TRUE,
          choices = c(
            "2000-2002", "2003-2005", "2006-2008", 
            "2009-2011", "2012-2014", "2015-2017"
          )
        ),
        downloadButton(outputId = "download_data", label = "Download Selected Data")
      ),
      
      inputId = "settings",
      right = "true",
      style = "simple", icon = icon("bars"),
      status = "danger", width = "500px"
      
    )
  ),

  tags$div(
    class = "main",
    # main
    
    tags$div(
      id = "page1",
      # page1 start
      tags$div(
        class = "tabs",
        tags$div(
          class = "bar",
          tags$button(
            class = "mort_button",
            onclick = "showGeo(\"geo_mort\")",
            "Mortality distribution"
          ),
          tags$button(
            class = "cluster_button",
            onclick = "showGeo(\"geo_cluster\")",
            "cluster distribution"
          )
          
        ),
        tags$div(
          class = "geo_plots",
          tags$div(
            id = "geo_mort",
            class = "geo",
            plotOutput("geo_cluster_mort",width="100%",height="100%")
            
          ),
          tags$div(
            id = "geo_cluster",
            class = "geo",
            plotOutput("geo_cluster_kmean",width="100%",height="100%")
            
          )
        )
      ),
      
      tags$div(
        class = "scroll",
        
        # scroll start
        tags$div(
          class = "mort_density",
          h3("Mortality Density"),
          tags$div(
            class = "mort_density_plot",
            plotOutput("mort_density",height = "100%", width = "100%")
          ),
          tags$div(
            class = "mort_density_text",
            "Desc",
            tags$hr()
          )
        ),
        
        tags$div(
          class = "mort_trend",
          h3("Mortality Trend"),
          tags$div(
            class = "mort_line_plot",
            plotOutput("mort_line",height = "100%", width = "100%")
          ),
          tags$div(
            class = "mort_trend_text",
            "Desc",
            tags$hr()
          )
          
        ),
        
        tags$div(
          class = "urban_dist_cluster",
          h3("Urbanization-Rural Composition"),
          tags$div(
            class = "urban_dist_cluster_plot",
            plotOutput("urban_dist_cluster", height = "100%", width = "100%")
          ),
          tags$div(
            class = "urban_dist_cluster_text",
            "Desc"
          )
          
        )
      
      # end of scroll 
      )
      
    # end of page1 
    ),
    tags$div(
      id = "page2",
      tags$hr(),
      tags$div(
        class = "corrolation",
        plotOutput("kendell",width = "100%",height = "100%")
      ),
      tags$div(
        class = "sd_choice",
        pickerInput(
          inputId = "sd_choice",
          label = "Social Determinent", 
          choices = stats::setNames(chr.namemap.2019$code, chr.namemap.2019$name),
          options = list(
            `live-search` = TRUE)
        )
        
      ),
      tags$div(
        class = "sd_boxplot",
        plotOutput("sd_boxplot",height = "100%", width = "100%")
      ),
      tags$div(
        class = "sd_density",
        plotOutput("sd_density",height = "100%", width = "100%")
      )
    ),
    tags$div(
      id = "page3",
      plotOutput("geo_sd",height = "100%", width = "100%")
    )
    
    # main end
  ),
  
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ScrollMagic/2.0.7/ScrollMagic.min.js"),
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ScrollMagic/2.0.7/plugins/debug.addIndicators.min.js"),
  includeScript(path = "~/data/AHRQ_Challenge/App_Shiny_Beta.01/inst/myscript.js")
)









server <- function(input, output){
  output$test <- renderText({
    input$death_cause
  })
  
  # Generate labelled mortality matrix
  mort.mat.label <- reactive({
    mort.mat <- cdc.mort.mat(cdc.data, input$state_choice, input$death_cause)
    mort.label <- km.func(mort.mat, 4)
    na.omit(dplyr::left_join(mort.label, mort.mat, by = "county_fips"))
  })
  
  output$geo_cluster_mort <- renderPlot({

    # Basic settings
    mort.mat <- mort.mat.label()
    mort.min <- 5
    mort.max <- max(mort.mat[-c(1, 2)])
    mort.period <- input$mort_period_choice
    cluster.type <- "km_cluster"
    
    #Fetch geo-data
    geo.data <- geo.map.fetch(input$state_choice, geo.namemap.state)
    geo.data <- dplyr::filter(cdc.data, state_abbr == input$state_choice) %>% 
      dplyr::select(county_name, county_fips) %>% 
      unique() %>% 
      left_join(geo.data, by = "county_name")

    # Calculate cluster label
    left_join(mort.mat, geo.data, by = "county_fips") %>%
      
    # Choropleth graph  
    ggplot(
        aes_(
          x = as.name("long"),
          y = as.name("lat"),
          fill = as.name(mort.period),
          group = as.name("geo_group")
        )
      ) + 
      geom_polygon(size = 0.25, color = "black", alpha = 0.9) + 
      scale_fill_viridis(
        breaks = round(seq(mort.min, mort.max, by = (mort.max - mort.min) / 5)), 
        name = "Mortality Rate", 
        guide = guide_legend(
          keyheight = unit(3, units = "mm"), 
          keywidth = unit(12, units = "mm"), 
          label.position = "bottom", 
          title.position = "top", 
          nrow = 1
        )
      ) +
      labs(
        title = paste("Mortality Rate by County", input$mort_period_choice, sep = "\n"),
        x = NULL,
        y = NULL
      ) + 
      theme_void() +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        
        plot.title = element_text(
          size = 16, hjust = 0.01, color = "#4e4d47", 
          margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
        ),
        
        legend.position = c(0.75, 1),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      ) + 
      coord_map()
  })
  
  output$geo_cluster_kmean <- renderPlot({
    
    # Basic settings
    mort.mat <- mort.mat.label()
    mort.period <- input$mort_period_choice
    cluster.type <- "km_cluster"
    cluster.num <- 4
    
    # Fetch geo-data
    geo.data <- geo.map.fetch(input$state_choice, geo.namemap.state)
    geo.data <- dplyr::filter(cdc.data, state_abbr == input$state_choice) %>% 
      dplyr::select(county_name, county_fips) %>% 
      unique() %>% 
      left_join(geo.data, by = "county_name")
    
    # Choropleth graph
    left_join(mort.mat, geo.data, by = "county_fips") %>%
      ggplot(
        aes_(
          x = as.name("long"), 
          y = as.name("lat"), 
          fill = as.name(cluster.type), 
          group = as.name("geo_group")
        )
      ) +
      geom_polygon(size = 0.25, color = "black", alpha = 0.9) + 
      scale_fill_manual(
        name = "Cluster", 
        guide = guide_legend(
          keyheight = unit(3, units = "mm"), 
          keywidth = unit(12, units = "mm"), 
          label.position = "bottom", 
          title.position = "top", 
          nrow = 1
        ),
        values = colorRampPalette(brewer.pal(9, "Blues")[-c(1:3)])(cluster.num)
      ) +  
      labs(
        title = paste("Cluster Distribution", mort.period, sep = "\n"),
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        plot.title = element_text(size = 16, hjust = 0.01, color = "#4e4d47", margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.75, 1),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      ) + 
      coord_map()
  })
  
  # Plot that help select different clustering algo
  output$cluster_algo_helper <- renderPlotly({
    ggplotly({
      
      # Generate mortality matrix
      mort.mat <- cdc.generateMat.state(cdc.data, input$state_choice)
      km.wssplot(select(mort.mat, -c(1, 2))) + 
        theme(
          axis.title.x = element_text(family = "Arial", size = "11", color = "#404040"),
          axis.title.y = element_text(family = "Arial", size = "11", color = "#404040"),
          plot.margin = unit(c(0,0,1,1), "cm")
        )
    })
  })
  
  # Densitiy plot of mortality, grouped by cluster
  output$mort_density <- renderPlot({

    # Basic settings
    mort.mat <- mort.mat.label()
    mort.period <- input$mort_period_choice
    cluster.type <- "km_cluster"
    cluster.num <- 4
    
    # Density plot
    ggplot(
        mort.mat, 
        aes_(
          x = as.name(mort.period), 
          fill = as.name(cluster.type)
        )
      ) + 
      geom_density(alpha = 0.4) + 
      labs(
        x = "Mortality Rate", 
        y = "Density",
        fill = "Trend Cluster"
      ) + 
      scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Reds"))(cluster.num)) + 
      theme_classic()
  })
  
  # Line graph of mortality, grouped by cluster
  output$mort_line <- renderPlot({
    
    cluster.num <- 4

    # Calculate cluster label and colMeans
    mort.mat.summary <- mort.mat.label() %>%
      gather(key = "period", value = "death_rate", `2000-2002`:`2015-2017`) %>% 
      dplyr::group_by(period, km_cluster) %>% 
      dplyr::summarise(mean_death_rate = mean(death_rate)) %>% 
      mutate(
        year_group = dplyr::recode(
          period,
          `2000-2002` = 1,
          `2003-2005` = 2,
          `2006-2008` = 3,
          `2009-2011` = 4,
          `2012-2014` = 5,
          `2015-2017` = 6
        )
      )
    
    # Make a color range using colorRampPalette() and the set of blues
    red_range <- colorRampPalette(brewer.pal(9, "Reds")[-c(1:3)])
    ggplot(mort.mat.summary, aes(x = year_group, y = mean_death_rate, color = km_cluster)) + 
      geom_line(size = 0.5) + 
      geom_point(color = "black", shape = 21, fill = "white") + 
      labs(
        x = "Period", 
        y = "Mortality Rate",
        color = "Trend Cluster"
      ) + 
      scale_x_continuous(
        breaks = 1:6, labels = c(
          "2000-2002", "2003-2005", "2006-2008",
          "2009-2011", "2012-2014", "2015-2017"
        )
      ) + 
      scale_color_manual(values = red_range(cluster.num)) + 
      theme_classic()
  })
  
  output$urban_dist_cluster <- renderPlot({
    
    # Calculate cluster label
    cluster.num <- 4
    mort.label <- km.func(
      cdc.mort.mat(cdc.data, input$state_choice, input$death_cause), cluster.num
    )
    urban.data <- cdc.data %>% 
      dplyr::filter(state_abbr == input$state_choice) %>% 
      dplyr::select(county_fips, urban_2013) %>% 
      unique() %>% 
      dplyr::left_join(mort.label, by = "county_fips")
    
    ggplot(urban.data, aes(km_cluster, fill = urban_2013)) +
      geom_bar(position = "fill", color = "black", width = .75) +
      labs(
        title = "Urban-Rural Composition by Cluster",
        x = "Cluster",
        y = "Composition",
        fill = "Urbanization 2013"
      ) +
      theme_classic() +
      scale_fill_manual(
        values = colorRampPalette(brewer.pal(9, "Blues"))(6)
      ) +
      #coord_polar(theta = "y") +
      NULL
  })
  
  output$kendell <- renderPlot({

    kendall.cor <- kendall.func(
      cdc.data, 
      input$mort_period_choice, 
      chr.data.2019, 
      input$state_choice,
      input$death_cause
    )
    kendall.cor %>%
      dplyr::mutate(
        sign = dplyr::if_else(
          kendall_cor <= 0,
          "Protective",
          "Destructive"
        )
      ) %>%
      na.omit() %>% 
      dplyr::filter(kendall_p < 0.1) %>%
      ggplot(
        aes(x = reorder(chr_code, kendall_cor), y = kendall_cor, color = sign, fill = sign)
      ) + 
      geom_point(stat = 'identity', size = 8) + 
      geom_segment(
        size = 1,
        aes(
          y = 0, 
          x = reorder(chr_code, kendall_cor), 
          yend = kendall_cor, 
          xend = reorder(chr_code, kendall_cor),
          color = sign
        )
      ) +
      geom_text(aes(label = round(kendall_cor, 2)), color = "black", size = 2) +
      coord_flip() + 
      scale_y_continuous(
        breaks = seq(
          round(min(kendall.cor$kendall_cor), 2) - .03,
          round(max(kendall.cor$kendall_cor), 2) + .03,
          by = .1
        )
      ) +
      geom_hline(yintercept = .0, linetype = "dashed") + 
      labs(
        title = "Kendall Correlation with 2015-2017 Deaths of Despair",
        y = "Correlation",
        x = "Social Determinants"
      ) + 
      theme_classic()
  })

  output$download_data <- downloadHandler(
    filename = "mortality_data.csv",
    content = function(file) {
      # create data
      data <- cdc.data %>% 
        dplyr::filter(
          death_cause == input$death_cause,
          period == input$mort_period_choice,
          state_abbr == input$state_choice
        )
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$sd_boxplot <- renderPlot({
    # Basic settings and get mort label
    mort.mat <- mort.mat.label()
    cluster.type <- "km_cluster"
    cluster.num <- 4
    
    # Get state name corresponding to the input state abbr
    state.name <- dplyr::pull(
      dplyr::filter(geo.namemap.state, state_abbr == input$state_choice), 
      state_name
    )
    
    # Subsetting data according to state selected and sd selected
    dplyr::filter(chr.data.2019, state_name == state.name) %>% 
      dplyr::left_join(mort.mat, by = "county_fips") %>% 
      
      # Plot boxplot
      ggplot(
        aes_(
          x = as.name(cluster.type),
          y = as.name(input$sd_choice)
        )
      ) + 
      geom_boxplot() + 
      theme_classic()
  })
  
  output$sd_density <- renderPlot({
    # Basic settings and get mort label
    mort.mat <- mort.mat.label()
    cluster.type <- "km_cluster"
    cluster.num <- 4
    
    # Get state name corresponding to the input state abbr
    state.name <- dplyr::pull(
      dplyr::filter(geo.namemap.state, state_abbr == input$state_choice), 
      state_name
    )
    
    # Subsetting data according to state selected and sd selected
    dplyr::filter(chr.data.2019, state_name == state.name) %>% 
      dplyr::left_join(mort.mat, by = "county_fips") %>%
      
      # Plot boxplot
      ggplot(
        aes_(
          x = as.name(input$sd_choice),
          fill = as.name(cluster.type)
        )
      ) + 
      geom_density(alpha = 0.4) + 
      scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Reds"))(cluster.num)) + 
      theme_classic()
  })
  
  output$geo_sd <- renderPlot({
    
    # Get state name corresponding to the input state abbr
    state.name <- dplyr::pull(
      dplyr::filter(geo.namemap.state, state_abbr == input$state_choice), 
      state_name
    )
    
    # Subsetting data according to state selected and sd selected
    sd.data <- dplyr::filter(chr.data.2019, state_name == state.name)
    
    # Fetch geo-data
    geo.data <- geo.map.fetch(input$state_choice, geo.namemap.state)
    
    # Choropleth graph
    left_join(sd.data, geo.data, by = "county_name") %>%
      ggplot(
        aes_(
          x = as.name("long"), 
          y = as.name("lat"), 
          fill = as.name(input$sd_choice), 
          group = as.name("geo_group")
        )
      ) +
      geom_polygon(size = 0.25, color = "black", alpha = 0.9) +   
      labs(
        title = paste(input$sd_choice, "Value Distribution"),
        x = NULL,
        y = NULL
      ) +
      scale_fill_viridis(
        name = "Val", 
        guide = guide_legend(
          keyheight = unit(3, units = "mm"), 
          keywidth = unit(12, units = "mm"), 
          label.position = "bottom", 
          title.position = "top", 
          nrow = 1
        )
      ) + 
      theme_void() +
      theme(
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        plot.title = element_text(size = 16, hjust = 0.01, color = "#4e4d47", margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.75, 1),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
      ) + 
      coord_map()
  })
}

shinyApp(ui = ui, server = server)