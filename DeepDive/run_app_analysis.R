# title: "run_app_analysis"
# author: Ross DeVito
#

source("run_loaders_analyzers.R")

# selected params for run
death.causes <- c("Despair","Cancer","Assault","Cardiovascular")

input <- list(state_choice = state.abb[1],
              death_cause = death.causes[1])

# This is copy and pasted because there is currently no way for code reuse unfortunately.
#   Functions are made not reactive 

# Cache of UNORDERED mortality trend cluster label calculation
mort.cluster.raw <- function() {
  
  # Variables:
  #   - county_fips
  #   - cluster
  
  # Currently hard-coded 4 clusters
  cdc.data %>%
    cdc.mort.mat(input$state_choice, input$death_cause) %>%
    km.func(4)
}

# # Cache of Weighed Avg by UNORDERED cluster
# mort.avg.cluster.raw <- function() {
# 
#   # Variables:
#   #   - period
#   #   - cluster
#   #   - death_rate
#   #   - count
# 
#   # Notes:
#   #   - The cluster labels are UNORDERED
# 
#   cdc.data %>%
#     dplyr::filter(state_abbr == input$state_choice, death_cause == input$death_cause) %>%
#     dplyr::right_join(mort.cluster.raw(), by = "county_fips") %>%
#     dplyr::group_by(period, cluster) %>%
#     dplyr::summarise(
#       death_rate = sum(death_num) / sum(population) * 10^5,
#       count = n()
#     ) %>%
#     dplyr::ungroup()
# }
# 
# # Cache of MAPPING from UNORDERED mortality trend label to ORDERED mortality trend label
# mort.cluster.map <- function() {
# 
#   # Variables:
#   #   - ord
# 
#   # Notes:
#   #   - This is a mapping from raw cluster label to ORDERED cluster.
#   #       Row names are the original cluster and `ord` are the reordered cluster
# 
#   mort.avg.cluster.raw() %>%
#     dplyr::filter(period == "2015-2017") %>%
#     dplyr::arrange(death_rate) %>%
#     dplyr::mutate(ord = as.character(1:n())) %>%
#     dplyr::select(-c(period, death_rate)) %>%
#     textshape::column_to_rownames("cluster")
# }
# 
# # Cache of ORDERED mortality trend cluster label calculation
# mort.cluster.ord <- function() {
# 
#   # Variables:
#   #   - county_fips
#   #   - cluster
# 
#   dplyr::mutate(mort.cluster.raw(), cluster = mort.cluster.map()[cluster, "ord"])
# }
# 
# # Cache of Weighed Avg by ORDERED cluster
# mort.avg.cluster.ord <- function() {
# 
#   # Variables:
#   #   - period
#   #   - cluster
#   #   - death_rate
#   #   - count
# 
#   # Notes:
#   #   - The cluster labels are ORDERED
# 
#   dplyr::mutate(mort.avg.cluster.raw(), cluster = mort.cluster.map()[cluster, "ord"])
# }

# -------------------------------------------------------------------------------------------------------------------------- #

# # Mortality Rate Trend Line Graph
# output$mort_line <- renderPlot({
#   ggplot(
#     mort.avg.cluster.ord(),
#     aes(
#       x = period, y = death_rate,
#       color = cluster, group = cluster
#     )
#   ) +
#     geom_line(size = 1) +
#     geom_point(color = "black", shape = 21, fill = "white") +
#     labs.line.mort(input$state_choice, input$death_cause) +
#     color.line.cluster() +
#     theme.line.mort() +
#     guides(
#       color = guide_legend(reverse = T)
#     )
# })
# 
# # Mortality Rate Table
# output$table <- renderTable(width = "100%", {
#   rate.table <- mort.avg.cluster.ord() %>%
#     dplyr::select(cluster, period, death_rate) %>%
#     tidyr::spread(key = period, value = death_rate) %>%
#     dplyr::select(cluster, `2000-2002`, `2015-2017`)
# 
#   count.table <- mort.avg.cluster.ord() %>%
#     dplyr::select(cluster, count) %>%
#     base::unique()
# 
#   dplyr::left_join(count.table, rate.table, by = "cluster") %>%
#     dplyr::mutate(cluster = as.character(cluster)) %>%
#     dplyr::arrange(desc(cluster)) %>%
#     dplyr::rename(
#       "Trend Grp." = "cluster",
#       "Count" = "count"
#     )
# })
# 
# # Mortality Cluster Urbanization Composition
# output$urban_dist_cluster <- renderPlot({
# 
#   # Calculate cluster label
#   cluster.num <- 4
#   urban.data <- cdc.data %>%
#     dplyr::filter(state_abbr == input$state_choice) %>%
#     dplyr::select(county_fips, urban_2013) %>%
#     unique() %>%
#     dplyr::left_join(mort.label.raw(), by = "county_fips")
# 
#   ggplot(urban.data, aes(km_cluster, fill = urban_2013)) +
#     geom_bar(position = "fill", color = "black", width = .75) +
#     labs(
#       title = "Urban-Rural Composition by Cluster",
#       x = "Cluster",
#       y = "Composition",
#       fill = "Urbanization 2013"
#     ) +
#     scale_fill_manual(
#       values = colorRampPalette(brewer.pal(9, "Blues"))(6)
#     ) +
#     theme_minimal() +
#     theme(
#       plot.background = element_rect(fill = "gray95", color = "gray95"),
#       plot.margin = unit(c(5, 10, 5, 10), units = "mm")
#     ) +
#     theme.text() +
#     NULL
# })
# 
# # Mortality Trend Cluster by County
# output$geo_cluster_kmean <- renderPlot({
#   draw.geo.cluster(input$state_choice, mort.cluster.ord())
# })
# 
# # Mortality Rate by County Period 1
# output$geo_mort_change1 <- renderPlot({
# 
#   mort.data <- dplyr::filter(
#     cdc.data,
#     state_abbr == input$state_choice,
#     death_cause == input$death_cause,
#     period == "2000-2002"
#   ) %>%
#     dplyr::mutate(
#       death_rate = death_num / population * 10^5,
#       death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
#     ) %>%
#     dplyr::select(county_fips, death_rate, period)
# 
#   draw.geo.mort(input$state_choice, "2000-2002", mort.data, input$death_cause)
# })
# 
# # Mortality Rate by County Period 2
# output$geo_mort_change2 <- renderPlot({
# 
#   mort.data <- dplyr::filter(
#     cdc.data,
#     state_abbr == input$state_choice,
#     death_cause == input$death_cause,
#     period == "2015-2017"
#   ) %>%
#     dplyr::mutate(
#       death_rate = death_num / population * 10^5,
#       death_rate = cut(death_rate, bin.geo.mort(input$death_cause))
#     ) %>%
#     dplyr::select(county_fips, death_rate, period)
# 
#   draw.geo.mort(input$state_choice, "2015-2017", mort.data, input$death_cause)
# })
# 
# # Kendall Correlation Between Raw Mort Rate and CHR-SD
# output$page1.bar.cor1 <- renderPlot({
#   kendall.cor <- kendall.func(mort.cluster.ord(), chr.data.2019)
# 
#   kendall.cor %>%
#     dplyr::mutate(
#       Direction = dplyr::if_else(
#         kendall_cor <= 0,
#         "Protective",
#         "Destructive"
#       ),
#       kendall_cor = abs(kendall_cor),
#       chr_code = chr.namemap.2019[chr_code, 1]
#     ) %>% na.omit() %>%
#     dplyr::filter(kendall_p < 0.05) %>%
#     dplyr::arrange(desc(kendall_cor)) %>%
#     dplyr::top_n(10, kendall_cor) %>%
#     ggplot(
#       aes(x = reorder(chr_code, kendall_cor), y = kendall_cor, color = Direction, fill = Direction)
#     ) +
#     geom_point(stat = 'identity', size = 8) +
#     geom_segment(
#       size = 1,
#       aes(
#         y = 0,
#         x = reorder(chr_code, kendall_cor),
#         yend = kendall_cor,
#         xend = reorder(chr_code, kendall_cor),
#         color = Direction
#       )
#     ) +
#     geom_text(
#       aes(label = round(kendall_cor, 2)),
#       color = "black",
#       size = 2
#     ) +
#     coord_flip() +
#     scale_y_continuous(
#       breaks = seq(
#         round(min(kendall.cor$kendall_cor), 2) - .03,
#         round(max(kendall.cor$kendall_cor), 2) + .03,
#         by = .1
#       )
#     ) +
#     geom_hline(yintercept = .0, linetype = "dashed") +
#     labs(
#       title = "Most Influential Social Determinants of 2019",
#       subtitle = "Kendall Correlation: SD - Mortality Trend Cluster",
#       caption = "Data Source:\n\t1.CDCWONDER Multi-Cause of Death\n\t2.County Health Ranking 2019",
#       y = "Correlation (tau)",
#       x = NULL
#     ) +
#     theme_minimal() +
#     theme.text() +
#     theme.background() +
#     theme(
#       axis.text.y = element_text(size = 12),
#       axis.text.x = element_text(size = 12),
#       axis.title.x = element_text(size = 12)
#     )
# 
# })
# 
# 
# data_to_json <- function(data) {
#   jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)
# }
# output$d3 <- renderD3({
#   data_geo <- jsonlite::read_json("all-counties.json")
#   data_stat <- cdc.mort.mat(cdc.data,input$state_choice,input$death_cause)
#   r2d3(data = list(data_geo,data_to_json(data_stat),state.name[grep(input$state_choice, state.abb)]),
#        d3_version = 3,
#        dependencies = "topojson.min.js",
#        css = "geoattr.css",
#        script = "d3.js")
# })