input <- list(
  "state_choice" = "NY",
  "death_cause" = "Despair"
)

mort.cluster.raw <- cdc.data %>%
  cdc.mort.mat(input$state_choice, input$death_cause) %>%
  km.func(4)

mort.avg.cluster.raw <- cdc.data %>%
  dplyr::filter(state_abbr == input$state_choice, death_cause == input$death_cause) %>%
  dplyr::right_join(mort.cluster.raw, by = "county_fips") %>%
  dplyr::group_by(period, cluster) %>%
  dplyr::summarise(death_rate = sum(death_num) / sum(population) * 10^5) %>% 
  dplyr::ungroup()

mort.cluster.map <- mort.avg.cluster.raw %>% 
    dplyr::filter(period == "2015-2017") %>%
    dplyr::arrange(death_rate) %>% 
    dplyr::mutate(ord = as.character(1:n())) %>% 
    dplyr::select(-c(period, death_rate)) %>% 
    textshape::column_to_rownames("cluster")

tmp <- mort.cluster.raw %>%
  dplyr::mutate(cluster = mort.cluster.map[cluster, "ord"]) %>% 
  kendall.func.dev(chr.data.2019)

# label <- cdc.data %>%
#   cdc.mort.mat(input$state_choice, input$death_cause) %>%
#   km.func(4)
# 
# 
# dat <- cdc.data %>%
#   dplyr::filter(state_abbr == input$state_choice, death_cause == input$death_cause) %>%
#   dplyr::right_join(label, by = "county_fips") %>%
#   dplyr::select(county_name, death_rate, period, cluster) %>%
#   filter(period == "2000-2002" | period == "2015-2017") %>% 
#   arrange(cluster) %>% 
#   mutate(county_name = fct_reorder(county_name, as.numeric(cluster), last)) %>% 
#   group_by(county_name) %>% 
#   mutate(MIN = min(death_rate)) 
#   
#   
# ggplot(dat, aes(x = death_rate, y = county_name, color = cluster)) + 
#   geom_path(arrow = arrow(length = unit(1.5, "mm"), type = "closed")) + 
#   geom_text(
#     aes(
#       death_rate,
#       county_name,
#       label = round(death_rate, 1),
#       hjust = ifelse(
#         death_rate == MIN,
#         1.4, 
#         -.4
#       )
#     ),
#     # vjust = 0.3,
#     family = "Bookman",
#     size = 3,
#     color = "gray25"
#   ) +
#   theme.background() + 
#   theme.text() + 
#   coord_cartesian(xlim = c(0, 130))

# ggplot(
#   data,
#   aes(
#     x = period, y = death_rate, 
#     color = cluster, group = cluster
#   )
# ) + 
#   geom_line(size = 0.7) + 
#   geom_point(color = "black", shape = 21, fill = "white") + 
#   labs.line.mort(input$state_choice, input$death_cause) + 
#   color.line.cluster() +
#   theme.line.mort()
