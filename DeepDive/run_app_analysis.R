# title: "run_app_analysis"
# author: Ross DeVito
#
# Loads in data sources and functions used in app and applies to selected state and
#   death cause so that all data used in the app at the time those variable are selected
#   is available here

library(dplyr)  # dplyr is needed for pipe syntax '%>%' 
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

# Cache of Weighed Avg by UNORDERED cluster
mort.avg.cluster.raw <- function() {

  # Variables:
  #   - period
  #   - cluster
  #   - death_rate
  #   - count

  # Notes:
  #   - The cluster labels are UNORDERED

  cdc.data %>%
    dplyr::filter(state_abbr == input$state_choice, death_cause == input$death_cause) %>%
    dplyr::right_join(mort.cluster.raw(), by = "county_fips") %>%
    dplyr::group_by(period, cluster) %>%
    dplyr::summarise(
      death_rate = sum(death_num) / sum(population) * 10^5,
      count = n()
    ) %>%
    dplyr::ungroup()
}

# Cache of MAPPING from UNORDERED mortality trend label to ORDERED mortality trend label
mort.cluster.map <- function() {

  # Variables:
  #   - ord

  # Notes:
  #   - This is a mapping from raw cluster label to ORDERED cluster.
  #       Row names are the original cluster and `ord` are the reordered cluster

  mort.avg.cluster.raw() %>%
    dplyr::filter(period == "2015-2017") %>%
    dplyr::arrange(death_rate) %>%
    dplyr::mutate(ord = as.character(1:n())) %>%
    dplyr::select(-c(period, death_rate)) %>%
    textshape::column_to_rownames("cluster")
}

# Cache of ORDERED mortality trend cluster label calculation
mort.cluster.ord <- function() {

  # Variables:
  #   - county_fips
  #   - cluster

  dplyr::mutate(mort.cluster.raw(), cluster = mort.cluster.map()[cluster, "ord"])
}

# Cache of Weighed Avg by ORDERED cluster
mort.avg.cluster.ord <- function() {

  # Variables:
  #   - period
  #   - cluster
  #   - death_rate
  #   - count

  # Notes:
  #   - The cluster labels are ORDERED

  dplyr::mutate(mort.avg.cluster.raw(), cluster = mort.cluster.map()[cluster, "ord"])
}

# -------------------------------------------------------------------------------------------------------------------------- #

# Mortality Rate Trend Line Graph
plot_mort_line <- function() {
  ggplot(
    mort.avg.cluster.ord(),
    aes(
      x = period, y = death_rate,
      color = cluster, group = cluster
    )
  ) +
    geom_line(size = 1) +
    geom_point(color = "black", shape = 21, fill = "white") +
    labs.line.mort(input$state_choice, input$death_cause) +
    color.line.cluster() +
    theme.line.mort() +
    guides(
      color = guide_legend(reverse = T)
    )
}

# Mortality Rate Table
get.cluster.table <- function() {
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
}

cluster.table <- get.cluster.table()

kendall.cor <- kendall.func(mort.cluster.ord(), chr.data.2019)
