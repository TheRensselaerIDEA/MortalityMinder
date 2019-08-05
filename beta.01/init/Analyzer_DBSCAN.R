library("dbscan")
library("fpc")
library("factoextra")


cdc.generateMortMat.state <- function(data, state.abbr) {
  data %>% 
    filter(state_abbr == state.abbr) %>%
    select(county_name, county_fips, death_rate, period) %>% 
    spread(key = period, value = death_rate)
}


cdc.generatePopMat.state <- function(data, state.abbr) {
  data %>% 
    filter(state_abbr == state.abbr) %>%
    select(county_name, county_fips, population, period) %>% 
    spread(key = period, value = population)
}


df.mort <- cdc.generateMortMat.state(cdc.data, "NY")
df.mort[df.mort$county_name == "Hamilton", -c(1, 2)] <- colMeans(df.mort[, -c(1, 2)])
km <- km.func(select(df.mort, -c(1, 2)), select(df.mort, c(1, 2)), 4)



d <- cdc.data %>% 
  filter(state_abbr == "NY") %>% 
  select(period, county_fips, population, death_num) %>% 
  left_join(km$cluster.labelled, by = "county_fips") %>% 
  select(km_cluster, period, population, death_num) %>% 
  group_by(km_cluster, period) %>% 
  summarise(
    population_sum = sum(population),
    death_num_sum = sum(death_num),
    death_rate_mean = death_num_sum / population_sum * 10^5
  ) %>% 
  select(km_cluster, period, m = death_rate_mean) %>% 
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
  ) %>%
  ggplot(aes(x = year_group, y = m, color = km_cluster)) +
  geom_point() +
  geom_line()
  
# db <- fpc::dbscan(select(df, -c(1, 2)), eps = 6, MinPts = 3)
# fviz_cluster(db, df, geom = "point")
# 
# mutate(df, db_cluster = factor(db$cluster + 1)) %>% 
#   gather(key = "period", value = "death_rate", 3:8) %>% 
#   select(-c(1, 2)) %>% 
#   group_by(db_cluster, period) %>% 
#   summarise(m = mean(death_rate)) %>% 
#   mutate(
#     year_group = dplyr::recode(
#       period,
#       `2000-2002` = 1,
#       `2003-2005` = 2,
#       `2006-2008` = 3,
#       `2009-2011` = 4,
#       `2012-2014` = 5,
#       `2015-2017` = 6
#     )
#   ) %>% 
#   ggplot(aes(x = year_group, y = m, color = db_cluster)) + 
#   geom_point() + 
#   geom_line()

