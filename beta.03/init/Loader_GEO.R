map.name.abbr.state <- tibble(state_name = state.name, state_abbr = state.abb)
map.name.fips.state <- read_excel(
    path = "../data/GEO/state-geocodes-v2016.xlsx",
    trim_ws = T,
    skip = 6,
    col_types = c("skip", "skip", "text", "text"),
    col_names = c("state_fips", "state_name")
  ) %>% 
  dplyr::filter(state_fips != "00") %>% 
  dplyr::filter(state_fips != "11")


geo.namemap.state <- dplyr::left_join(
    map.name.abbr.state,
    map.name.fips.state,
    by = "state_name"
  )


# Special case, county.fips do not have Alaska data
geo.namemap.alaska <- cdc.data %>% 
  dplyr::filter(state_abbr == "AK", period == "2000-2002", death_cause == "Despair") %>% 
  dplyr::select(state_name, county_name, county_fips)


geo.namemap.county <- county.fips %>% 
  dplyr::rename(
    "county_fips" = "fips",
    "poly_name" = "polyname"
  ) %>%
  tidyr::separate(
    col = "poly_name", 
    into = c("state_name", "county_name"),
    sep = ','
  ) %>% 
  dplyr::mutate(
    "state_name" = stringr::str_to_title(state_name),
    "county_name" = stringr::str_to_title(county_name),
    "county_fips" = as.character(county_fips)
  ) %>% 
  dplyr::bind_rows(geo.namemap.alaska)

geo.namemap <- dplyr::left_join(geo.namemap.state, geo.namemap.county, by = "state_name") %>% 
  dplyr::select(state_name, state_abbr, state_fips, county_name, county_fips)


geo.map.fetch <- function(state.abbr, data) {
  urbnmapr::counties %>% 
    dplyr::mutate(county_fips = as.character(county_fips)) %>%
    dplyr::rename(state_abbr = state_abbv) %>%
    dplyr::filter(state_abbr == state.abbr) %>%
    dplyr::left_join(data, geo.data, by = "county_fips")
}


# Clean-up's
rm(
  list = c(
    "map.name.abbr.state",
    "map.name.fips.state",
    "geo.namemap.county",
    "geo.namemap.state",
    "geo.namemap.alaska"
  )
)
