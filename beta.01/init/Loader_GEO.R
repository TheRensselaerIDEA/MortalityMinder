map.name.abbr.state <- data_frame(state_name = state.name, state_abbr = state.abb)
map.name.fips.state <- read_excel(
    path = "../data/RHI/state-geocodes-v2016.xlsx",
    trim_ws = T,
    skip = 6,
    col_types = c("skip", "skip", "text", "text"),
    col_names = c("state_fips", "state_name")
  ) %>% 
  filter(state_fips != "00")


geo.namemap.state <- dplyr::left_join(
    map.name.abbr.state,
    map.name.fips.state,
    by = "state_name"
  ) %>% as.data.frame()


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
  )

geo.namemap <- dplyr::left_join(geo.namemap.county, geo.namemap.state, by = "state_name") %>% 
  dplyr::select(state_name, state_abbr, state_fips, county_name, county_fips)


geo.map.fetch <- function(state.abbr, namemap.state) {
  
  re <- dplyr::filter(namemap.state, state_abbr == state.abbr) %>% 
    dplyr::pull(state_name)

  maps::map("county", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% 
    fortify() %>%
    dplyr::filter(region == stringr::str_to_lower(re)) %>% 
    dplyr::rename(
      county_name = subregion, 
      state_name = region,
      geo_group = group
    ) %>% 
    dplyr::mutate(
      county_name = stringr::str_to_title(county_name),
      state_name = stringr::str_to_title(state_name)
    ) %>% as.data.frame()
}


geo.map.fetch.dev <- function(state.abbr, geo.namemap) {
  
  re <- dplyr::filter(geo.namemap, state_abbr == state.abbr) %>% 
    dplyr::pull(state_name) %>% 
    base::unique()
  
  map.data <- maps::map("county", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% 
    fortify() %>%
    dplyr::filter(region == stringr::str_to_lower(re)) %>% 
    dplyr::rename(
      county_name = subregion, 
      state_name = region,
      geo_group = group
    ) %>% 
    dplyr::mutate(
      county_name = stringr::str_to_title(county_name),
      state_name = stringr::str_to_title(state_name)
    ) %>% as.data.frame()
  
  dplyr::filter(geo.namemap, state_abbr == state.abbr) %>% 
    dplyr::select("county_fips", "county_name") %>% 
    dplyr::left_join(map.data, by = "county_name")
}


# Clean-up's
rm(
  list = c(
    "map.name.abbr.state",
    "map.name.fips.state",
    "geo.namemap.county"
  )
)
