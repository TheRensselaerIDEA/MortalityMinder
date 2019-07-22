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


# Clean-up's
rm(
  list = c(
    "map.name.abbr.state",
    "map.name.fips.state"
  )
)
