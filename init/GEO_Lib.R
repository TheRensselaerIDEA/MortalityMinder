#' Used in app.R for maps based on urbnmapr
#' 
#' @note Pulled from Loader_GEO.R. Depends on urbnmapr
#'
#' @param state.abbr 
#' @param data 
#'
#' @return a plot used to visualize optimal k for k means clustering
#'
#' @examples
#' 
#' 
#' 
#' @author John Erickson (based on others)
#' @export

geo.map.fetch <- function(state.abbr, data) {
  if (state.abbr != "US") {
    urbnmapr::counties %>% 
      dplyr::mutate(county_fips = as.character(county_fips)) %>%
      dplyr::rename(state_abbr = state_abbv) %>%
      dplyr::filter(state_abbr == state.abbr) %>%
      dplyr::left_join(data, geo.data, by = "county_fips")
  }else {
    urbnmapr::counties %>% 
      dplyr::mutate(county_fips = as.character(county_fips)) %>%
      dplyr::left_join(data, geo.data, by = "county_fips")
  }
  
}

