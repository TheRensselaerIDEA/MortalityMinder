# README:----------------------------------------------------------------------------------+
# DESC:
#   Importing and Preparing Poverty Indicator dataset from RHIhub
#   - Source Website: TODO
#   - Source File: TODO
#   - Source Path1: ~/data/AHRQ_Challenge/App/beta.03/data/RHI/all_states/Poverty.csv
#   - Source Path2: ~/data/AHRQ_Challenge/App/beta.03/data/RHI/all_states/Unemployment Rate.csv
#   - Source Path3: ~/data/AHRQ_Challenge/App/beta.03/data/RHI/all_states/Median Household Income.csv

# NOTE: 
#   All of the resulting data frames will have names starting with `rhi`
#   A mapping from state full name to state abbr is needed
#     - state-geocodes-v2016.xlsx

# PACKAGES: 
#   `dplyr`, `readr`, `readxl`, `stringr`, `lubridate`, `purrr`
# -----------------------------------------------------------------------------------------+

# Helper reader function
rhi.reader <- function(file, value.name) {
  read_csv(
      file = file,
      col_types = cols(
        metro_nonmetro = col_skip(),
        year = col_datetime(format = "%Y"),
        county_name = col_character(),
        state_abbr = col_character()
      )
    ) %>%
    dplyr::select(
      dplyr::everything(),
      county_fips = geoId,
      !!quo_name(value.name) := value
    ) %>%
    dplyr::mutate(
      # Clean county names
      county_name = str_replace(county_name, pattern = " County", replacement = ''),
      county_name = str_trim(county_name, side = "both"),

      # Clean year names
      year = str_replace(year, pattern = "-01-01", replacement = ''),
      year = str_trim(year, side = "both")
    )
}

# Load raw data files
rhi.poverty <- rhi.reader("../data/RHI/Poverty.csv", "poverty_rate")
rhi.healthyFood <- rhi.reader("../data/RHI/Low Access to Healthy Food.csv", "healthyFood")
rhi.unemploy <- rhi.reader("../data/RHI/Unemployment Rate.csv", "unemploy_rate")
rhi.medianIncome <- rhi.reader("../data/RHI/Median Household Income.csv", "medianIncome")
rhi.highSchool <- rhi.reader("../data/RHI/Population Without a High School Diploma.csv", "highSchool")

# Create mapping from state name to state_fips
rhi.map.state.nameAbbr <- tibble(state_name = state.name, state_abbr = state.abb)
rhi.map.state.fipsAbbr <- state.fips %>% 
  dplyr::select(state_fips = fips, state_abbr = abb) %>% 
  dplyr::mutate(
    state_fips = as.character(state_fips),
    state_fips = str_pad(state_fips, width = 2, side = "left", pad = '0')
  ) %>%
  dplyr::bind_rows(
    tibble(
      state_fips = "02",
      state_abbr = "AK"
    )
  ) %>% 
  base::unique()

# Combining data frames
rhi.data <- purrr::reduce(
    list(rhi.poverty, rhi.unemploy, rhi.medianIncome, rhi.healthyFood, rhi.highSchool),
    dplyr::full_join,
    by = c("state_abbr", "county_name", "year", "county_fips")
  ) %>%
  dplyr::left_join(rhi.map.state.nameAbbr, by = "state_abbr") %>% 
  dplyr::left_join(rhi.map.state.fipsAbbr, by = "state_abbr") %>% 
  dplyr::mutate(county_fips = str_replace(county_fips, "us-..-", state_fips)) %>% 
  dplyr::select(-c("state_name", "state_fips"))

rhi.namemap <- tibble(
  code = c("poverty_rate", "unemploy_rate", "medianIncome", "healthyFood"),
  name = c("Poverty Rate", "Unemployment Rate", "Median Income", "Low Healthy Food Access")
) %>% column_to_rownames("code")

# Clean-up's
rm(list = c("rhi.map.state.fipsAbbr", "rhi.map.state.nameAbbr", 
            "rhi.medianIncome", "rhi.poverty", "rhi.unemploy", "rhi.healthyFood", "rhi.highSchool"))
