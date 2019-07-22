# README:----------------------------------------------------------------------------------+
# DESC:
#   Importing and Preparing Poverty Indicator dataset from RHIhub
#   - Source Website: TODO
#   - Source File: TODO
#   - Source Path1: ~/data/AHRQ_Challenge/Datasets/RHIhub/Poverty-NY.csv
#   - Source Path2: ~/data/AHRQ_Challenge/Datasets/RHIhub/Median Household Income-NY.csv
#   - Source Path3: ~/data/AHRQ_Challenge/Datasets/RHIhub/Unemployment Rate-NY.csv

# NOTE: 
#   All of the resulting data frames will have names starting with `rhi`
#   A mapping from state full name to state abbr is needed
#     - state-geocodes-v2016.xlsx

# PACKAGES: 
#   `dplyr`, `readr`, `readxl`, `stringr`, `lubridate`, `purrr`
# -----------------------------------------------------------------------------------------+

# Load raw data files
rhi.poverty <- read_csv(
    file = "~/data/AHRQ_Challenge/Datasets/RHIhub/Poverty-NY.csv",
    col_types = cols(
      metro_nonmetro = col_character(),
      year = col_datetime(format = "%Y"),
      county_name = col_character()
    )
  ) %>% 
  
  ## Unify names
  rename(
    poverty_rate = value
  ) %>%
  
  ## Basic type conversion
  # NULL
  
  ## General data manipulation
  mutate(
    # Clean county names
    county_name = str_replace(county_name, pattern = " County", replacement = ''),
    county_name = str_trim(county_name, side = "both")
  )

rhi.unemploy <- read_csv(
    file = "../data/RHI/Unemployment Rate-NY.csv",
    col_types = cols(
      metro_nonmetro = col_character(),
      year = col_datetime(format = "%Y"),
      county_name = col_character()
    )
  ) %>% 
  
  ## Unify names
  rename(
    unemploy_rate = value
  ) %>%
  
  ## Basic type conversion
  # NULL
  
  ## General data manipulation
  mutate(
    # Clean county names
    county_name = str_replace(county_name, pattern = " County", replacement = ''),
    county_name = str_trim(county_name, side = "both")
  )

rhi.medianIncome <- read_csv(
    file = "../data/RHI/Median Household Income-NY.csv",
    col_types = cols(
      metro_nonmetro = col_character(),
      year = col_datetime(format = "%Y"),
      county_name = col_character()
    )
  ) %>% 
  
  ## Unify names
  rename(
    medianIncome = value
  ) %>%
  
  ## Basic type conversion
  # NULL
  
  ## General data manipulation
  mutate(
    # Clean county names
    county_name = str_replace(county_name, pattern = " County", replacement = ''),
    county_name = str_trim(county_name, side = "both")
  )

# Create mapping from state name to state_fips
rhi.map.state.nameAbbr <- data_frame(state_name = state.name, state_abbr = state.abb)
rhi.map.state.nameFips <- read_excel(
    path = "../data/RHI/state-geocodes-v2016.xlsx",
    trim_ws = T,
    skip = 6,
    col_types = c("skip", "skip", "text", "text"),
    col_names = c("state_fips", "state_name")
  ) %>% 
  filter(state_fips != "00")

# Combining data frames
rhi.data.byYear <- purrr::reduce(
    list(rhi.poverty, rhi.unemploy, rhi.medianIncome),
    dplyr::full_join,
    by = c("metro_nonmetro", "state_abbr", "county_name", "year", "geoId")
  ) %>%
  
  # Replacing geoId with county fips, with name `County.Code`
  # which matches the name of mortality file
  left_join(rhi.map.state.nameAbbr, by = "state_abbr") %>% 
  left_join(rhi.map.state.nameFips, by = "state_name") %>% 
  mutate(geoId = str_replace(geoId, "us-..-", state_fips)) %>% 
  select(year, county_name, county_fips = geoId, metro_nonmetro, 
         poverty_rate, unemploy_rate, medianIncome)

rhi.namemap <- data_frame(
    code = c("poverty_rate", "unemploy_rate", "medianIncome"),
    name = c("Poverty Rate", "Unemployment Rate", "Median Income")
  ) %>% column_to_rownames("code")

# Clean-up's
rm(list = c("rhi.map.state.nameFips", "rhi.map.state.nameAbbr", 
            "rhi.medianIncome", "rhi.poverty", "rhi.unemploy"))
