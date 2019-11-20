# README:----------------------------------------------------------------------------------+
# DESC:
#   Importing and Preparing County Health Records and Roadmaps dataset
#   - Source Website: http://www.countyhealthrankings.org/explore-health-rankings
#   - Source File: 2019 County Health Rankings New York
#   - Source Path: ~/data/AHRQ_Challenge/Datasets/CDCWONDERDESPAIR/New York.xls

# NOTE: 
#   Only data from sheet2 - sheet5 is imported
#   All of the resulting data frames will have names starting with `chr`

# PACKAGES: 
#   `readxl`, `tidyr`, `dplyr`, `stringr`, `janitor`
# -----------------------------------------------------------------------------------------+

# Name maps
# Sheet 2:
chr.factorRanking.namemap <- tibble(
  code = c("health_outcomes", "health_factors"),
  name = c("Health Outcomes", "Health Overall Factors")
)

# Sheet 3:
chr.factorSubRanking.namemap <- tibble(
  code = c("life_length", "life_quality", "health_behavior", 
           "clinical_care", "socio_econ", "physical_env"),
  name = c("Length of Life", "Quality of Life", "Health Behaviors", 
           "Clinical Care", "Socio-Economic", "Physical Environment")
)

# Sheet 4:
chr.rankedMeasure.namemap <- tibble(
  code = c("years_of_potential_life_lost_rate", "ypll_rate_black", "ypll_rate_hispanic", "ypll_rate_white", 
           "pct_fair_or_poor", "physically_unhealthy_days", "mentally_unhealthy_days", "pct_lbw", "pct_lbw_black",
           "pct_lbw_hispanic", "pct_lbw_white", "pct_adult_smoking", "pct_adult_obesity", "food_environment_index",
           "pct_physically_inactive", "pct_with_access", "pct_excessive_drinking", "num_alcohol_impaired_driving_deaths",
           "num_driving_deaths", "pct_alcohol_impaired", "num_chlamydia_cases", "chlamydia_rate", "teen_birth_rate", 
           "teen_birth_rate_black", "teen_birth_rate_hispanic", "teen_birth_rate_white", "num_uninsured", "pct_uninsured",
           "num_primary_care_physicians", "pcp_rate", "pcp_ratio", "num_dentists", "dentist_rate", "dentist_ratio",
           "num_mental_health_providers", "mhp_rate", "mhp_ratio", "preventable_hosp_rate", "preventable_hosp_rate_black",
           "preventable_hosp_rate_hispanic", "preventable_hosp_rate_white", "pct_screened", "pct_screened_black", 
           "pct_screened_hispanic", "pct_screened_white", "pct_vaccinated", "pct_vaccinated_black", "pct_vaccinated_hispanic",
           "pct_vaccinated_white", "cohort_size", "graduation_rate", "num_some_college", "some_collage_population", "pct_some_college", 
           "num_unemployed", "labor_force", "pct_unemployed", "pct_children_in_poverty", "pct_children_in_poverty_black",
           "pct_children_in_poverty_hispanic", "pct_children_in_poverty_white", "80th_percentile_income", "20th_percentile_income",
           "income_ratio", "num_single_parent_households", "num_households", "pct_single_parent_households", "num_associations",
           "association_rate", "annual_average_violent_crimes", "violent_crime_rate", "num_injury_deaths", "injury_death_rate",
           "average_daily_pm25", "presence_of_violation", "pct_severe_housing_problems", "severe_housing_cost_burden",
           "overcrowding", "inadequate_facilities", "pct_drive_alone", "pct_drive_alone_black", "pct_drive_alone_hispanic",
           "pct_drive_alone_white", "num_workers_who_drive_alone", "pct_long_commute_drives_alone"),
  
  name = c("years of potential life lost rate", "years of potential life lost rate (Black)", 
           "years of potential life lost rate (Hispanic)", "years of potential life lost rate (White)", 
           "percentage of fair or poor health", "physically unhealthy days", "mentally unhealthy days", "percentage of low birthweight",
           "percentage of low birthweight (Black)", "percentage of low birthweight (Hispanic)", "percentage of low birthweight (White)",
           "percentage of adult smoking", "percentage of adult obesity", "food environment index", "percentage of physically inactive",
           "percentage of accessing to exercise opportunities", "percentage of excessive drinking", 
           "number of alcohol impaired driving deaths", "number of driving deaths", "percentage of alcohol impaired", 
           "number of chlamydia cases", "chlamydia rate", "teenagers birth rate", "teenagers birth rate (Black)", "teenagers birth rate (Hispanic)",
           "teenagers birth rate (White)", "number of uninsured", "percentage of uninsured", "number of primary care physicians",
           "primary care physicians rate", "primary care physicians ratio", "number of dentists", "dentist rate", "dentist ratio",
           "number of mental health providers", "mental health providers rate", "mental health providers ratio", "rate of preventable hospital stays",
           "rate of preventable hospital stays (Black)", "rate of preventable hospital stays (Hispanic)", "rate of preventable hospital stays (White)",
           "percentage of screened", "percentage of screened (Black)", "percentage of screened (Hispanic)", "percentage of screened (White)",
           "percentage of vaccinated", "percentage of vaccinated (Balck)", "percentage of vaccinated (Hispanic)", "percentage of vaccinated (White)",
           "cohort size", "graduation rate", "number of some college", "Some College Edu Population", "percentage of some college", "number of unemployed",
           "labor force", "percentage of unemployed", "percentage of children in poverty", "percentage of children in poverty (Black)",
           "percentage of children in poverty (Hispanic)", "percentage of children in poverty (White)", "80th percentile income", "20th percentile income",
           "income ratio", "number of single parent households", "number of households", "percentage of single parent households", 
           "number of associations", "association rate", "annual average violent crimes", "violent crime rate", "number of injury deaths",
           "injury death rate", "average daily pm25", "presence of violation", "percentage of severe housing problems", 
           "severe housing cost burden", "overcrowding", "inadequate facilities", "percentage of driving alone", "percentage of driving alone (Balck)",
           "percentage of driving alone (Hispanic)", "percentage of driving alone (White)", "number of workers who drive alone",
           "percentage of workers who drive alone in lone commute"
  )
)

# Sheet 5:
chr.additionMeasure.namemap <- tibble(
  code = c("life_expectancy","life_expectancy_black", "life_expectancy_hispanic","life_expectancy_white",
           "death_num_premature_age_adjusted_mortality", "age_adjusted_mortality", "age_adjusted_mortality_black","age_adjusted_mortality_hispanic",
           "age_adjusted_mortality_white","death_num_child_mortality","child_mortality_rate","child_mortality_rate_black",
           "child_mortality_rate_hispanic","child_mortality_rate_white","death_num_infant_mortality","infant_mortality_rate",
           "infant_mortality_rate_black","infant_mortality_rate_hispanic","infant_mortality_rate_white",
           "pct_frequent_physical_distress","pct_frequent_mental_distress","pct_diabetic", "num_hiv_cases", "hiv_prevalence_rate",
           "num_food_insecure","pct_food_insecure","num_limited_access","pct_limited_access","num_drug_overdose_deaths",
           "drug_overdose_mortality_rate","num_motor_vehicle_deaths","mv_mortality_rate","pct_insufficient_sleep",                       
           "num_uninsured_adults","pct_uninsured_adults","num_uninsured_children","pct_uninsured_children","other_pcp_rate",
           "other_pcp_ratio","pct_disconnected_youth","household_income","household_income_black","household_income_hispanic",
           "household_income_white","pct_free_or_reduced_lunch","seg_idx_black_or_white","seg_idx_non_white_or_white",
           "homicide_rate","num_firearm_fatalities","firearm_fatalities_rate","num_homeowners","pct_homeowners",
           "num_households_with_severe_cost_burden","pct_severe_housing_cost_burden","population","pct_less_18",
           "pct_65_and_over","num_african_american","pct_african_american","num_american_indian_or_alaskan_native",
           "pct_american_indian_or_alaskan_native","num_asian","pct_asian","num_native_hawaiian_or_other_pacific_islander",
           "pct_native_hawaiian_or_other_pacific_islander","num_hispanic","pct_hispanic","num_non_hispanic_white",
           "pct_non_hispanic_white","num_not_proficient_in_english","pct_not_proficient_in_english","pct_female",
           "num_rural","pct_rural"),
  
  name = c("Life expectancy","Life Expectancy (Black)","Life Expectancy (Hispanic)","Life Expectancy (White)",
           "Premature age-adjusted mortality", "Age-Adjusted Mortality", "Age-Adjusted Mortality (Black)","Age-Adjusted Mortality (Hispanic)",
           "Age-Adjusted Mortality (White)","Child total number of death","Child Mortality Rate",
           "Child Mortality Rate (Black)","Child Mortality Rate (Hispanic)","Child Mortality Rate (White)","Infant total number of death",
           "Infant Mortality Rate","Infant Mortality Rate (Black)","Infant Mortality Rate (Hispanic)","Infant Mortality Rate (White)",
           "Frequent Physical Distress Percentage","Frequent Mental Distress	Percentage","Diabetes Prevalence Percentage",
           "Number of HIV Cases", "HIV Prevalence Rate","Food Insecurity Number","Food Insecure Percentage","Total Number of Limited Access To Healthy Foods",
           "Limited access to healthy foods percentage","Total number of drug overdose deaths","Drug overdose deaths rate",
           "Total Number of Motor Vehicle Crash Deaths","Motor Vehicle Crash Deaths Rate","Insufficient Sleep Percentage",
           "Total Number of Uninsured Adults","Uninsured Adults Percentage","Total Number of Uninsured Children",
           "Uninsured children percentage","Other primary care providers rate","Other primary care providers ratio",
           "Disconnected Youth Percentage","Median Household Income","Household Income (Black)","Household Income (Hispanic)",
           "Household Income (White)","Children Eligible for Free or Reduced Price Lunch Percentage",
           "Residential Segregation - Black/White","Residential Segregation - Non-White/White","Homicides Rate",
           "Total Number of Firearm Fatalities","Firearm Fatalities Rate","Total Number of Homeowners","Homeownership Percentage",
           "Total Number of Households With Severe Housing Cost Burden","Households With Severe Housing Cost Burden Percentage",
           "Population","Percentage Younger Than 18","Percentage Older Than 65","Number of African American","African American percentage",
           "Number of American Indian/Alaskan Native","American Indian/Alaskan Native percentage","Number of Asian",
           "Asian percentage","Number of Native Hawaiian/Other Pacific Islander","Native Hawaiian/Other Pacific Islander percentage",
           "Number of Hispanic","Hispanic percentage","Number of Non-Hispanic White","Non-Hispanic White percentage",
           "Number of people Not Proficient in English","people Not Proficient in English percentage","female percentage",
           "Number of rural", "Rural percentage")
)

# Summary name mapping
chr.namemap.2019 <- bind_rows(
    list(
      chr.factorRanking.namemap, 
      chr.factorSubRanking.namemap,
      chr.rankedMeasure.namemap,
      chr.additionMeasure.namemap
    )
  )

chr.namemap.2019 %>% 
  #dplyr::mutate(name = stringr::str_to_title(name)) %>%
  as.data.frame() %>% 
  textshape::column_to_rownames("name") -> chr.namemap.inv.2019
write_rds(chr.namemap.inv.2019, "chr.namemap.inv.2019.rds")

chr.namemap.2019 %>% 
  dplyr::mutate(name = stringr::str_to_title(name)) %>%
  as.data.frame() %>% 
  textshape::column_to_rownames("code") -> chr.namemap.2019

# Write out binary version of dataframe to wd
write_rds(chr.namemap.2019, "chr.namemap.2019.rds")

# Data files
# Sheet 23
chr.reader.sheet23.2019 <- function(chr.file) { 
  # Sheet 2: Outcomes, factor ranking
  # First three rows are removed (headers)
  chr.factorRanking <- read_excel(
      path = chr.file, 
      sheet = 2,
      skip = 3,
      trim_ws = T,
      col_types = c("text", "text", "text", "numeric", "skip", "numeric", "skip"),
      col_names = c("county_fips", "state_name", "county_name", "health_outcomes", "health_factors")
    )
  
  # Sheet 3: Outcomes, factor sub ranking
  # First three rows are removed (headers)
  chr.factorSubRanking <- read_excel(
    path = chr.file, 
    sheet = 3,
    skip = 3,
    trim_ws = T,
    col_types = c(
      "text", "text", "text", 
      "numeric", "skip",
      "numeric", "skip",
      "numeric", "skip",
      "numeric", "skip",
      "numeric", "skip",
      "numeric", "skip"
    ),
    col_names = c(
      "county_fips", "state_name", "county_name",
      "life_length", "life_quality", "health_behavior", 
      "clinical_care", "socio_econ", "physical_env"
    )
  )
  
  return(dplyr::left_join(
    chr.factorRanking, 
    chr.factorSubRanking, 
    by = c("county_fips", "state_name", "county_name"))
  )
}

# Sheet 4:
chr.header.sheet4.2019 <- function(chr.file) {
  
  # Clean up headers
  chr.rankedMeasure.headers <- read_excel(
      path = chr.file,
      col_names = F,
      n_max = 2,
      sheet = 4,
      trim_ws = T
    ) %>% 
    data.table::transpose() %>% 
    
    # Make sure the defualt transposed name are V1 and V2
    dplyr::select(V1 = V1, V2 = V2) %>% 
    dplyr::mutate(
      # Manual cleaning of complex name
      # Some college population
      V2 = if_else(
        str_detect(V2, "Population"),
        paste("some_collage", "population", sep = '_'),
        V2
      ),
      
      # % Obese
      V2 = if_else(
        str_detect(V2, "% Obese"),
        paste("pct", str_to_lower(V1), sep = '_'),
        V2
      ),
      
      # % Smokers
      V2 = if_else(
        str_detect(V2, "% Smokers"),
        paste("pct", str_to_lower(V1), sep = '_'),
        V2
      ),
      
      # Substitute symbol with char
      V2 = str_replace_all(V2, '[ -]', '_'),
      V2 = str_replace_all(V2, "[().]", ''),
      V2 = str_replace_all(
        V2, c(
          '%' = "pct",
          '#' = "num",
          '<' = "less",
          '/' = "_or_"
        )
      ),
      V2 = str_replace_all(V2, "___", '_'),
      
      # Unify upper-lower case
      V1 = str_to_title(V1),
      V2 = str_to_lower(V2),
      
      # Miscs
      V2 = str_replace_all(
        V2,
        c(
          # DO NOT CHANGE ORDER
          "county" = "county_name",
          "fips" = "county_fips",
          "state" = "state_name"
        )
      ),
      
      # Do not need CI bounds, marked with "TO_REMOVE"
      V2 = str_replace(V2, "95pct_ci.*", "TO_REMOVE"),
      V2 = str_replace(V2, "z_score", "TO_REMOVE"),
      V2 = str_replace(V2, "unreliable", "TO_REMOVE")
    )
  
  return(chr.rankedMeasure.headers)
}

chr.reader.sheet4.2019 <- function(chr.file) {

  # Import data from sheet 4
  chr.rankedMeasure <- read_excel(
      path = chr.file, 
      sheet = 4,
      skip = 3,
      trim_ws = T,
      col_names = pull(chr.header.sheet4.2019(chr.file), "V2")
    ) %>% 
    
    ## Type conversion
    dplyr::mutate(
      county_name = as.character(county_name),
      county_fips = as.character(county_fips),
      state_name = as.character(state_name),
      mhp_ratio = parse_number(mhp_ratio),
      pcp_ratio = parse_number(pcp_ratio),
      presence_of_violation = parse_character(presence_of_violation),
      presence_of_violation = dplyr::recode(
        presence_of_violation,
        "Yes" = 1,
        "No" = 0,
        .defualt = NaN
      )
    ) %>%
    
    ## Get rid of unwanted cols
    dplyr::select(-starts_with("TO_REMOVE"))
  
  return(chr.rankedMeasure)
}

# Sheet 5
chr.header.sheet5.2019 <- function(chr.file) {

  # Sheet 5: Additional Measures
  chr.additionMeasure.headers <- read_excel(
      path = chr.file,
      col_names = F,
      n_max = 2,
      sheet = 5,
      trim_ws = T
    ) %>% 
    data.table::transpose() %>% 
    
    # Make sure the defualt transposed name are V1 and V2
    dplyr::select(V1 = V1, V2 = V2) %>%
    dplyr::mutate(
      # Manual cleaning of complex name
      # # Death
      V2 = if_else(
        str_detect(V2, "# Deaths"),
        paste("death_num", V1, sep = '_'),
        V2
      ),
      
      # # Uninsured
      V2 = if_else(
        str_detect(V2, "# Uninsured"),
        paste("num", V1, sep = '_'),
        V2
      ),
      
      # Segregation Index
      V2 = if_else(
        str_detect(V2, "Segregation"),
        paste("seg_idx", 
              str_extract(V1, "(?<=- ).*"), 
              sep = '_'),
        V2
      ),
  
      # Substitute symbol with char
      V2 = str_replace_all(V2, '[ -]', '_'),
      V2 = str_replace_all(V2, "[()]", ''),
      V2 = str_replace_all(
        V2, c(
          '%' = "pct",
          '#' = "num",
          '<' = "less",
          '/' = "_or_"
        )
      ),
      
      # Unify upper-lower case
      V1 = str_to_title(V1),
      V2 = str_to_lower(V2),
      
      # Miscs
      V2 = str_replace_all(
        V2,
        c(
          # DO NOT CHANGE ORDER
          "county" = "county_name",
          "fips" = "county_fips",
          "state" = "state_name"
        )
      ),
      
      # Do not need CI bounds, marked with "TO_REMOVE"
      V2 = str_replace(V2, "95pct_ci.*", 'TO_REMOVE')
    )

  # Dangerous:
  #   V2 cannot be changed
  #   Operations can only be done after uniforming cases
  chr.am.idx <- which(str_detect(chr.additionMeasure.headers$V2, "pct_uninsured"))
  chr.additionMeasure.headers$V2[chr.am.idx] <- paste(
    chr.additionMeasure.headers$V2[chr.am.idx],
    str_extract(chr.additionMeasure.headers$V2[chr.am.idx -1], "(?<=uninsured_).*"),
    sep = '_'
  )
  
  return(chr.additionMeasure.headers)
}

chr.reader.sheet5.2019 <- function(chr.file) {
  chr.additionMeasure <- read_excel(
      path = chr.file, 
      sheet = 5,
      skip = 3,
      trim_ws = T,
      col_names = pull(chr.header.sheet5.2019(chr.file), "V2")
    ) %>% 
    
    ## Type conversion
    dplyr::mutate(
      county_name = as.character(county_name),
      county_fips = as.character(county_fips),
      state_name = as.character(state_name),
      other_pcp_ratio = parse_number(other_pcp_ratio)
    ) %>%
  
    ## Get rid of unwanted cols
    dplyr::select(-starts_with("TO_REMOVE"))
  
  return(chr.additionMeasure)
}

# Combining data frames
chr.reader.2019 <- function(chr.file, keep.na = TRUE) {
  chr.data <- purrr::reduce(
      list(
        chr.reader.sheet23.2019(chr.file),
        chr.reader.sheet4.2019(chr.file),
        chr.reader.sheet5.2019(chr.file)
      ),
      dplyr::left_join,
      by = c("county_fips", "state_name", "county_name")
    ) %>% 
    as.data.frame()
  
  if (!keep.na) {
    return(as.data.frame(dplyr::select_if(chr.data, ~ !any(is.na(.)))))
  } else {
    return(chr.data)
  }
}

chr.reader.batch.2019 <- function(chr.files, keep.na = TRUE) {

  chr.data <- dplyr::bind_rows(
    lapply(
      chr.files, 
      chr.reader.2019,
      keep.na = TRUE
    )
  ) %>% as.data.frame()
  
  if (!keep.na) {
    return(as.data.frame(dplyr::select_if(chr.data, ~ !any(is.na(.)))))
  } else {
    return(chr.data)
  }
}


# Print `chr.files.2019` to see expected input, which is a vector
chr.files.2019 <- sapply(
  state.name,  # this is builtin variable
  function(state) {
    paste("../data/CHR/2019/", state, ".xls", sep = '')
  }
)

# Import data
chr.data.2019 <- readRDS("../data/CHR/CountyHealthRanking2019.rds") %>% 
  dplyr::select(
    -dplyr::contains("mortality"), 
    -dplyr::contains("deaths"), 
    -dplyr::contains("death"),
    -dplyr::contains("life"),
    -dplyr::contains("ypll")
  )

# Write out binary version of dataframe to wd
write_rds(chr.data.2019, "chr.data.2019.rds")

# Clean-up's
rm(
  list = c(
    "chr.rankedMeasure.namemap",
    "chr.additionMeasure.namemap",
    "chr.factorRanking.namemap",
    "chr.factorSubRanking.namemap"
  )
)
