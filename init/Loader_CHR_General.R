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
chr.factorRanking.namemap <- data_frame(
  code = c("health_outcomes", "health_factors"),
  name = c("Health Outcomes", "Health Overall Factors")
)

# Sheet 3:
chr.factorSubRanking.namemap <- data_frame(
  code = c("life_length", "life_quality", "health_behavior", 
           "clinical_care", "socio_econ", "physical_env"),
  name = c("Length of Life", "Quality of Life", "Health Behaviors", 
           "Clinical Care", "Socio-Economic", "Physical Environment")
)

# Sheet 4:
chr.rankedMeasure.namemap <- data_frame(
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
  
  name = c("years of potential life lost rate", "years of potential life lost rate(black)", 
           "years of potential life lost rate(hispanic)", "years of potential life lost rate(white)", 
           "percentage of fair or poor", "physically unhealthy days", "mentally unhealthy days", "percentage of low birthweight",
           "percentage of low birthweight(black)", "percentage of low birthweight(hispanic)", "percentage of low birthweight(white)",
           "percentage of adult smoking", "persentage of adult obesity", "food environment index", "percentage of physically inactive",
           "percentage of accessing to exercise opportunities", "percentage of excessive drinking", 
           "number of alcohol impaired driving deaths", "number of driving deaths", "percentage of alcohol impaired", 
           "number of chlamydia cases", "chlamydia rate", "teenagers birth rate", "teenagers birth rate(black)", "teenagers birth rate(hispanic)",
           "teenagers birth rate(white)", "number of uninsured", "percentage of uninsured", "number of primary care physicians",
           "primary care physicians rate", "primary care physicians ratio", "number of dentists", "dentist rate", "dentist ratio",
           "number of mental health providers", "mental health providers rate", "mental health providers ratio", "rate of preventable hospital stays",
           "rate of preventable hospital stays(black)", "rate of preventable hospital stays(hispanic)", "rate of preventable hospital stays(white)",
           "percentage of screened", "percentage of screened(black)", "percentage of screened(hispanic)", "percentage of screened(white)",
           "percentage of vaccinated", "percentage of vaccinated(balck)", "percentage of vaccinated(hispanic)", "percentage of vaccinated(white)",
           "cohort size", "graduation rate", "number of some college", "Some Collage Edu Population", "percentage of some college", "number of unemployed",
           "labor force", "percentage of unemployed", "percentage of children in poverty", "percentage of children in poverty(black)",
           "percentage of children in poverty(hispanic)", "percentage of children in poverty(white)", "80th percentile income", "20th percentile income",
           "income ratio", "number of single parent households", "number of households", "percentage of single parent households", 
           "number of associations", "association rate", "annual average violent crimes", "violent crime rate", "number of injury deaths",
           "injury death rate", "average daily pm25", "presence of violation", "percentage of severe housing problems", 
           "severe housing cost burden", "overcrowding", "inadequate facilities", "percentage of driving alone", "percentage of driving alone(balck)",
           "percentage of driving alone(hispanic)", "percentage of driving alone(white)", "number of workers who drive alone",
           "percentage of workers who drive alone in lone commute"
  )
)

# Sheet 5:
chr.additionMeasure.namemap <- data_frame(
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
           "Infant mortality rate","Infant Mortality Rate (Black)","Infant Mortality Rate (Hispanic)","Infant Mortality Rate (White)",
           "Frequent physical distress percentage","Frequent mental distress	percentage","Diabetes prevalence percentage",
           "Number of HIV Cases", "HIV prevalence rate","Food insecurity number","Food Insecure percentage","total number of Limited access to healthy foods",
           "Limited access to healthy foods percentage","Total number of drug overdose deaths","Drug overdose deaths	rate",
           "Total number of motor vehicle crash deaths","Motor vehicle crash deaths rate","Insufficient sleep percentage",
           "Total number of uninsured adults","Uninsured adults percentage","Total number of uninsured children",
           "Uninsured children percentage","Other primary care providers rate","Other primary care providers ratio",
           "Disconnected youth percentage","Median household income","Household income (Black)","Household income (Hispanic)",
           "Household income (White)","Children eligible for free or reduced price lunch percentage",
           "Residential segregation - black/white","Residential segregation - non-white/white","Homicides rate",
           "Total number of firearm fatalities","Firearm fatalities rate","Total number of Homeowners","Homeownership percentage",
           "Total number of households with Severe housing cost burden","households with Severe housing cost burden percentage",
           "Population","Percentage younger than 18","Percentage older than 65","Number of African American","African American percentage",
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
  ) %>% 
  dplyr::mutate(name = stringr::str_to_title(name)) %>%
  as.data.frame()

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
      # Some collage population
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
      pcp_ratio = parse_number(pcp_ratio)
    ) %>% 
    
    ## Dynamic type conversion
    dplyr::mutate_at(
      vars(dplyr::matches("presence_of_violation")),
      function(x) {
        parse_character(x) %>% 
          dplyr::recode(
            x,
            "Yes" = 1,
            "No" = 0,
            .defualt = NaN
          )
      }
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
chr.data.2019 <- readRDS("../data/CHR/2019/CountyHealthRanking2019.rds")

# Clean-up's
rm(
  list = c(
    "chr.rankedMeasure.namemap",
    "chr.additionMeasure.namemap",
    "chr.factorRanking.namemap",
    "chr.factorSubRanking.namemap"
  )
)
