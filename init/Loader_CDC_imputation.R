# README:----------------------------------------------------------------------------------+
# DESC:
#   Importing and Preparing CDC_WONDER Death of Despaire Mortality dataset of NY state
#   - Source Website: CDC wonder not compressed

# NOTE: 
#   Year 2000-2017

# PACKAGES: 
#   `stringr`, `tidyr`, `dplyr`, `readr`, `janitor`, `Amelia`
# -----------------------------------------------------------------------------------------+

# Helper function for reading CDC_WONDER deaths rate data
# Three years in a row
# Skip `Notes` and `Age Adjusted Rate` and `Crude Rate`
cdc.reader <- function(cdc.file, cdc.period, death.cause, suppress.sub) {
  ## Import raw data
  readr::read_delim(
    file = cdc.file,
    delim = '\t',
    col_types = cols(
      "Notes" = col_skip(),
      # "Age Adjusted Rate" = col_skip(),
      "Crude Rate" = col_skip(),
      "Population" = col_character(),
      "Deaths" = col_character(),
      "2013 Urbanization Code" = col_skip()
    )
  ) %>% 
    
    ## Unify names
    janitor::clean_names() %>% 
    dplyr::select(
      county_name = county,
      county_fips = county_code,
      state_name = state,
      state_fips = state_code,
      death_num = deaths,
      population = population,
      urban_2013 = x2013_urbanization
    ) %>% 
    
    ## Basic type conversion
    dplyr::mutate(
      county_name = as.character(county_name),
      county_fips = as.character(county_fips),
      death_num = parse_number(death_num, na = c("Missing", "Suppressed", "Unreliable")),
      population = parse_number(population, na = c("Missing", "Suppressed", "Unreliable")),
      urban_2013 = as.factor(urban_2013)
    ) %>%
    
    ## General data manipulation
    dplyr::mutate(
      # Recode `Suppressed` to `1` 
      # Supressed counties have 9 or less deaths so we set it to 0.1
      # In this step, supressed entry has been substituded to NA
      death_num = dplyr::if_else(is.na(death_num), suppress.sub, death_num),
      # population = ifelse(is.na(population), -1, population),
      population = dplyr::if_else(is.na(population), suppress.sub, population),
      
      # Calculate crude rate by hand
      # Mortality rate is number of deaths per 10000 people
      death_rate = dplyr::if_else(is.na(population), NaN, (death_num / population) * 10^5),
      
      # Addition of all kinds of "stamps"
      # Adding time stamp
      period = cdc.period,
      
      # Add cause of death
      death_cause = death.cause
    ) %>%
    
    ## General tidying of data
    dplyr::mutate(
      # Clean county names
      county_name = stringr::str_replace(county_name, " Parish", ''),
      county_name = stringr::str_replace(county_name, pattern = " County, ", replacement = ','),
      county_name = stringr::str_trim(county_name, side = "both"),
      county_fips = stringr::str_trim(county_fips, side = "both")
    ) %>%
    tidyr::separate(
      col = county_name,
      into = c("county_name", "state_abbr"),
      sep = ','
    ) %>% 
    dplyr::mutate(
      state_abbr = stringr::str_trim(state_abbr, side = "both"),
      state_name = stringr::str_trim(state_name, side = "both")
    ) %>%
    dplyr::select(
      # Rearrangement
      period, state_name, state_abbr, county_name, 
      county_fips, urban_2013, population, death_num, death_rate, death_cause
    ) %>% 
    as.data.frame()
}


# Helper function for reading CDC_WONDER deaths rate data in STATES level
# Three years in a row
# Skip `Notes` and `Age Adjusted Rate` and `Crude Rate`
cdc.reader.state <- function(cdc.state.file, cdc.period, death.cause, suppress.sub) {
  ## Import raw data
  readr::read_delim(
    file = cdc.state.file,
    delim = '\t',
    col_types = cols(
      "Notes" = col_skip(),
      # "Age Adjusted Rate" = col_skip(),
      "Crude Rate" = col_skip(),
      "Population" = col_character(),
      "Deaths" = col_character()
    )
  ) %>% 
    
    ## Unify names
    janitor::clean_names() %>% 
    dplyr::select(
      state_name = state,
      state_fips = state_code,
      death_num = deaths,
      population = population
    ) %>% 
    
    ## Basic type conversion
    dplyr::mutate(
      state_name = as.character(state_name),
      state_fips = as.character(state_fips),
      death_num = parse_number(death_num, na = c("Missing", "Suppressed", "Unreliable")),
      population = parse_number(population, na = c("Missing", "Suppressed", "Unreliable"))
    ) %>%
    
    ## General data manipulation
    dplyr::mutate(
      # Recode `Suppressed` to `1` 
      # Supressed counties have 9 or less deaths so we set it to 0.1
      # In this step, supressed entry has been substituded to NA
      death_num = dplyr::if_else(is.na(death_num), suppress.sub, death_num),
      # population = ifelse(is.na(population), -1, population),
      population = dplyr::if_else(is.na(population), suppress.sub, population),
      
      # Calculate crude rate by hand
      # Mortality rate is number of deaths per 10000 people
      death_rate = (death_num / population) * 10^5,
      
      # Addition of all kinds of "stamps"
      # Adding time stamp
      period = cdc.period,
      
      # Add cause of death
      death_cause = death.cause
    ) %>%
    
    ## General tidying of data
    dplyr::mutate(
      state_name = stringr::str_trim(state_name, side = "both"),
      state_abbr = as.character(state.name.abbr[state_name])
    ) %>%
    dplyr::select(
      # Rearrangement
      period, state_name, state_abbr, state_fips, population, death_num, death_rate, death_cause
    ) %>% 
    as.data.frame()
}


## Importing and Conversion
cdc.reader.batch <- function(cdc.files, cdc.periods, cdc.cause, suppress.sub = NaN) {
  
  if (length(cdc.files) != length(cdc.periods)) {
    stop("Lengths of cdc.files and cdc.periods are not equal")
  }
  
  # Collection of dataframes (in list)
  cdc.collection.list <- list()
  for (cdc.i in 1:length(cdc.files)) {
    cdc.collection.list[[cdc.i]] <- cdc.reader(
      cdc.files[cdc.i], 
      cdc.periods[cdc.i], 
      cdc.cause,
      suppress.sub
    )
  }
  
  return(dplyr::bind_rows(cdc.collection.list))
}


cdc.reader.state.batch <- function(cdc.state.files, cdc.periods, cdc.cause, suppress.sub = NaN) {
  
  if (length(cdc.state.files) != length(cdc.periods)) {
    stop("Lengths of cdc.files and cdc.periods are not equal")
  }
  
  # Collection of dataframes (in list)
  cdc.collection.list <- list()
  for (cdc.i in 1:length(cdc.state.files)) {
    cdc.collection.list[[cdc.i]] <- cdc.reader.state(
      cdc.state.files[cdc.i], 
      cdc.periods[cdc.i], 
      cdc.cause,
      suppress.sub
    )
  }
  
  return(dplyr::bind_rows(cdc.collection.list))
}


cdc.mort.mat <- function(cdc.data.long, state.abbr, death.cause = "Despair") {
  
  tmp <- cdc.data.long
  if (state.abbr != "ALL") {
    tmp <- dplyr::filter(cdc.data.long, state_abbr == state.abbr)
  }
  
  dplyr::filter(tmp, death_cause == death.cause) %>%
    tidyr::drop_na(county_fips) %>%
    dplyr::select(county_fips, death_rate, period) %>%
    tidyr::spread(key = period, value = death_rate) %>%
    dplyr::arrange(county_fips)
}

cdc.mort.state.mat <- function(cdc.data.state.long, state.abbr, death.cause = "Despair") {
  
  tmp <- cdc.data.state.long
  if (state.abbr != "ALL") {
    tmp <- dplyr::filter(cdc.data.state.long, state_abbr == state.abbr)
  }
  
  dplyr::filter(tmp, death_cause == death.cause) %>%
    tidyr::drop_na(state_fips) %>%
    dplyr::select(state_fips, state_abbr, death_rate, period) %>%
    tidyr::spread(key = period, value = death_rate) %>%
    dplyr::arrange(state_fips)
}

cdc.pop.mat <- function(cdc.data.long, state.abbr, death.cause = "Despair") {
  tmp <- cdc.data.long
  if (state.abbr != "ALL") {
    tmp <- dplyr::filter(cdc.data.long, state_abbr == state.abbr)
  }
  
  tmp1 <- dplyr::filter(tmp, death_cause == death.cause) %>%
    tidyr::drop_na(county_fips) %>%
    dplyr::select(county_fips, state_abbr, death_num, period) %>%
    tidyr::spread(key = period, value = death_num) %>%
    dplyr::arrange(county_fips)
  
  tmp2 <- dplyr::filter(tmp, death_cause == death.cause) %>%
    tidyr::drop_na(county_fips) %>%
    dplyr::select(county_fips, population, period) %>%
    tidyr::spread(key = period, value = population) %>%
    dplyr::arrange(county_fips)
  
  dplyr::inner_join(tmp1, tmp2, by = "county_fips", 
                    suffix = c(".death_num", ".population"))
}

cdc.fill.by.excl <- function(cdc.data.cause, cdc.data.allcause, cdc.data.excluded) {
  
  num.allcause <- cdc.data.allcause %>% dplyr::select(period, county_fips, death_num)
  num.excluded <- cdc.data.excluded %>% dplyr::select(period, county_fips, death_num)
  
  mutate.cause <- cdc.data.cause %>% 
    left_join(num.allcause, by = c("county_fips", "period"), suffix=c("", ".allcause")) %>% 
    left_join(num.excluded, by = c("county_fips", "period"), suffix=c("", ".excluded"))
  
  # first, verify that the excluded data is mostly right (for non suppressed)
  incorrect <- which( (mutate.cause$death_num.allcause - mutate.cause$death_num.excluded) != 
                        mutate.cause$death_num )
  
  if (length(incorrect) > 0) {
    stop("ERROR: The excluded data set does not match!")
  }
  
  mutate.cause <- mutate.cause %>% 
    dplyr::mutate( 
      death_num = dplyr::if_else(is.na(death_num), 
                                 death_num.allcause - death_num.excluded, 
                                 death_num)) %>%
    
    dplyr::mutate( 
      death_rate = dplyr::if_else(is.na(population), 
                                  death_rate, 
                                  death_num/population*10^5) ) %>%
    
    # Rearrangement
    dplyr::arrange(county_fips) %>% 
    
    dplyr::select(-death_num.allcause, -death_num.excluded)
  
  return(mutate.cause)  

}



# the function that reads the county long data (and suppliments of state long data)
cdc.impute <- function(cdc.data.long, cdc.data.state.long, state.abbr, death.cause = "Despair") {
  
  # > cdc.data.wide <- cdc.mort.mat(cdc.data, "ALL", "Despair")
  # > cdc.supp.wide <- cdc.pop.mat(cdc.data, "ALL", "Despair")
  # > cdc.data.state.wide <- cdc.mort.state.mat(cdc.data.state, "ALL", "Despair")
  
  cdc.data.wide <- cdc.mort.mat(cdc.data.long, state.abbr, death.cause)
  cdc.data.wide[is.na(cdc.data.wide)] <- NaN
  
  cdc.supp.wide <- cdc.pop.mat(cdc.data.long, state.abbr, death.cause)
  
  cdc.data.state.wide <- cdc.mort.state.mat(cdc.data.state.long, state.abbr, death.cause)
  
  county.data <- unique( cdc.data.long %>% 
    dplyr::select(period, state_name, state_abbr, county_name, county_fips, urban_2013, population) 
    )
  
  rownames(cdc.data.state.wide) <- cdc.data.state.wide$state_abbr
  
  hi_bound <- as.data.frame(matrix(Inf, nrow=nrow(cdc.data.wide), ncol=6))
  
  colnames(hi_bound) <- colnames(cdc.data.wide)[-1]
  rownames(hi_bound) <- cdc.data.wide$county_fips
  
  random1 <- hi_bound
  random2 <- hi_bound
  
  set.seed(88)
  
  miss_index <- which(is.na(cdc.data.wide[,-1]),arr.ind=TRUE)
  get_pop_index <- t(matrix(c(0,8), nrow=ncol(miss_index), ncol=nrow(miss_index)))
  hi_bound[miss_index] <- 
    dplyr::if_else( is.na(cdc.supp.wide[(miss_index+get_pop_index)]), Inf,
                    9 / as.numeric( cdc.supp.wide[(miss_index+get_pop_index)] ) * 10^5)
    
  random1[miss_index] <- 
    dplyr::if_else( is.na(cdc.supp.wide[(miss_index+get_pop_index)]), Inf,
                    sample(c(1:4), 1, replace=TRUE) / 
                      as.numeric( cdc.supp.wide[(miss_index+get_pop_index)] ) * 10^5)

  random2[miss_index] <- 
    dplyr::if_else( is.na(cdc.supp.wide[(miss_index+get_pop_index)]), Inf,
                    sample(c(5:8), 1, replace=TRUE) / 
                      as.numeric( cdc.supp.wide[(miss_index+get_pop_index)] ) * 10^5)
  
  # imputation
  
  cdc.states.split <- split(cdc.data.wide, cdc.supp.wide$state_abbr) 
  
  complt.df <- data.frame()
  
  # for each state
  for (i in 1:length(cdc.states.split)) {
    
    state_name <- names(cdc.states.split[i])
    state <- cdc.states.split[[i]]
    # state.mat <- data.matrix( state[, -1] )
    rownames(state) <- state$county_fips
    
    
    # category a)
    
    complt <- state[complete.cases(state), -1] 
    
    # category b)
    
    part <- state[ !complete.cases(state) & 
                    rowSums(is.na(state)) <= 4 , -1] 
    
    state_mean <- cdc.data.state.wide[state_name,c(-1,-2)] 
    
    #if (nrow(complt) == nrow(state)) {
    #  complt.df <- rbind(complt.df, complt)
    #  next
    #}
    
    if (nrow(part) > 0) {
      for (j in 1:nrow(part)) {
        county <- part[j,]
        complt <- rbind(complt, county)
        
        miss_i <- which(is.na(county))
        lo_bound <- rep(0, length(miss_i))
        up_bound <- as.numeric(hi_bound[rownames(county), miss_i])

        bound <- cbind(miss_i, lo_bound, up_bound)
        
        county_nonmiss_mean <- rowMeans(county[,-miss_i])
        
        amelia_impute <- Amelia::amelia(complt, m=2, parallel="multicore", bounds=bound, p2s=0)
        amelia_out1 <- amelia_impute$imputations$imp1
        amelia_out2 <- amelia_impute$imputations$imp2
        
        amelia_list <- list(amelia_out1, amelia_out2)
        
        amelia_result <- Reduce("+", amelia_list) / length(amelia_list)
        
        # If both imputation fails, replace it by state mean or random rate that is closer
        # to other non-missing mortality rates in this county
        if (length(amelia_result) == 0) {
          
          less_i <- which( is.na(complt[rownames(county),]) &
                           (state_mean <= hi_bound[rownames(county),]) )
          random_i <- which( is.na(complt[rownames(county),]) &
                             (state_mean > hi_bound[rownames(county),]) )
          
          complt[rownames(county),less_i] <- state_mean[,less_i]
          
          complt[rownames(county),random_i] <- 
            sapply(rbind(random1[rownames(county),], random2[rownames(county),]), 
                   function(x) {
                     x[ which( abs(x-county_nonmiss_mean) == 
                                 min(abs(x-county_nonmiss_mean) ) ) ]
                   }
            )[random_i]
        }
        # If one of the imputations failed, use the successful one
        else if (xor(length(amelia_out1) == 1, length(amelia_out2) == 1)) {
          if (length(amelia_out1) == 1){
            amelia_result <- amelia_out2
          }
          else{
            amelia_result <- amelia_out1
          }
          complete <- amelia_result
        }
        # If both imputations succeed, use the mean output
        else {
          complt <- amelia_result
        }
      }
    }

    # category c)
    
    miss_more <- state[ rowSums(is.na(state)) > 4 , -1] 
    
    if (nrow(miss_more) > 0) {
      for (k in 1:nrow(miss_more)) {
        county <- miss_more[k,]
        less_i <- which( is.na(county) & 
                           (state_mean <= hi_bound[rownames(county),]) )
        random_i <- which( is.na(county) & 
                             (state_mean > hi_bound[rownames(county),]) )
        county[,less_i] <- state_mean[,less_i]
        
        # county[,random_i] <- 
        #   colMeans(rbind(random1[rownames(county),], 
        #                  random2[rownames(county),])
        #   )[random_i]
          
        county[,random_i] <- 
          sapply(rbind(random1[rownames(county),], random2[rownames(county),]), 
                 function(x) {
                   x[ which( abs(x-rowMeans(state_mean)) == 
                               min(abs(x-rowMeans(state_mean)) ) ) ]
                 }
          )[random_i]
        
        complt <- rbind(complt, county)
      }
    }
    
    complt.df <- rbind( complt.df, complt )
    print(i)
  }
    
  final.df <- dplyr::mutate(complt.df, county_fips = rownames(complt.df)) %>% 
    tidyr::gather(key="period", value="death_rate", 1:6) %>% 
    dplyr::inner_join(county.data, by=c("period"="period", "county_fips"="county_fips")) %>%
    
    # convert the death_rate to the nearest integer num/pop rate
    dplyr::mutate( death_num = round(death_rate*population/10^5) ) %>%
    
    # dplyr::mutate( death_num =
    #                  dplyr::if_else(round(death_rate*population/10^5) < 1,
    #                                 1, round(death_rate*population/10^5) ) ) %>%
    dplyr::mutate( 
      death_rate = dplyr::if_else(is.na(population), death_rate, death_num/population*10^5),
      death_cause = death.cause ) %>%
    
    dplyr::mutate(
      death_rate = dplyr::if_else(death_rate == 0, 
                                  min(death_rate[death_rate>0]),
                                  death_rate) ) %>%
    
    # Rearrangement
    dplyr::arrange(county_fips) %>% 
    dplyr::select(
      # Rearrangement
      period, state_name, state_abbr, county_name, 
      county_fips, urban_2013, population, death_num, death_rate, death_cause
    ) %>% 
    as.data.frame()
  
  # ================ older version ================== #
  # final.df <- 
  #   
  #   # combine all info for counties (fips, state, death_num, population, etc.)
  #   dplyr::mutate(complt.df,county_fips = rownames(complt.df)) %>% 
  #   dplyr::inner_join(dplyr::select(cdc.supp.wide, -state_abbr), by="county_fips") %>%
  #   dplyr::inner_join(county.data, by="county_fips") %>% 
  #   
  #   # before gather, combine all death info, easier to structure
  #   tidyr::unite(`2000-2002`, `2000-2002`, `2000-2002.death_num`, `2000-2002.population`, sep=",") %>% 
  #   tidyr::unite(`2003-2005`, `2003-2005`, `2003-2005.death_num`, `2003-2005.population`, sep=",") %>% 
  #   tidyr::unite(`2006-2008`, `2006-2008`, `2006-2008.death_num`, `2006-2008.population`, sep=",") %>% 
  #   tidyr::unite(`2009-2011`, `2009-2011`, `2009-2011.death_num`, `2009-2011.population`, sep=",") %>% 
  #   tidyr::unite(`2012-2014`, `2012-2014`, `2012-2014.death_num`, `2012-2014.population`, sep=",") %>% 
  #   tidyr::unite(`2015-2017`, `2015-2017`, `2015-2017.death_num`, `2015-2017.population`, sep=",") %>% 
  #   
  #   # gather we convert data from wide to long
  #   tidyr::gather(key="period", value="death_info", 1:6) %>%    
  #   
  #   # seperate the death info so we have three columns
  #   tidyr::separate("death_info", c("death_rate","death_num","population"), sep=",") %>% 
  #   
  #   # make int death_num
  #   dplyr::mutate( death_rate = as.numeric(death_rate), 
  #                  death_num = as.numeric(death_num), 
  #                  population = as.numeric(population) ) %>%
  #   
  #   # convert the death_rate to the nearest integer num/pop rate
  #   dplyr::mutate( death_num = 
  #                    dplyr::if_else(round(death_rate*population/10^5) < 1,
  #                                   1, round(death_rate*population/10^5) ) ) %>%
  #   dplyr::mutate( 
  #     death_rate = dplyr::if_else(is.na(population), death_rate, death_num/population*10^5),
  #     death_cause = death.cause ) %>%
  #   
  #   # Rearrangement
  #   dplyr::arrange(county_fips) %>% 
  #   dplyr::select(
  #     # Rearrangement
  #     period, state_name, state_abbr, county_name, 
  #     county_fips, urban_2013, population, death_num, death_rate, death_cause
  #   ) %>% 
  #   as.data.frame()
  # ================================================= #
  
}

cdc.periods <- c(
  "2000-2002", "2003-2005", "2006-2008", 
  "2009-2011", "2012-2014", "2015-2017"
)

cdc.files.assault <- c(
  "../data/CDC/assault/Underlying Cause of Death, 2000-2002.txt",
  "../data/CDC/assault/Underlying Cause of Death, 2003-2005.txt",
  "../data/CDC/assault/Underlying Cause of Death, 2006-2008.txt",
  "../data/CDC/assault/Underlying Cause of Death, 2009-2011.txt",
  "../data/CDC/assault/Underlying Cause of Death, 2012-2014.txt",
  "../data/CDC/assault/Underlying Cause of Death, 2015-2017.txt"
)

cdc.files.cancer <- c(
  "../data/CDC/cancer/Underlying Cause of Death, 2000-2002.txt",
  "../data/CDC/cancer/Underlying Cause of Death, 2003-2005.txt",
  "../data/CDC/cancer/Underlying Cause of Death, 2006-2008.txt",
  "../data/CDC/cancer/Underlying Cause of Death, 2009-2011.txt",
  "../data/CDC/cancer/Underlying Cause of Death, 2012-2014.txt",
  "../data/CDC/cancer/Underlying Cause of Death, 2015-2017.txt"
)

cdc.files.despair <- c(
  "../data/CDC/despair/Underlying Cause of Death, 2000-2002.txt",
  "../data/CDC/despair/Underlying Cause of Death, 2003-2005.txt",
  "../data/CDC/despair/Underlying Cause of Death, 2006-2008.txt",
  "../data/CDC/despair/Underlying Cause of Death, 2009-2011.txt",
  "../data/CDC/despair/Underlying Cause of Death, 2012-2014.txt",
  "../data/CDC/despair/Underlying Cause of Death, 2015-2017.txt"
)

cdc.files.cardiovascular <- c(
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2000-2002.txt",
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2003-2005.txt",
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2006-2008.txt",
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2009-2011.txt",
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2012-2014.txt",
  "../data/CDC/cardiovascular/Underlying Cause of Death, 2015-2017.txt"
)

cdc.files.allcause <- c(
  "../data/CDC/all_cause/Underlying Cause of Death, 2000-2002.txt",
  "../data/CDC/all_cause/Underlying Cause of Death, 2003-2005.txt",
  "../data/CDC/all_cause/Underlying Cause of Death, 2006-2008.txt",
  "../data/CDC/all_cause/Underlying Cause of Death, 2009-2011.txt",
  "../data/CDC/all_cause/Underlying Cause of Death, 2012-2014.txt",
  "../data/CDC/all_cause/Underlying Cause of Death, 2015-2017.txt"
)

# state data name map #

state.name.abbr <- list(
                "Alabama"     =    "AL",
                 "Alaska"     =    "AK",
                "Arizona"     =    "AZ",
               "Arkansas"     =    "AR",
             "California"     =    "CA",
               "Colorado"     =    "CO",
            "Connecticut"     =    "CT",
               "Delaware"     =    "DE",
   "District of Columbia"     =    "DC",
                "Florida"     =    "FL",
                "Georgia"     =    "GA",
                 "Hawaii"     =    "HI",
                  "Idaho"     =    "ID",
               "Illinois"     =    "IL",
                "Indiana"     =    "IN",
                   "Iowa"     =    "IA",
                 "Kansas"     =    "KS",
               "Kentucky"     =    "KY",
              "Louisiana"     =    "LA",
                  "Maine"     =    "ME",
               "Maryland"     =    "MD",
          "Massachusetts"     =    "MA",
               "Michigan"     =    "MI",
              "Minnesota"     =    "MN",
            "Mississippi"     =    "MS",
               "Missouri"     =    "MO",
                "Montana"     =    "MT",
               "Nebraska"     =    "NE",
                 "Nevada"     =    "NV",
          "New Hampshire"     =    "NH",
             "New Jersey"     =    "NJ",
             "New Mexico"     =    "NM",
               "New York"     =    "NY",
         "North Carolina"     =    "NC",
           "North Dakota"     =    "ND",
                   "Ohio"     =    "OH",
               "Oklahoma"     =    "OK",
                 "Oregon"     =    "OR",
           "Pennsylvania"     =    "PA",
           "Rhode Island"     =    "RI",
         "South Carolina"     =    "SC",
           "South Dakota"     =    "SD",
              "Tennessee"     =    "TN",
                  "Texas"     =    "TX",
                   "Utah"     =    "UT",
                "Vermont"     =    "VT",
               "Virginia"     =    "VA",
             "Washington"     =    "WA",
          "West Virginia"     =    "WV",
              "Wisconsin"     =    "WI",
                "Wyoming"     =    "WY"
)

# state data location

cdc.files.despair.state <- c(
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2000-2002.txt",
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2003-2005.txt",
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2006-2008.txt",
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2009-2011.txt",
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2012-2014.txt",
  "../data/CDC/despair_state/Underlying Cause of Death, Despair, State, 2015-2017.txt"
)

cdc.files.assault.state <- c(
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2000-2002.txt",
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2003-2005.txt",
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2006-2008.txt",
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2009-2011.txt",
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2012-2014.txt",
  "../data/CDC/assault_state/Underlying Cause of Death, Assault, State, 2015-2017.txt"
)

cdc.files.cancer.state <- c(
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2000-2002.txt",
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2003-2005.txt",
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2006-2008.txt",
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2009-2011.txt",
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2012-2014.txt",
  "../data/CDC/cancer_state/Underlying Cause of Death, Cancer, State, 2015-2017.txt"
)

cdc.files.cardiovascular.state <- c(
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2000-2002.txt",
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2003-2005.txt",
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2006-2008.txt",
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2009-2011.txt",
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2012-2014.txt",
  "../data/CDC/cardiovascular_state/Underlying Cause of Death, State, 2015-2017.txt"
)

cdc.files.allcause.state <- c(
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2000-2002.txt",
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2003-2005.txt",
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2006-2008.txt",
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2009-2011.txt",
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2012-2014.txt",
  "../data/CDC/allcause_state/Underlying Cause of Death, All Causes, State, 2015-2017.txt"
)

# excluded data

cdc.files.despair.excluded <- c(
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2000-2002.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2003-2005.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2006-2008.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2009-2011.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2012-2014.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Despair/Underlying Cause of Death, exclude Despair, 2015-2017.txt"
)

cdc.files.assault.excluded <- c(
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2000-2002.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2003-2005.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2006-2008.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2009-2011.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2012-2014.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Assault/Underlying Cause of Death, exclude Assault, 2015-2017.txt"
)

cdc.files.cancer.excluded <- c(
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2000-2002.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2003-2005.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2006-2008.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2009-2011.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2012-2014.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cancer/Underlying Cause of Death, exclude Cancer, 2015-2017.txt"
)

cdc.files.cardiovascular.excluded <- c(
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2000-2002.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2003-2005.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2006-2008.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2009-2011.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2012-2014.txt", 
  "../data/CDC/CDC_excluded_data/excluded_Cardiovascular/Underlying Cause of Death, exclude Cardiovascular, 2015-2017.txt"
)

####################### Original Source, Separate Causes of Death Data File ###################
cdc.data.despair <- cdc.reader.batch(cdc.files.despair, cdc.periods, "Despair")
cdc.data.assault <- cdc.reader.batch(cdc.files.assault, cdc.periods, "Assault")
cdc.data.cancer <- cdc.reader.batch(cdc.files.cancer, cdc.periods, "Cancer")
cdc.data.cardiovascular <- cdc.reader.batch(cdc.files.cardiovascular, cdc.periods, "Cardiovascular")
cdc.data.allcause <- cdc.reader.batch(cdc.files.allcause, cdc.periods, "All Cause")

cdc.data.despair.state <- cdc.reader.state.batch(cdc.files.despair.state, cdc.periods, "Despair")
cdc.data.assault.state <- cdc.reader.state.batch(cdc.files.assault.state, cdc.periods, "Assault")
cdc.data.cancer.state <- cdc.reader.state.batch(cdc.files.cancer.state, cdc.periods, "Cancer")
cdc.data.cardiovascular.state <- cdc.reader.state.batch(cdc.files.cardiovascular.state, cdc.periods, "Cardiovascular")
cdc.data.allcause.state <- cdc.reader.state.batch(cdc.files.allcause.state, cdc.periods, "All Cause")

cdc.data.state <- dplyr::bind_rows(
    cdc.data.despair.state, 
    cdc.data.assault.state, 
    cdc.data.cancer.state,
    cdc.data.cardiovascular.state, 
    cdc.data.allcause.state
  ) %>% 
  as.data.frame()


############################ Create Mortality Data by Subtraction ############################
# All cause of death without *
cdc.data.despair.excluded <- cdc.reader.batch(cdc.files.despair.excluded, cdc.periods, "-Despair")
cdc.data.assault.excluded <- cdc.reader.batch(cdc.files.assault.excluded, cdc.periods, "-Assault")
cdc.data.cancer.excluded <- cdc.reader.batch(cdc.files.cancer.excluded, cdc.periods, "-Cancer")
cdc.data.cardiovascular.excluded <- cdc.reader.batch(cdc.files.cardiovascular.excluded, cdc.periods, "-Cardiovascular")

# Subtraction
cdc.data.despair.fill <- cdc.fill.by.excl(cdc.data.despair, cdc.data.allcause, cdc.data.despair.excluded)
cdc.data.assault.fill <- cdc.fill.by.excl(cdc.data.assault, cdc.data.allcause, cdc.data.assault.excluded)
cdc.data.cancer.fill <- cdc.fill.by.excl(cdc.data.cancer, cdc.data.allcause, cdc.data.cancer.excluded)
cdc.data.cardiovascular.fill <- cdc.fill.by.excl(cdc.data.cardiovascular, cdc.data.allcause, cdc.data.cardiovascular.excluded)

# Bind to a single data frame
cdc.data <- dplyr::bind_rows(
    cdc.data.despair.fill, 
    cdc.data.assault.fill, 
    cdc.data.cancer.fill, 
    cdc.data.cardiovascular.fill, 
    cdc.data.allcause
  ) %>% 
  as.data.frame()

# Save file
saveRDS(cdc.data, "../data/CDC/cdc.data.fill.Rds")

################################## Exam Heavily Missing Counties ####################################
# If the data is heavily suppressed, then the imputation is not reliable. So 
#   we have to record the heavily suppressed counties
#   Generated data frame: 
#     - Rows: 3174 counties, identified by county_fips
#     - Cols: 5 causes of deaths
#     - Each entry denotes the county-death_cause heavily suppressed or not (boolean values)

# Check the counties with more than 4 periods being missing
cdc.miss_gt4 <- cdc.data %>% 
  dplyr::mutate(is_missing = is.na(death_rate)) %>%
  dplyr::group_by(county_fips, death_cause) %>%
  dplyr::summarise(sup_gt4 = sum(is_missing)) %>% 
  dplyr::mutate(sup_gt4 = sup_gt4 >= 4)

# Check the counties with last period being missing
cdc.miss_last_period <- cdc.data %>% 
  dplyr::filter(period == "2015-2017") %>%
  dplyr::group_by(county_fips, death_cause) %>%
  dplyr::summarise(sup_last = is.na(death_rate))

# Union the two conditions
cdc.suppress <- cdc.miss_last_period %>% 
  dplyr::full_join(cdc.miss_gt4, by = c("county_fips", "death_cause")) %>% 
  dplyr::mutate(suppress = sup_gt4 || sup_last) %>%
  dplyr::select(county_fips, death_cause, suppress) %>% 
  tidyr::spread(key = death_cause, value = suppress)

# Save the data frame as `cdc.suppress.rds`
saveRDS(cdc.suppress, "../data/CDC/cdc.suppress.Rds")

################################# Imputation with Amelia ######################################
# Impute data with Amelia, data used is "filled data" instead of orginal download
cdc.data.despair.imputed <- cdc.impute(cdc.data, cdc.data.state, "ALL", "Despair")
cdc.data.assault.imputed <- cdc.impute(cdc.data, cdc.data.state, "ALL", "Assault")
cdc.data.cancer.imputed <- cdc.impute(cdc.data, cdc.data.state, "ALL", "Cancer")
cdc.data.cardiovascular.imputed <- cdc.impute(cdc.data, cdc.data.state, "ALL", "Cardiovascular")
cdc.data.allcause.imputed <- cdc.impute(cdc.data, cdc.data.state, "ALL", "All Cause")

# Bind to a single data frame
cdc.data.imputed <- dplyr::bind_rows(
    cdc.data.despair.imputed, 
    cdc.data.assault.imputed, 
    cdc.data.cancer.imputed,
    cdc.data.cardiovascular.imputed, 
    cdc.data.allcause.imputed
  ) %>% 
  as.data.frame()

# Save file
saveRDS(cdc.data.imputed, "../data/CDC/cdc.data.imputed.Rds")

############################# Remove Temporary Variables #####################################
# Clean-up's
rm(
  list = c(
    "cdc.periods", 
    "state.name.abbr",
    
    "cdc.files.despair", 
    "cdc.files.assault", 
    "cdc.files.cancer",
    "cdc.files.cardiovascular",
    "cdc.files.allcause",
    
    "cdc.files.despair.state", 
    "cdc.files.assault.state", 
    "cdc.files.cancer.state",
    "cdc.files.cardiovascular.state",
    "cdc.files.allcause.state",
    
    # Comment out the three lines below to keep individual data frames
    "cdc.data.despair",
    "cdc.data.assault",
    "cdc.data.cancer",
    "cdc.data.cardiovascular",
    "cdc.data.allcause", 
    
    "cdc.data.despair.state",
    "cdc.data.assault.state",
    "cdc.data.cancer.state",
    "cdc.data.cardiovascular.state",
    "cdc.data.allcause.state"
  )
)
