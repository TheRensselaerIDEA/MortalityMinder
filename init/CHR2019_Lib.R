#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Sheets 2,3
#'
#' @param chr.file 
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Sheet 4 (header)
#'
#' @param chr.file 
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Sheet 4 (reader)
#'
#' @param chr.file 
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Sheet 5 (header)
#'
#' @param chr.file 
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export
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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Sheet 5 (reader)
#'
#' @param chr.file 
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Combining data frames
#'
#' @param chr.file 
#' @param keep.na
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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

#' Pulled from Loader_CHR2019.R; potentially used elsewhere
#' 
#' @note Data files: Combining data frames (batch)
#'
#' @param chr.file 
#' @param keep.na
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

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
