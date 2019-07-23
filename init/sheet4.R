# Data files
# Sheet 4:
chr.header.sheet4.gen <- function(chr.file) {
  
  # Clean up headers
  chr.rankedMeasure.headers <- read_excel(
    path = chr.file,
    col_names = F,
    n_max = 2,
    sheet = 4,
    trim_ws = T
  )
  
  # Fill NA's for header1 
  for (i in 4:length(chr.rankedMeasure.headers)) {
    if (is.na(chr.rankedMeasure.headers[1, i])) {
      chr.rankedMeasure.headers[1, i] = chr.rankedMeasure.headers[1, i-1]
    }
  }
  

  chr.rankedMeasure.headers <- data.table::transpose(chr.rankedMeasure.headers)
  colnames(chr.rankedMeasure.headers) <- c("h1", "h2")
  chr.rankedMeasure.headers %>% 
    
    # Symbols and cases
    dplyr::mutate(
      # Substitute symbol with char
      h2 = str_replace_all(h2, '[ -]', '_'),
      h2 = str_replace_all(h2, "[().]", ''),
      h2 = str_replace_all(
        h2, c(
          '%' = "pct",
          '#' = "num",
          '<' = "less",
          '/' = "_or_"
        )
      ),
      h2 = str_replace_all(h2, "___", '_'),
      h2 = str_replace_all(h2, "__", '_'),
      
      h1 = str_replace_all(h1, '\\^', ''),
      h1 = str_replace_all(h1, '\\*', ''),
      h1 = str_replace_all(h1, " - ", ' '),
      
      # Unify upper-lower case
      h1 = str_to_title(h1),
      h2 = str_to_lower(h2)
    ) %>% 
    
    # Geo ids
    dplyr::mutate(
      h2 = str_replace_all(
        h2, c(
          "county" = "county_name",
          "fips" = "county_fips",
          "state" = "state_name"
        )
      )
    ) %>% 
    
    # Get rid of unwanted cols, mark with "TO_REMOVE"
    dplyr::mutate(
      h2 = str_replace(h2, "95pct_ci.*", "TO_REMOVE"),
      h2 = str_replace(h2, "z_score", "TO_REMOVE"),
      h2 = str_replace(h2, "unreliable", "TO_REMOVE"),
      h2 = str_replace(h2, "sample_size", "TO_REMOVE"),
      
      h2 = str_replace_all(h2, "___", '_'),
      h2 = str_replace_all(h2, "__", '_')
    ) %>% 
    
    na.omit() %>% pull(h1) %>% unique()
}

chr.reader.sheet4.gen <- function(chr.file) {

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

t10 <- chr.header.sheet4.gen("../data/CHR/2010/New York.xls")
t11 <- chr.header.sheet4.gen("../data/CHR/2011/New York.xls")
t12 <- chr.header.sheet4.gen("../data/CHR/2012/New York.xls")
t13 <- chr.header.sheet4.gen("../data/CHR/2013/New York.xls")
t14 <- chr.header.sheet4.gen("../data/CHR/2014/New York.xls")
t15 <- chr.header.sheet4.gen("../data/CHR/2015/New York.xls")
t16 <- chr.header.sheet4.gen("../data/CHR/2016/New York.xls")
t17 <- chr.header.sheet4.gen("../data/CHR/2017/New York.xls")
t18 <- chr.header.sheet4.gen("../data/CHR/2018/New York.xls")
t19 <- chr.header.sheet4.gen("../data/CHR/2019/New York.xls")

t <- list(t15, t16, t17, t18, t19) %>% 
  purrr::reduce(dplyr::intersect)
