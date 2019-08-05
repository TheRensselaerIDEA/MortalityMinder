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
      # Substitute symbol with char
      # V1 = ifelse(is.na(V1), tmp_header_name, V1),
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
      # )
      
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
  
  for (row in 2:nrow(chr.additionMeasure.headers)) {
    if (is.na(chr.additionMeasure.headers[row, 1])) {
      cat(file=stderr(), "Hello\n")
      if (is.na(chr.additionMeasure.headers[row - 1, 1])) {
        chr.additionMeasure.headers[row - 1, 1] <- "General"
        chr.additionMeasure.headers[row, 1] <- chr.additionMeasure.headers[row - 1, 1]
      }
      chr.additionMeasure.headers[row, 1] <- chr.additionMeasure.headers[row - 1, 1]
    }
  }
  
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

tmp1 <- chr.header.sheet5.2019("../data/CHR/2019/New York.xls")
tmp2 <- chr.reader.sheet5.2019("../data/CHR/2019/New York.xls")
