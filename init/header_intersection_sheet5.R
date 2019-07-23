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
      # # # Death
      # V2 = if_else(
      #   str_detect(V2, "# Deaths"),
      #   paste("death_num", V1, sep = '_'),
      #   V2
      # ),
      # 
      # # # Uninsured
      # V2 = if_else(
      #   str_detect(V2, "# Uninsured"),
      #   paste("num", V1, sep = '_'),
      #   V2
      # ),
      # 
      # # Segregation Index
      # V2 = if_else(
      #   str_detect(V2, "Segregation"),
      #   paste("seg_idx", 
      #         str_extract(V1, "(?<=- ).*"), 
      #         sep = '_'),
      #   V2
      # ),
      
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

tmp <- chr.header.sheet5.2019("data/CHR_different_year/2010 County Health Ranking New York Data - v2.xls")