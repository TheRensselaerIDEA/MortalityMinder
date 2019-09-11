# Data files
# Sheet 4:
chr.header <- function(chr.file, chr.sheet) {
  
  chr.headers <- readxl::read_excel(
    path = chr.file,
    col_names = F,
    n_max = 2,
    sheet = chr.sheet,
    trim_ws = T
  )
  
  # Fill NA's for header1 
  chr.headers[1, 1] <- "Id"
  chr.headers[1, ] <- zoo::na.locf(as.character(chr.headers[1, ]), fromLast = F)
  chr.headers[1, ] <- base::make.unique(as.character(chr.headers[1, ]), sep = '.')
  
  # Symbols and cases
  chr.headers <- data.table::transpose(chr.headers)
  colnames(chr.headers) <- c("h1", "h2")
  hd.l <- chr.headers %>% 
    
    dplyr::mutate(
      # Substitute symbol with char
      h2 = stringr::str_replace_all(h2, "[().]", ''),
      h2 = stringr::str_replace_all(
        h2, c(
          '%' = "Percentage of ",
          '#' = "Number of ",
          '<' = "Less than ",
          '/' = " or "
        )
      ),
      h2 = stringr::str_replace_all(h2, "  ", ' '),
      h2 = stringr::str_replace_all(h2, "   ", ' '),
      h2 = stringr::str_replace_all(h2, " - ", '-'),
      
      h1 = stringr::str_replace_all(h1, "[()]", ''),
      h1 = stringr::str_replace_all(h1, '\\^', ''),
      h1 = stringr::str_replace_all(h1, '\\*', ''),
      h1 = stringr::str_replace_all(h1, " - ", ' '),
      
      # Unify upper-lower case
      h1 = str_to_title(h1),
      h2 = str_to_title(h2),
      
      # Special cases
      h1 = stringr::str_replace_all(h1, "Years Of Potential Life Lost", ''),
      
      # Trim ws
      h1 = str_trim(h1),
      h2 = str_trim(h2)
    ) %>% 
    
    # Geo Ids
    dplyr::mutate(
      h2 = stringr::str_replace_all(
        h2, c(
          "County" = "county_name",
          "Fips" = "county_fips",
          "State" = "state_name"
        )
      )
    )
  
  # Transposed data header mapping, using h1 as column names
  hd.w <- dplyr::select(hd.l, "h2") %>% 
    data.table::transpose() %>% 
    magrittr::set_colnames(dplyr::pull(hd.l, "h1"))
  
  # Unique h1
  hd.u <- dplyr::pull(hd.l, "h1") %>% 
    stringr::str_replace_all("\\.\\d+", '') %>% 
    base::unique()
  # Return a list containing header_long and header_transpose and header1_unique
  list(
    "header_long" = hd.l,
    "header_wide" = hd.w,
    "header_unique" = hd.u
  )
}

chr.reader <- function(chr.file, chr.sheet) {
  
  # Retrieve headers
  hd <- chr.header(chr.file, chr.sheet)
  hd_l <- hd[["header_long"]]
  hd_w <- hd[["header_wide"]]
  hd_u <- hd[["header_unique"]]
  
  # Import data from sheet 4
  data <- readxl::read_excel(
    path = chr.file, 
      sheet = chr.sheet,
      skip = 3,
      trim_ws = T,
      col_names = dplyr::pull(hd_l, "h1")
    ) %>% 
    tidyr::drop_na(
      dplyr::starts_with("Id")
    )
  
  # Split into smaller df and rename cols with header2
  df.list <- list()
  for (h1 in hd_u) {
    h2 <- dplyr::select(hd_w, dplyr::starts_with(h1))
    df <- dplyr::select(data, dplyr::starts_with(h1)) %>% 
      magrittr::set_colnames(h2) %>% 
      dplyr::select(-dplyr::starts_with("95Percentage")) %>% 
      dplyr::select(-dplyr::starts_with("Rank"))
    
    if (h1 != "Id") {
      df <- dplyr::mutate(
        df, 
        "county_fips" = dplyr::pull(df.list[["Id"]], "county_fips")
      )
    }
    df.list[[h1]] <- as.data.frame(df)
  }
  
  # Ret
  return(df.list)
}

chr.unite.v <- function(df.list1, df.list2) {
  
  col.union <- dplyr::union(
    names(df.list1),
    names(df.list2)
  )
  
  new.list <- list()
  for (n in col.union) {
    df1 <- df.list1[[n]]
    df2 <- df.list2[[n]]
    new.list[[n]] <- dplyr::bind_rows(df1, df2)
  }
  
  return(new.list)
}


chr.unite.h <- function(df.list1, df.list2) {
  
  temp.list <- append(df.list1, df.list2)
  
  new.list <- list()
  for (n in unique(names(temp.list))) {
    new.list[[n]] <- temp.list[[n]]
  }
  
  return(new.list)
}

# 2019 dat
chr.data.2019 <- lapply(
    # Sheet selected
    2:5,
    
    # Importation function -> combine all states' data (vertical combination)
    function(sheet, fnames) {
      purrr::reduce(
        lapply(
          fnames,
          chr.reader,
          sheet
        ),
        chr.unite.v
      )
    },
    
    # File names
    chr.fnames.2019 <- sapply(
      state.name,  # this is builtin variable
      function(state) {
        paste("../data/CHR/2019/", state, ".xls", sep = '')
      }
    )
  ) %>%
  purrr::reduce(
    chr.unite.h
  )

# 2018 dat
chr.data.2018 <- lapply(
  # Sheet selected
  2:5,
  
  # Importation function -> combine all states' data (vertical combination)
  function(sheet, fnames) {
    purrr::reduce(
      lapply(
        fnames,
        chr.reader,
        sheet
      ),
      chr.unite.v
    )
  },
  
  # File names
  chr.fnames.2018 <- sapply(
    state.name,  # this is builtin variable
    function(state) {
      paste("../data/CHR/2018/", state, ".xls", sep = '')
    }
  )
) %>%
  purrr::reduce(
    chr.unite.h
  )

# 2017 dat
chr.data.2017 <- lapply(
  # Sheet selected
  2:5,
  
  # Importation function -> combine all states' data (vertical combination)
  function(sheet, fnames) {
    purrr::reduce(
      lapply(
        fnames,
        chr.reader,
        sheet
      ),
      chr.unite.v
    )
  },
  
  # File names
  chr.fnames.2017 <- sapply(
    state.name,  # this is builtin variable
    function(state) {
      paste("../data/CHR/2017/", state, ".xls", sep = '')
    }
  )
) %>%
  purrr::reduce(
    chr.unite.h
  )

# 2016 dat
chr.data.2016 <- lapply(
  # Sheet selected
  2:5,
  
  # Importation function -> combine all states' data (vertical combination)
  function(sheet, fnames) {
    purrr::reduce(
      lapply(
        fnames,
        chr.reader,
        sheet
      ),
      chr.unite.v
    )
  },
  
  # File names
  chr.fnames.2016 <- sapply(
    state.name,  # this is builtin variable
    function(state) {
      paste("../data/CHR/2016/", state, ".xls", sep = '')
    }
  )
) %>%
  purrr::reduce(
    chr.unite.h
  )

# 2015 dat
chr.data.2015 <- lapply(
  # Sheet selected
  2:5,
  
  # Importation function -> combine all states' data (vertical combination)
  function(sheet, fnames) {
    purrr::reduce(
      lapply(
        fnames,
        chr.reader,
        sheet
      ),
      chr.unite.v
    )
  },
  
  # File names
  chr.fnames.2015 <- sapply(
    state.name,  # this is builtin variable
    function(state) {
      paste("../data/CHR/2015/", state, ".xls", sep = '')
    }
  )
) %>%
  purrr::reduce(
    chr.unite.h
  )

# Loaders does NOT work for the below years yet.

# # 2014 dat
# chr.data.2014 <- lapply(
#   # Sheet selected
#   2:5,
#   
#   # Importation function -> combine all states' data (vertical combination)
#   function(sheet, fnames) {
#     purrr::reduce(
#       lapply(
#         fnames,
#         chr.reader,
#         sheet
#       ),
#       chr.unite.v
#     )
#     cat("Testing\n")
#   },
#   
#   # File names
#   chr.fnames.2014 <- sapply(
#     state.name,  # this is builtin variable
#     function(state) {
#       paste("../data/CHR/2014/", state, ".xls", sep = '')
#     }
#   )
# ) %>%
#   purrr::reduce(
#     chr.unite.h
#   )
# 
# # 2013 dat
# chr.data.2013 <- lapply(
#   # Sheet selected
#   2:5,
#   
#   # Importation function -> combine all states' data (vertical combination)
#   function(sheet, fnames) {
#     purrr::reduce(
#       lapply(
#         fnames,
#         chr.reader,
#         sheet
#       ),
#       chr.unite.v
#     )
#   },
#   
#   # File names
#   chr.fnames.2013 <- sapply(
#     state.name,  # this is builtin variable
#     function(state) {
#       paste("../data/CHR/2013/", state, ".xls", sep = '')
#     }
#   )
# ) %>%
#   purrr::reduce(
#     chr.unite.h
#   )
# 
# # 2012 dat
# chr.data.2012 <- lapply(
#   # Sheet selected
#   2:5,
#   
#   # Importation function -> combine all states' data (vertical combination)
#   function(sheet, fnames) {
#     purrr::reduce(
#       lapply(
#         fnames,
#         chr.reader,
#         sheet
#       ),
#       chr.unite.v
#     )
#   },
#   
#   # File names
#   chr.fnames.2012 <- sapply(
#     state.name,  # this is builtin variable
#     function(state) {
#       paste("../data/CHR/2012/", state, ".xls", sep = '')
#     }
#   )
# ) %>%
#   purrr::reduce(
#     chr.unite.h
#   )
# 
# # 2011 dat
# chr.data.2011 <- lapply(
#   # Sheet selected
#   2:5,
#   
#   # Importation function -> combine all states' data (vertical combination)
#   function(sheet, fnames) {
#     purrr::reduce(
#       lapply(
#         fnames,
#         chr.reader,
#         sheet
#       ),
#       chr.unite.v
#     )
#   },
#   
#   # File names
#   chr.fnames.2011 <- sapply(
#     state.name,  # this is builtin variable
#     function(state) {
#       paste("../data/CHR/2011/", state, ".xls", sep = '')
#     }
#   )
# ) %>%
#   purrr::reduce(
#     chr.unite.h
#   )
# 
# # 2010 dat
# chr.data.2010 <- lapply(
#   # Sheet selected
#   2:5,
#   
#   # Importation function -> combine all states' data (vertical combination)
#   function(sheet, fnames) {
#     purrr::reduce(
#       lapply(
#         fnames,
#         chr.reader,
#         sheet
#       ),
#       chr.unite.v
#     )
#   },
#   
#   # File names
#   chr.fnames.2010 <- sapply(
#     state.name,  # this is builtin variable
#     function(state) {
#       paste("../data/CHR/2010/", state, ".xls", sep = '')
#     }
#   )
# ) %>%
#   purrr::reduce(
#     chr.unite.h
#   )



# Index dictionary - index in this corresponds with the index in resultant selector data frames.

chr.indices <- function() {
  chr.indices.df <<- as.data.frame(chr.data.2019[["Id"]])
}

# Selector - takes year range and determinant for exploration.

chr.selector <- function (determ, year_begin, year_end) {
  chr.determ.output <<- data.frame()
  for (year in year_begin:year_end) {
    if (year == 2019) {
      tmp <- chr.data.2019[[determ]]
    } else if (year == 2018) {
      tmp <- as.data.frame(chr.data.2018[[determ]])
    } else if (year == 2017) {
      tmp <- as.data.frame(chr.data.2018[[determ]])
    } else if (year == 2016) {
      tmp <- as.data.frame(chr.data.2018[[determ]])
    } else if (year == 2015) {
      tmp <- as.data.frame(chr.data.2018[[determ]])
    }
    # else if (year == 2014) {
    #   tmp <- as.data.frame(chr.data.2018[[determ]])
    # } else if (year == 2013) {
    #   tmp <- as.data.frame(chr.data.2018[[determ]])
    # } else if (year == 2012) {
    #   tmp <- as.data.frame(chr.data.2018[[determ]])
    # } else if (year == 2011) {
    #   tmp <- as.data.frame(chr.data.2018[[determ]])
    # } else if (year == 2010) {
    #   tmp <- as.data.frame(chr.data.2018[[determ]])
    # }
    
    i <- 0
    for (colname in colnames(tmp)) {
      i <- i + 1
      colnames(tmp)[i] <- paste(colnames(tmp)[i], year)
      print(colnames(tmp)[i])
    }
    
    if (year == year_begin) {
      chr.determ.output <<- tmp
    } else {
      chr.determ.output <<- cbind(chr.determ.output, tmp)
      print("After merge:", stderr())
      print(chr.determ.output, stderr())
    }
    # print(chr.determ.output, stderr())
    chr.determ.output
  }
}