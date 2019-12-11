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

cdc.data.despair <- cdc.reader.batch(cdc.files.despair, cdc.periods, "Despair")
cdc.data.assault <- cdc.reader.batch(cdc.files.assault, cdc.periods, "Assault")
cdc.data.cancer <- cdc.reader.batch(cdc.files.cancer, cdc.periods, "Cancer")
cdc.data.cardiovascular <- cdc.reader.batch(cdc.files.cardiovascular, cdc.periods, "Cardiovascular")
cdc.data.allcause <- cdc.reader.batch(cdc.files.allcause, cdc.periods, "All Cause")

cdc.data.ori <- dplyr::bind_rows(
  cdc.data.despair, 
  cdc.data.assault, 
  cdc.data.cancer,
  cdc.data.cardiovascular, 
  cdc.data.allcause
) %>% as.data.frame()

saveRDS(cdc.data.ori, file = "../data/CDC/cdc.original.Rds")