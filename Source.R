setwd("init")
source("Curl_fixer.R")
source("Librarian.R")
#source("Loader_CHR2019.R") # Added write_rds 9/27/2019
read_rds("chr.data.2019.rds")
read_rds("chr.namemap.2019.rds")
#source("Loader_CDC.R")     # Added write_rds 9/27/2019
read_rds("cdc.data.rds")
#source("Loader_GEO.R")     # Added write_rds 9/27/2019
read_rds("geo.namemap.rds")
source("Analyzer_PCA.R")
source("Clustering_Lib.R")
source("Analyzer_Correlation.R")
source("Theme.R")
setwd("../www")
