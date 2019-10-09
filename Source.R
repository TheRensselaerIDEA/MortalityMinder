setwd("init")
source("Curl_fixer.R")
source("Librarian.R")
# Separated out function definitions 10/01/2019
#source("Loader_CHR2019.R") # Added write_rds 9/27/2019
source("CHR2019_Lib.R")     # Function definitions from Loader_CHR2019.R
chr.data.2019 <- readRDS("chr.data.2019.rds") %>%
  
  # 10/02/2019
  # Remove unwanted social determinants
  # This should be a temporary change because the source file will be replaced in the end
  dplyr::select(
    -dplyr::starts_with("health_"),
    -dplyr::contains("mortality"), 
    -dplyr::contains("deaths"), 
    -dplyr::contains("death"),
    -dplyr::contains("life"),
    -dplyr::contains("ypll"),
    -dplyr::starts_with("num_")
  )
  
  
  
chr.namemap.2019 <- readRDS("chr.namemap.2019.rds")
# Separated out function definitions 10/01/2019
#source("Loader_CDC.R")     # Added write_rds 9/27/2019
source("CDC_Lib.R")         # Function definitions from Loader_CDC.R
cdc.data <- readRDS("cdc.data.dev.rds")
# Separated out function definitions 10/01/2019
#source("Loader_GEO.R")     # Added write_rds 9/27/2019
source("GEO_Lib.R")         # Function definitions from Loader_GEO.R
geo.namemap <- readRDS("geo.namemap.rds")
source("Analyzer_PCA.R")
source("Clustering_Lib.R")
source("Analyzer_Correlation.R")
source("Theme.R")
# Read in new SD definitions
SocialDeterminants <- readRDS("SocialDeterminants.rds")
setwd("../www")