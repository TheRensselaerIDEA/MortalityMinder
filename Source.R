setwd("init")
source("Curl_fixer.R")
source("Librarian.R")
library(withr)
# Separated out function definitions 10/01/2019
#source("Loader_CHR2019.R") # Added write_rds 9/27/2019
source("CHR2019_Lib.R")     # Function definitions from Loader_CHR2019.R
  
#chr.data.2019 <- readRDS("chr.data.2019.reduced.rds")
chr.data.2019 <- readRDS("chr.data.2019.rds")
reduced.chr.list <- readRDS("reduced.chr.list")

#Filtering out the social determinants that we don't want; making chr.data.2019 only contain the few selected social determinants that are relevant

reduced.chr.list <- readRDS("reduced.chr.list")
reduced.chr.list <- append(reduced.chr.list, "county_fips", after = 0)

chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(reduced.chr.list)

# Load state and national mortality rate data
state_natl_death_rates <- readRDS("../data/CDC/state_natl_death_rates.Rds")
  
chr.namemap.2019 <- readRDS("chr.namemap.2019.rds")
chr.namemap.inv.2019 <- readRDS("chr.namemap.inv.2019.rds")
# Separated out function definitions 10/01/2019
#source("Loader_CDC.R")     # Added write_rds 9/27/2019
source("CDC_Lib.R")         # Function definitions from Loader_CDC.R
cdc.data <- readRDS("../data/CDC/cdc.data.imputed.Rds")
# cdc.data <- readRDS("cdc.data.dev.rds")
# need impute non zero death_rate
# cdc.data$death_rate[cdc.data$death_rate < 1] <- 1
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