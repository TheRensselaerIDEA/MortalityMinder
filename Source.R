#setwd("./init")
source("./init/Curl_fixer.R")
source("./init/Librarian.R")
#source("Librarian_deploy.R")
library(withr)
library(stringdist)
# Separated out function definitions 10/01/2019
#source("Loader_CHR2019.R") # Added write_rds 9/27/2019
source("./init/CHR2019_Lib.R")     # Function definitions from Loader_CHR2019.R

# Read in new Social Determinants definitions
#SocialDeterminants <- read_csv("../init/SocialDeterminants_updated.csv")
SocialDeterminants <- read_csv("./init/SocialDeterminants.csv")

final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
getwd()
chr.data.2019 <- readRDS("./init/chr.data.2019.rds")
chr.data.2019 <- chr.data.2019 %>%
  # as_data_frame %>%    # JSE 23-Feb-2021 (depreciated)
  as_tibble %>% 
  dplyr::select(all_of(final.determinants))

# Load name map and its inverse
chr.namemap.2019 <- SocialDeterminants %>% 
  dplyr::select("Code", "Name")
chr.namemap.2019 <- column_to_rownames(chr.namemap.2019, "Code")
names(chr.namemap.2019)[1] <- "name"

chr.namemap.inv.2019 <- SocialDeterminants %>% 
  dplyr::select("Name", "Code")
chr.namemap.inv.2019 <- column_to_rownames(chr.namemap.inv.2019, "Name")
names(chr.namemap.inv.2019)[1] <- "code"

# Load state and national mortality rate data
state_natl_death_rates <- readRDS("./data/CDC/state_natl_death_rates.Rds")
  
# Separated out function definitions 10/01/2019
#source("Loader_CDC.R")     # Added write_rds 9/27/2019
source("./init/CDC_Lib.R")         # Function definitions from Loader_CDC.R
cdc.original.data <- readRDS("./data/CDC/cdc.original.Rds")
cdc.unimputed.data <- readRDS("./data/cdc.data.rds")
cdc.data <- readRDS("./data/CDC/cdc.data.imputed.Rds")

cdc.suppress <- readRDS("./data/CDC/cdc.suppress.Rds")
# cdc.data <- readRDS("cdc.data.dev.rds")
# need impute non zero death_rate
# cdc.data$death_rate[cdc.data$death_rate < 1] <- 1
# Separated out function definitions 10/01/2019
#source("Loader_GEO.R")     # Added write_rds 9/27/2019
source("./init/GEO_Lib.R")         # Function definitions from Loader_GEO.R
geo.namemap <- readRDS("./init/geo.namemap.rds")
source("./init/Analyzer_PCA.R")
source("./init/Clustering_Lib.R")
source("./init/Analyzer_Correlation.R")
source("./init/Theme.R")

# Set final working directory
setwd("./www")
