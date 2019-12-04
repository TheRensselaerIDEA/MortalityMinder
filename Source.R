setwd("init")
source("Curl_fixer.R")
source("Librarian.R")
library(withr)
# Separated out function definitions 10/01/2019
#source("Loader_CHR2019.R") # Added write_rds 9/27/2019
source("CHR2019_Lib.R")     # Function definitions from Loader_CHR2019.R

# Read in new Social Determinants definitions
#SocialDeterminants <- read_csv("../init/SocialDeterminants_updated.csv")
SocialDeterminants <- read_csv("../init/SocialDeterminants.csv")

final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
chr.data.2019 <- readRDS("../init/chr.data.2019.rds")
chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(final.determinants)

# Load name map and its inverse
chr.namemap.2019 <- SocialDeterminants %>% select("Code", "Name")
chr.namemap.2019 <- column_to_rownames(chr.namemap.2019, "Code")
names(chr.namemap.2019)[1] <- "name"

chr.namemap.inv.2019 <- SocialDeterminants %>% select("Name", "Code")
chr.namemap.inv.2019 <- column_to_rownames(chr.namemap.inv.2019, "Name")
names(chr.namemap.inv.2019)[1] <- "code"

# Load state and national mortality rate data
state_natl_death_rates <- readRDS("../data/CDC/state_natl_death_rates.Rds")
  
# Separated out function definitions 10/01/2019
#source("Loader_CDC.R")     # Added write_rds 9/27/2019
source("CDC_Lib.R")         # Function definitions from Loader_CDC.R
cdc.unimputed.data <- readRDS("../data/cdc.data.rds")
cdc.original.data <- readRDS("../data/CDC/cdc.data.fill.Rds")
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

# Set final working directory
setwd("../www")