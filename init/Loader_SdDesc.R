# This loader is used for loading the descriptions of social determinants
# The output file is:
#   SocialDeterminants.rds
# Data-structure:
#   Four columns:
#     - Code: column names
#     - Name: the displayed names one website
#     - Definitions: the explainations of the social determinants
#     - URL: link to the dataset
#
# Author: 
#   Yuxuan Wang
# Date:
#   06-11-2019

sd <- read_csv(
    file = "SocialDeterminants.csv",
    col_types = cols(
      "Code" = col_character(),
      "Name" = col_character(),
      "Definitions" = col_character(),
      "URL" = col_character()
    )
  ) %>% 
  # saveRDS("SocialDeterminants.rds") %>% 
  as.data.frame()
