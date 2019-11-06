# Author: 
#   Yuxuan Wang
#
# Date:
#   06-11-2019
#
# Desc:
#   This loader is used for loading the descriptions of social determinants
#
# Output:
#   File:
#     SocialDeterminants.rds
#
#   Data-structure:
#     8 columns:
#       - Code: column names
#       - Name: the displayed names one website
#       - Definitions: the explainations of the social determinants
#       - URL: link to the social determinants definition
#       - Source: name of the source website (like CDC)
#       - Source_url: link to source website
#       - Reason: reason for ranking
#       - Keep: need the social determinant or not.

read_csv(
    file = "SocialDeterminants.csv",
    col_types = cols(
      "Code" = col_character(),
      "Name" = col_character(),
      "Definitions" = col_character(),
      "URL" = col_character(),
      "Source" = col_character(),
      "Source_url" = col_character(),
      "Reason" = col_character(),
      "Keep" = col_logical()
    )
  ) %>% 
  saveRDS("SocialDeterminants.rds")
