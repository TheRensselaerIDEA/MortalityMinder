
social_determ <- read.csv("SocialDeterminants.csv", stringsAsFactors = FALSE)
social_determ$Keep <- as.factor(social_determ$Keep)

saveRDS(social_determ, file = "SocialDeterminants.Rds") 
