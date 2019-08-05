# README:----------------------------------------------------------------------------------+
# DESC:
#   Importing and Preparing CDC_WONDER Death of Despaire Mortality dataset of NY state
#   - Source Website: CDC wonder not compressed

# NOTE: 
#   Year 2000-2017
#Please run Loader_CDC.R before running this file

# PACKAGES: 
#   `stringr`, `tidyr`, `dplyr`, `readr`, `janitor`
# -----------------------------------------------------------------------------------------+


fit_data <-cdc.data[order(cdc.data$county_fips,cdc.data$death_cause,cdc.data$period),]

for(i in seq(1, nrow(fips), 6)){
  x <- c(1,2,3,4,5,6)
  y <- fit_data$death_num[i:(i+5)]
  df <- as.data.frame(x)
  df <- cbind(df,y)
  num_na <- sum(is.na(y))
  if(num_na > 3){
    next
  }
  df2 <- df %>% filter(!is.na(y))
  fit <- lm(y ~ x, data = df2)
  df3 <- df %>% 
    mutate(pred = predict(fit, .)) %>%
    # Replace NA with pred in y
    mutate(y = ifelse(is.na(y), min(9,pred), y))
  fit_data$death_num[i:(i+5)] = df3$y
}
