data.trend <- read.delim2("~/data/AHRQ_Challenge/MortalityMinder/beta.02/data/CDC/all_cause_of_death.txt")

data.trend$Year <- data.trend$Year-1999

data.trend <- data.trend %>% 
  tidyr::drop_na(Deaths) %>%
  group_by(X2013.Urbanization.Code,Year)%>%
              summarise(death=mean(as.numeric(Deaths)),population=mean(as.numeric(Population)))

data.first <- data.trend[data.trend$X2013.Urbanization.Code == 1,]
data.second <- data.trend[data.trend$X2013.Urbanization.Code == 2,]
data.third <- data.trend[data.trend$X2013.Urbanization.Code == 3,]
data.forth <- data.trend[data.trend$X2013.Urbanization.Code == 4,]
data.fifth <- data.trend[data.trend$X2013.Urbanization.Code == 5,]
data.sixth <- data.trend[data.trend$X2013.Urbanization.Code == 6,]

ggplot(data.first, aes(Year, death, population))