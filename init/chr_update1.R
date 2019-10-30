

chr_update <- readRDS("~/Documents/Research/Research_2019_fall/task6/chr_update.rds") # change the path
chr_update1 <- chr_update

# Drop NA value
drop = chr_update1[colSums(is.na(chr_update1)) > 1500]
d = colnames(drop)
chr_update1 = dplyr::select(chr_update,-d)

# change ratio to rate
ratio = dplyr::select_if(chr_update1, is.character)

opc_ratio = ratio[[2]]
mhp_ratio = ratio[[4]]
dentist_ratio = ratio[[5]]
pcp_ratio = ratio[[6]]

a = sapply(strsplit(opc_ratio, ":"), function(x) round(as.numeric(x[2])/as.numeric(x[1]),5)*10^5) #1200:1 changes to (1/1200)*10^5,only first 5 precise number saved
b = sapply(strsplit(mhp_ratio, ":"), function(x) round(as.numeric(x[2])/as.numeric(x[1]),5)*10^5)
c = sapply(strsplit(dentist_ratio, ":"), function(x) round(as.numeric(x[2])/as.numeric(x[1]),5)*10^5)
d = sapply(strsplit(pcp_ratio, ":"), function(x) round(as.numeric(x[2])/as.numeric(x[1]),5)*10^5)

chr_update1[colnames(ratio[2])] = a
chr_update1[colnames(ratio[4])] = b
chr_update1[colnames(ratio[5])] = c
chr_update1[colnames(ratio[6])] = d

# change yes no to 0,1
chr_update1 <- chr_update1 %>%
  dplyr::mutate(drinking_water_violations_presence_of_violation = ifelse(drinking_water_violations_presence_of_violation == "No",0,1))

saveRDS(chr_update1, file = "~/Documents/Research/Research_2019_fall/task6/chr_update1.rds") 











