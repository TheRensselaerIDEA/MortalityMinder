source("Source.R")
library(tigris)

state.list <- append(state.abb, "US", after = 0)

for (state in state.list) {
  if (state != "US") {
    dataset <- counties(cb = TRUE, resolution = "20m", state = state)
  } else {
    dataset <- counties(cb = TRUE, resolution = "20m")
  }
  saveRDS(dataset, paste("shape_files/",state, ".Rds", sep = ""))
  print(paste("Collected data for", state, sep = " "))
}
