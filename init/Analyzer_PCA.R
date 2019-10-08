# PCA
pca.func <- function(data.mat, row.name, seed = 20) {
  set.seed(seed)
  pca.result <- prcomp(data.mat, retx = TRUE, center = FALSE)
  pca.result[["row.name"]] <- row.name
  return(pca.result)
}
