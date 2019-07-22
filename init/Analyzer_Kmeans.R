# Viz for k-means
km.wssplot <- function(data, cluster.num = 25, seed = 20){
  
  # Initialize table
  cluster.num <- min(nrow(data) - 1, cluster.num)
  wss <- data.frame(cluster = 1:cluster.num, quality = 0)
  
  # Do k-means clustering, reduce data points to `cluster.num` clusters
  for (i in 1:cluster.num){
    set.seed(seed)
    wss[i, "quality"] <- sum(kmeans(data, centers = i)$withinss)
  }
  
  # Plot the qualities of k-means by cluster
  plot <- ggplot(data = wss, aes(x = cluster, y = quality)) +
    geom_point() +
    geom_line() +
    labs(x = "Cluster", y = "Quality") +
    scale_x_continuous(breaks = 1:cluster.num) + 
    theme_classic()
  return(plot)
}

# Kmeans core function
km.func <- function(data.mat, cluster.num = 25, seed = 200) {
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    as.data.frame(
      data_frame(
        county_fips = dplyr::pull(data.mat, county_fips),
        km_cluster = as.factor(km.result$cluster)
      )
    )
  )
}
