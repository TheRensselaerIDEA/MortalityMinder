#' Used to visualize optimal k for k means clustering
#' 
#' @note most recent use is beta.01
#'
#' @param data 
#' @param cluster.num 
#' @param seed 
#'
#' @return a plot used to visualize optimal k for k means clustering
#'
#' @examples
#' In beta.01:
#' # Plot that help select different clustering algo
#' mort.mat <- cdc.generateMat.state(cdc.data, input$state_choice)
#' km.wssplot(select(mort.mat, -c(1, 2)))
#' 
#' @author 
#' @export
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
    labs(
      x = "Cluster", 
      y = "Quality"
    ) +
    scale_x_continuous(breaks = 1:cluster.num)
  return(plot)
}

#' Kmeans core function
#'
#' @param data.mat data.frame in which one column is "county_fips" and other columns
#'                  are data to cluster by
#' @param cluster.num Number of clusters to form 
#' @param seed random seed to use, is variable so that results can be replicated
#'
#' @return a tribble with two columns:
#'    county_fips: unique identifier for each county
#'    cluster: number corresponding to cluster county belongs to
#'
#' @examples
#' cdc.data %>%
#'   cdc.mort.mat(input$state_choice, input$death_cause) %>%
#'   km.func(4)
#'   
#' km.func(cdc.mort.mat(cdc.data, "NY", "Despair"), 5)
#' 
#' @author 
#' @export
km.func <- function(data.mat, cluster.num = 4, seed = 200) {
  
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    tibble(
      county_fips = dplyr::pull(data.mat, county_fips),
      cluster = as.character(km.result$cluster)
    )
  )
}
