plot.cor <- function(sd1.select, sd2.select, chr.data) {
  
  # sd1.select <- "graduation_rate"
  # sd2.select <- "labor_force"
  
  sd1 <- pull(chr.data.2019, sd1.select)
  sd2 <- pull(chr.data.2019, sd2.select)
  cor.sd1.sd2 <- abs(cor(sd1, sd2, use = 'complete.obs'))
  
  ggplot() + 
    geom_rect(
      color = "black", 
      fill = "blue",
      alpha = 0.5,
      aes(xmin = -0.25, xmax = 0.25, ymin = - cor.sd1.sd2, ymax = 1 - cor.sd1.sd2)
    ) + 
    geom_rect(
      color = "black", 
      fill = "red",
      alpha = 0.5,
      aes(xmin = -0.25, xmax = 0.25, ymin = cor.sd1.sd2 - 1, ymax = cor.sd1.sd2)
    ) + 
    xlim(-1, 1) + 
    coord_flip() + 
    theme_minimal()
}

plot.cor("graduation_rate", "life_quality", chr.data.2019)
