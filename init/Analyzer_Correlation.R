# Kendall correlation core function
kendall.func <- function(x.data, sd.data) {
  
  # TODO's:
  #   1. Align social determinants and "x" data by "county_fips"
  #   2. Re-separate "x column and sd data frame with select
  #   3. Do cor.test
  
  align <- dplyr::left_join(x.data, sd.data, by = "county_fips") %>% 
    dplyr::select(-dplyr::one_of(c("county_fips", "state_name", "county_name")))
  
  x <- as.numeric(dplyr::pull(align, cluster))
  sd <- dplyr::select(align, -cluster)
  
  cor.res <- list()
  for (n in names(sd)) {
    y <- dplyr::pull(sd, n)
    if (is_character(y)) {
      # print(y)
      y <- readr::parse_number(y)
    }
    
    if (sum(is.na(y)) < 0.5 * length(y)) {
      # print(tibble(x, y, n))
      cor.res[[n]] <- cor.test(
        x = x, y = y, use = "pairwise.complete.obs", method = "kendall", exact = F
      )
    }
  }
  
  tibble(
    chr_code = names(cor.res),
    kendall_cor = sapply(cor.res, function(r) r$estimate),
    kendall_p = sapply(cor.res, function(r) r$p.value)
  )
}
