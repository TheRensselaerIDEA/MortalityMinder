# Kendall correlation core function
kendall.func <- function(
  cdc.data.long, cdc.period, chr.data.long, state.abbr, death.cause
) {
  
  chr.data.long <- dplyr::select_if(chr.data.long, ~ !any(is.na(.)))
  
  state.name <- cdc.data.long %>% 
    dplyr::filter(state_abbr == state.abbr) %>% 
    dplyr::select(state_abbr, state_name) %>% 
    unique() %>% 
    dplyr::pull(state_name)
  
  mort <- cdc.data.long %>%
    dplyr::filter(state_abbr == state.abbr, death_cause == death.cause) %>% 
    dplyr::select(county_fips, period, death_rate) %>% 
    tidyr::spread(key = period, value = death_rate) %>% 
    dplyr::pull(cdc.period)
  
  chr.data.factors <- chr.data.long %>%
    dplyr::filter(state_name == state.name) %>%
    dplyr::select(-dplyr::one_of(c("state_name", "county_name", "county_fips")))
  
  chr.cdc.kendall.cor <- apply(
    chr.data.factors, 2, 
    function(x) {
      cor(
        x = as.numeric(x), 
        use = "pairwise.complete.obs", 
        method = "kendall", 
        y = mort
      )
    }
  )
  
  chr.cdc.kendall.p <- apply(
    chr.data.factors, 2,
    function(x) {
      cor.test(
        x = as.numeric(x),
        use = "pairwise.complete.obs",
        method = "kendall",
        y = mort
      )$p.value
    }
  )
  
  chr.cdc.kendall <- data_frame(
    chr_code = names(chr.cdc.kendall.cor),
    kendall_cor = chr.cdc.kendall.cor,
    kendall_p = chr.cdc.kendall.p
  )
  
  return(as.data.frame(chr.cdc.kendall))
}
