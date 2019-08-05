geo.data <- geo.map.fetch.dev("NY", geo.namemap)

mort.mat <- cdc.mort.mat(cdc.data, "NY", "Despair")
mort.label <- km.func(mort.mat, 4)
mort.data <- na.omit(dplyr::left_join(mort.label, mort.mat, by = "county_fips"))
cluster.type <- "km_cluster"

# Choropleth graph %>%
ggplot(
    left_join(geo.data, mort.data, by = "county_fips"),
    aes_(
      x = as.name("long"), 
      y = as.name("lat"), 
      # fill = as.name(cluster.type),
      group = as.name("geo_group")
    )
  ) +
  geom_polygon(size = 0.25, color = "black", alpha = 0.9) + 
  coord_map() + 
  scale_fill_manual(
    name = "Cluster", 
    guide = guide_legend(
      keyheight = unit(3, units = "mm"), 
      keywidth = unit(12, units = "mm"), 
      label.position = "bottom", 
      title.position = "top", 
      nrow = 1
    ),
    values = colorRampPalette(brewer.pal(9, "Blues")[-c(1:3)])(4)
  )