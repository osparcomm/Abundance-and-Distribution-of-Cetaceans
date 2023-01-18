##--------------------------------------------------------------------------------------------------------
## SCRIPT : Data for QSR2023
##
## Authors : Matthieu Authier & Rémi Pigeault
## Last update : 2021-12-14
## R version 4.0.1 (2020-06-06) -- "See Things Now"
## Copyright (C) 2020 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

lapply(c("lubridate", "tidyverse", "sf", "ggthemes", "ggspatial", 'pelaDSM'), library, character.only = TRUE)

rm(list = ls())

### functions
mapQSR <- function(predsf, type = c("distribution", "uncertainty"),
                   filename
                   ) {
  predsf <- predsf %>%
    filter(!is.na(distribution),
           !is.na(uncertainty)
           ) %>%
    mutate(year = factor(year, levels = c("2005-2009", "2010-2020"))) %>%
    st_transform(crs = 3035)
  
  theme_set(theme_bw(base_size = 16))
  if(type == "distribution") {
    m <- ggplot() +
      geom_sf(data = predsf,
              aes(fill = distribution), color = NA
              ) +
      geom_sf(data = world %>% 
                st_transform(crs = 3035),
              fill = "#2ca25f", alpha = 0.5
              ) +
      facet_wrap( ~ year, ncol = 2) +
      xlab("Longitude") + ylab("Latitude") +
      coord_sf(xlim = c(1835000, 5010000), ylim = c(1559000, 5687000)) +
      scale_fill_viridis_d(name = "density (ind/km²)") +
      annotation_scale(location = "br", width_hint = 0.25) +
      annotation_north_arrow(location = "tl", 
                             which_north = "true",
                             pad_x = unit(0.2, "cm"), 
                             pad_y = unit(0.1, "cm"),
                             style = north_arrow_fancy_orienteering
                             ) +
      theme(legend.position = "top",
            legend.key.width = unit(1, "cm")
            )
  } else {
    m <- ggplot() +
      geom_sf(data = predsf %>%
                mutate(uncertainty = factor(uncertainty, 
                                            levels = c("(0,20]", "(20,50]",
                                                       "(50,100]", "(100,150]",
                                                       "(150,Inf]")
                                            )
                       ),
              aes(fill = uncertainty), color = NA
              ) +
      geom_sf(data = world %>% 
                st_transform(crs = 3035),
              fill = "#2ca25f", alpha = 0.5
              ) +
      facet_wrap( ~ year, ncol = 2) +
      xlab("Longitude") + ylab("Latitude") +
      coord_sf(xlim = c(1835000, 5010000), ylim = c(1559000, 5687000)) +
      scale_fill_manual(name = "CV (%)", values = c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404")) +
      annotation_scale(location = "br", width_hint = 0.25) +
      annotation_north_arrow(location = "tl", 
                             which_north = "true",
                             pad_x = unit(0.2, "cm"), 
                             pad_y = unit(0.1, "cm"),
                             style = north_arrow_fancy_orienteering
                             ) +
      theme(legend.position = "bottom",
            legend.key.width = unit(1, "cm")
            )
  }
  ggsave(m,
         filename = paste("output/", filename, ".jpeg", sep = ""), 
         dpi = 600, units = 'cm',
         width = 25, height = 20
         )
}

### World
world <- read_sf("C:/Users/mauthier/Documents/DATA/WORLD/world_wgs84.gpkg") %>%
  st_transform(crs = 3035) 

### modelled distributions
ttru <- st_read(dsn = "data/QSR2023_M4_Distribution_wgs84.gpkg",
                layer = "Offshore Bottlenose Dolphins"
                )
ppho <- st_read(dsn = "data/QSR2023_M4_Distribution_wgs84.gpkg", 
                layer = "Harbour porpoises"
                )
bacu <- st_read(dsn = "data/QSR2023_M4_Distribution_wgs84.gpkg", 
                layer = "Minke whales"
                )
bphy <- st_read(dsn = "data/QSR2023_M4_Distribution_wgs84.gpkg", 
                layer = "Fin whales"
                )

### maps for QSR
ttru %>%
  mapQSR(type = "distribution",
         filename = "Figure_o_cetaceans_distribution_ttru_mean"
         )
ttru %>%
  mapQSR(type = "uncertainty",
         filename = "Figure_o_cetaceans_distribution_ttru_cv"
         )

ppho %>%
  mapQSR(type = "distribution",
         filename = "Figure_k_cetaceans_distribution_ppho_mean"
         )
ppho %>%
  mapQSR(type = "uncertainty",
         filename = "Figure_k_cetaceans_distribution_ppho_cv"
         )

bacu %>%
  mapQSR(type = "distribution",
         filename = "Figure_v_cetaceans_distribution_bacu_mean"
         )
bacu %>%
  mapQSR(type = "uncertainty",
         filename = "Figure_v_cetaceans_distribution_bacu_cv"
         )

bphy %>%
  mapQSR(type = "distribution",
         filename = "Figure_w_cetaceans_distribution_bphy_mean"
         )
bphy %>%
  mapQSR(type = "uncertainty",
         filename = "Figure_w_cetaceans_distribution_bphy_cv"
         )
