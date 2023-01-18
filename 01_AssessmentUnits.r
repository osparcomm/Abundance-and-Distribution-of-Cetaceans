##--------------------------------------------------------------------------------------------------------
## SCRIPT : Vizualise assessment units
##
## Authors : Matthieu Authier
## Last update : 2022-04-20
##
## R version 4.0.5 (2021-03-31) -- "Shake and Throw"
## Copyright (C) 2021 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

rm(list = ls())

# devtools::install_github("yutannihilation/ggsflabel")

source("code/20220402_FctDistributionMaps.r")

hp <- st_read(paste("data", "QSR2023_AU_HarbourPorpoise_WGS84.gpkg", sep = "/")) %>%
  st_transform(crs = 3035)

ggplot() +
  geom_sf(data = hp,
          aes(fill = Name)
          ) +
  geom_sf(data = NEA,
          fill = "black", alpha = 0.1
          ) +
  geom_sf_text(data = hp %>%
                 select(Name, geom) %>%
                 st_centroid() %>%
                 mutate(code = c("Not Assessed", "Not Assessed", "Not Assessed", "Not Assessed", 
                                 "Assessed", "Assessed", "Assessed", "Assessed", 
                                 "Not Assessed", "Not Assessed"
                                 ),
                        code = factor(code, levels = c("Assessed", "Not Assessed")),
                        Name = c("NA", "NA", "NA", "NA", "A", "A", "A", "A", "NA", "NA")
                        ),
               aes(label = Name, color = code)
               ) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(1835000, 5010000), ylim = c(1559000, 5687000)) +
  scale_fill_viridis_d(name = "Assessment Units") +
  scale_color_manual(name = "", values = c(grey(0.95), "black")) +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         pad_x = unit(0.2, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering
                         ) +
  theme_bw()
