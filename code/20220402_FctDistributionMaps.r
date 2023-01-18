# remotes::install_gitlab(repo = "pelaverse/pelaDSM", host = "gitlab.univ-lr.fr")

lapply(c("sf", "readxl", "ggthemes", "ggspatial", "tidyverse", "lubridate",
         "pelaDSM"
         ),
       library, character.only = TRUE
       )

data(NEA)

figure_a <- function(sp, n_col = 1, n_row = 1) {
  theme_set(theme_bw(base_size = 16))
  obs2016 %>% 
    filter(n_ind != 0,
           species %in% sp
           ) %>%
    ggplot() +
    geom_sf(data = NEA %>% 
              st_transform(crs = 3035),
            fill = "#2ca25f", alpha = 0.5
            ) +
    geom_sf(data = survey2016,
            aes(), color = "black", alpha = 0.025
            ) +
    geom_sf(aes(), color = "midnightblue", alpha = 0.3) +
    xlab("Longitude") + ylab("Latitude") +
    facet_wrap(~ species, ncol = n_col, nrow = n_row) +
    coord_sf(xlim = c(1835000, 5010000), ylim = c(1559000, 5687000)) +
    # scale_color_viridis_d() +
    annotation_scale(location = "br", width_hint = 0.25) +
    annotation_north_arrow(location = "tl", 
                           which_north = "true",
                           pad_x = unit(0.2, "cm"), 
                           pad_y = unit(0.1, "cm"),
                           style = north_arrow_fancy_orienteering
                           ) +
    theme(legend.position = "top",
          legend.key.width = unit(3, "cm")
          )
}

mapQSR <- function(predsf, type = c("distribution", "uncertainty")) {
  predsf <- predsf %>%
    filter(!is.na(distribution),
           !is.na(uncertainty)
           ) %>%
    mutate(year = factor(year, levels = c("2005-2009", "2010-2020"))) %>%
    st_transform(crs = 3035)
  
  theme_set(theme_bw(base_size = 16))
  if(type == "distribution") {
    ggplot() +
      geom_sf(data = predsf,
              aes(fill = distribution), color = NA
              ) +
      geom_sf(data = NEA %>% 
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
    ggplot() +
      geom_sf(data = predsf %>%
                mutate(uncertainty = factor(uncertainty, 
                                            levels = c("(0,20]", "(20,50]",
                                                       "(50,100]", "(100,150]",
                                                       "(150,Inf]")
                                            )
                       ),
              aes(fill = uncertainty), color = NA
              ) +
      geom_sf(data = NEA %>% 
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
}
