##--------------------------------------------------------------------------------------------------------
## SCRIPT : Example Map of sightings or distribution
##
## Authors : Matthieu Authier
## Last update : 2022-02-24
##
## R version 4.0.5 (2021-03-31) -- "Shake and Throw"
## Copyright (C) 2021 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

rm(list = ls())

source("code/20220402_FctDistributionMaps.r")

m4 <- st_read(dsn = "data/QSR2023_M4_FormattedData_wgs84.gpkg",
              layer = "data"
              ) %>%
  st_transform(crs = 3035)

### number of sighted animals
m4 %>%
  st_drop_geometry() %>% 
  summarize(deldel = sum(ddel),
            stecoe = sum(scoe),
            smadel = sum(ddsc),
            turtru = sum(ttru),
            glomel = sum(gmel),
            gragri = sum(ggri),
            balphy = sum(bphy),
            balacu = sum(bacu),
            phymac = sum(pmac),
            zipcav = sum(zcav),
            phopho = sum(ppho)
            ) %>%
  as.data.frame()

### results of the datacall
theme_set(theme_bw(base_size = 16))
m4 %>%
  mutate(Year = lubridate::year(lubridate::ymd(Date))) %>%
  ggplot() +
  geom_sf(data = NEA %>% 
            st_transform(crs = 3035),
          fill = "#2ca25f", alpha = 0.5
          ) +
  geom_sf(aes(color = Year)) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ Platform) +
  coord_sf(xlim = c(1835000, 5010000), ylim = c(1559000, 5687000)) +
  scale_color_viridis_c() +
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

### seasonal differences
m4 %>% 
  st_drop_geometry() %>% 
  mutate(year = lubridate::year(lubridate::ymd(Date)),
         month = lubridate::month(lubridate::ymd(Date)),
         season = ifelse(month %in% c(12, 1, 2), "winter", NA),
         season = ifelse(month %in% 3:5, "spring", season),
         season = ifelse(month %in% 6:8, "summer", season),
         season = ifelse(month %in% 9:11, "autumn", season)
         ) %>%
  group_by(season, month) %>% 
  summarize(n = n(),
            Effort_km = sum(Effort_km),
            Bathy = mean(Bathy),
            Slope = mean(Slope),
            Aspect = mean(Aspect),
            mSST = mean(mSST, na.rm = TRUE),
            gradSST = mean(gradSST, na.rm = TRUE),
            EKE = mean(EKE, na.rm = TRUE),
            NPPV = mean(NPPV, na.rm = TRUE)
            ) %>% 
  ggplot(aes(x = month, y = Effort_km, fill = season)) +
  geom_col() +
  xlab("Month") + ylab("Effort (km)") +
  scale_fill_colorblind() +
  scale_y_sqrt(breaks = c(10000, 50000, 100000, 150000)) +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()

### year differences
m4 %>% 
  st_drop_geometry() %>% 
  mutate(year = lubridate::year(lubridate::ymd(Date))) %>%
  group_by(year, Platform) %>% 
  summarize(n = n(),
            Effort_km = sum(Effort_km),
            Bathy = mean(Bathy),
            Slope = mean(Slope),
            Aspect = mean(Aspect),
            mSST = mean(mSST, na.rm = TRUE),
            gradSST = mean(gradSST, na.rm = TRUE),
            EKE = mean(EKE, na.rm = TRUE),
            NPPV = mean(NPPV, na.rm = TRUE)
            ) %>% 
ggplot(aes(x = year, y = Effort_km)) +
  geom_col() +
  xlab("Year") + ylab("Effort (km)") +
  facet_wrap(~ Platform) +
  scale_y_sqrt(breaks = c(10000, 30000, 50000, 100000)) +
  scale_x_continuous(breaks = 2005:2020) +
  theme_bw()

### maps with sightings of a few species in 2016
survey2016 <- m4 %>%
  mutate(Year = lubridate::year(lubridate::ymd(Date)),
         month = lubridate::month(lubridate::ymd(Date)),
         season = ifelse(month %in% c(12, 1, 2), "winter", NA),
         season = ifelse(month %in% 3:5, "spring", season),
         season = ifelse(month %in% 6:8, "summer", season),
         season = ifelse(month %in% 9:11, "autumn", season),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")),
         cetacean = ppho + lalb + lacu + desp + ttru + ggri + bacu + ddel + scoe + ddsc + gmel + glsp + zcav + mbid + hamp + zisp + sfro + gmac + kosp + pmac + bphy + bede + lw,
         cetacean = ifelse(cetacean > 0, 1, 0),
         beaked_whale = zcav + mbid + hamp + zisp
         ) %>%
  filter(Year == 2016) %>%
  mutate(X = st_centroid(.) %>% st_coordinates(.) %>% as.data.frame() %>% pull("X"),
         Y = st_centroid(.) %>% st_coordinates(.) %>% as.data.frame() %>% pull("Y")
         ) %>%
  st_as_sf()

obs2016 <- survey2016 %>%
  dplyr::select(X, Y, 
                cetacean,
                ppho, ddel, scoe, 
                lalb, lacu, ttru, 
                zcav, bacu, bphy,
                pmac, beaked_whale, gmel, ggri,
                geom
                ) %>%
  pivot_longer(cols = c("cetacean", "ppho", "ddel", "scoe", "lalb", "lacu", "ttru", "zcav", "bacu", "bphy", "pmac", "gmel", "ggri", "beaked_whale"),
               names_to = "species",
               values_to = "n_ind"
               ) %>%
  st_centroid()

figure_a(sp = "cetacean")
figure_a(sp = "ppho")
figure_a(sp = "ddel")
figure_a(sp = "ttru")
figure_a(sp = "lacu")
figure_a(sp = "lalb")
figure_a(sp = "scoe")

figure_a(sp = "zcav")
figure_a(sp = "pmac")
figure_a(sp = "gmel")
figure_a(sp = "ggri")
figure_a(sp = "beaked_whale")

figure_a(sp = "bacu")
figure_a(sp = "bphy")

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
ttru %>%
  mapQSR(type = "distribution")
ttru %>%
  mapQSR(type = "uncertainty")

ppho %>%
  mapQSR(type = "distribution")
ppho %>%
  mapQSR(type = "uncertainty")

bacu %>%
  mapQSR(type = "distribution")
bacu %>%
  mapQSR(type = "uncertainty")

bphy %>%
  mapQSR(type = "distribution")
bphy %>%
  mapQSR(type = "uncertainty")