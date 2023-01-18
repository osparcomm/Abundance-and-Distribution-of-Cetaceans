##--------------------------------------------------------------------------------------------------------
## SCRIPT : Post-stratified CDS estimate for FW assessment units
##
## Authors : Matthieu Authier
## Last update : 2022-02-24
##
## R version 4.0.5 (2021-03-31) -- "Shake and Throw"
## Copyright (C) 2021 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

rm(list = ls())

source("code/20220218_FctCDS_postratification.r")

fw <- st_read(paste("data", "QSR2023_AU_FinWhale_WGS84.gpkg", sep = "/")) %>%
  st_transform(crs = 3035) %>%
  st_buffer(dist = units::set_units(0, "m")) %>%
  mutate(ID = 1:n())

### SCANS-III data
# copy data from report 
scansIII_df <- tribble(~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                       "8", 159669, 820, 0.0051, 0.493,
                       "9", 144352, 10600, 0.0734, 0.290,
                       "11", 68759, 2052, 0.0298, 0.220,
                       "12", 111115, 10245, 0.0922, 0.212,
                       "13", 59340, 3575, 0.0602, 0.215,
                       "S1", 62052, 0, 0, 0,
                       "S2", 59931, 48, 0.001, 1.00,
                       "S3", 100586, 47, 0.0005, 1.00
                       ) %>%
  mutate(Surf = ifelse(is.na(Surf), Abund/Dens, Surf))

# computation of total abundance and CV
scansIII_df %>%
  mutate(Var_abund = Abund*CV*Abund*CV) %>% 
  summarise(Abund_tot = sum(Abund), 
            Surf_tot = sum(Surf), 
            Dens_tot = Abund_tot/Surf_tot,
            Var_tot = sum(Var_abund), 
            CV_tot = sqrt(Var_tot)/Abund_tot
            )


EIF <- get_cds("1", survey = scansIII, df = scansIII_df, AU = fw)
EIF$estimate

NW <- get_cds("2", survey = scansIII, df = scansIII_df, AU = fw)
NW$estimate

Sp <- get_cds("3", survey = scansIII, df = scansIII_df, AU = fw)
Sp$estimate

# 
theme_set(theme_bw())
ggplot() +
  geom_sf(data = scansI,
          aes(fill = Stratum)
          ) +
  geom_sf(data = fw %>%
            filter(ID %in% c("1", "2", "3")),
          aes(), color = "black", fill = NA
          ) +
  geom_sf(data = NEA,
          aes(), fill = "lightblue", alpha = 0.3
          ) +
  scale_fill_viridis_d(name = "Block") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         pad_x = unit(0.2, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering
                         ) +
  ggtitle("SCANS-I") +
  theme(legend.position = "right")

ggplot() +
  geom_sf(data = scansII,
          aes(fill = Stratum)
          ) +
  geom_sf(data = fw %>%
            filter(ID %in% c("1", "2", "3")),
          aes(), color = "black", fill = NA
          ) +
  geom_sf(data = NEA,
          aes(), fill = "lightblue", alpha = 0.3
          ) +
  scale_fill_viridis_d(name = "Block") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         pad_x = unit(0.2, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering
                         ) +
  ggtitle("SCANS-II/CODA") +
  theme(legend.position = "right")

ggplot() +
  geom_sf(data = scansIII,
          aes(fill = Stratum)
          ) +
  geom_sf(data = fw %>%
            filter(ID %in% c("1", "2", "3")),
          aes(), color = "black", fill = NA
          ) +
  geom_sf(data = NEA,
          aes(), fill = "lightblue", alpha = 0.3
          ) +
  scale_fill_viridis_d(name = "Block") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         pad_x = unit(0.2, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering
                         ) +
  ggtitle("SCANS-III/ObSERVE") +
  theme(legend.position = "right")
