##--------------------------------------------------------------------------------------------------------
## SCRIPT : Post-stratified CDS estimate for HP assessment units
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

hp <- st_read(paste("data", "QSR2023_AU_HarbourPorpoise_WGS84.gpkg", sep = "/")) %>%
  st_transform(crs = 3035)

### SCANS-I data
scans_df <- tribble(~ Block, "A", "B", "J", "K", "L", "M", "C", "D", "E", "F",
                    "G", "H", "I", "Y", "X"
                    ) %>%
  left_join(readxl::read_excel(path = paste("data", "tables", "SCANS 1994 revised estimates 2017-03-22.xlsx", sep = "/"),
                               sheet = 1
                               ) %>%
              filter(Species == "HP",
                     !is.na(Block)
                     ) %>%
              select(Block,
                     `N individuals`,
                     `CV...9`
                     ) %>%
              rename(Abund = `N individuals`,
                     CV = `CV...9`
                     ) %>%
              mutate(Abund = ceiling(as.numeric(Abund)),
                     CV = as.numeric(CV)
                     ),
            by = "Block"
            ) %>%
  mutate(Abund = ifelse(is.na(Abund), 0, Abund),
         CV = ifelse(is.na(CV), 0, round(CV, 2))
         ) %>%
  left_join(scansI %>%
              st_drop_geometry() %>%
              select(Stratum, Area_sqkm) %>%
              rename(Block = Stratum, 
                     Surf = Area_sqkm
                     ),
            by = "Block"
            ) %>%
  mutate(Dens = Abund / Surf)

### SCANS-II/CODA data
scansII_df <- tribble(~ Block, "M", "B", "J", "L", "N", "Y", "H", "O", "R", "Z",
                      "P",  "Q", "T", "U", "V", "W", "S", "01", "02", "03", "04"
                      ) %>%
  left_join(readxl::read_excel(path = paste("data", "tables", "SCANS-II HP estimates revised.xlsx", sep = "/"),
                               sheet = 1
                               ) %>%
              filter(Species == "HP",
                     !is.na(Block)
                     ) %>%
              select(Block, `N indivs`, `CV(Nind)`) %>%
              rename(Abund = `N indivs`,
                     CV = `CV(Nind)`
                     ),
            by = "Block"
            ) %>%
  mutate(Abund = ifelse(is.na(Abund), 0, Abund),
         CV = ifelse(is.na(CV), 0, round(CV, 2))
         ) %>%
  left_join(scansII %>%
              st_drop_geometry() %>%
              select(Stratum, Area_sqkm) %>%
              rename(Block = Stratum, 
                     Surf = Area_sqkm
              ),
            by = "Block"
            ) %>%
  mutate(Dens = Abund / Surf)

### SCANS-III data
# copy data from report 
scansIII_df <- tribble(~ Block, ~ Surf, ~ Abund, ~Dens, ~CV, 
                       "AA", 12015, 0, 0, 0,
                       "AB", 26668, 2715, 0.102, 0.308,
                       "AC", 35180, 183, 0.005, 1.020,
                       "B", 118471, 3374, 0.028, 0.586,
                       "C", 81297, 17323, 0.213, 0.303,
                       "D", 48590, 5734, 0.118, 0.489,
                       "E", 34890, 8320, 0.239, 0.281, 
                       "F", 12322, 1056, 0.086, 0.383,
                       "G", 15122, 5087, 0.336, 0.428, 
                       "H", 18634, 1682, 0.09, 0.741, 
                       "I", 13979, 5556, 0.397, 0.347, 
                       "J", 35099, 2045, 0.058, 0.716, 
                       "K", 32505, 9999, 0.308, 0.273,
                       "L", 31404, 19064, 0.607, 0.383,
                       "M", 56469, 15655, 0.277, 0.342, 
                       "N", 69386, 58066, 0.837, 0.257,
                       "O", 60198, 53485, 0.888, 0.209,
                       "P", 63655, 52406, 0.823, 0.315, 
                       "P1", 23557, 25367, 1.077, 0.302, 
                       "Q", 49746, 16569, 0.333, 0.347, 
                       "R", 64464, 38646, 0.599, 0.287, 
                       "S", 40383, 6147, 0.152, 0.279, 
                       "T", 65417, 26309, 0.402, 0.295, 
                       "U", 60046, 19269, 0.321, 0.298,
                       "V", 38306, 5240, 0.137, 0.367, 
                       "W", 49778, 8978, 0.180, 0.568,
                       "X", 19496, 6713, 0.344, 0.305, 
                       "Y", 18779, 4006, 0.213, 0.400,
                       "Z", 11228, 4556, 0.406, 0.275,
                       "SVG", 714, 423, 0.593, 0.386, 
                       "TRD", 966, 273, 0.282, 0.476,
                       "S1", 62052, 3218.7, 0.053, 0.558,
                       "S2", 59931, 0, 0, 0,
                       "S3", 100586, 3320.2, 0.032, 0.525,
                       "S4", 63125, 14195.8, 0.227, 0.372,
                       "S5", 11010, 11624.5, 1.046, 0.282,
                       "S6", 15752, 3300.0, 0.212, 0.357,
                       "S7", 17752, 623.8, 0.037, 0.789,
                       "S8", 9723, 1977.4, 0.208, 0.629
                       ) %>%
  mutate(Surf = ifelse(is.na(Surf), Abund/Dens, Surf))

# computation of total abundance and CV for checking 
scansIII_df %>%
  mutate(Var_abund = Abund*CV*Abund*CV) %>% 
  summarise(Abund_tot = sum(Abund), 
            Surf_tot = sum(Surf), 
            Dens_tot = Abund_tot/Surf_tot,
            Var_tot = sum(Var_abund), 
            CV_tot = sqrt(Var_tot)/Abund_tot
            )

# "West Scotland & Ireland", mPBR = 78
WSI <- get_cds("10", survey = scansI, df = scans_df, AU = hp)
WSI$estimate
WSI <- get_cds("10", survey = scansII, df = scansII_df, AU = hp)
WSI$estimate
WSI <- get_cds("10", survey = scansIII, df = scansIII_df, AU = hp)
WSI$estimate

conf_logN(N = 44261, cv = 0.139)

scans_df %>%
  filter(Block %in% c("A", "B2")) %>%
  mutate(Var_abund = Abund*CV*Abund*CV) %>% 
  summarise(Abund_tot = sum(Abund), 
            Surf_tot = sum(Surf), 
            Dens_tot = Abund_tot/Surf_tot,
            Var_tot = sum(Var_abund), 
            CV_tot = sqrt(Var_tot)/Abund_tot
            )

# "Irish Seas and Celtic Sea"
ICS <- get_cds("11 + 12", survey = scansI, df = scans_df, AU = hp)
ICS$estimate
ICS <- get_cds("11 + 12", survey = scansII, df = scansII_df, AU = hp)
ICS$estimate
ICS <- get_cds("11 + 12", survey = scansIII, df = scansIII_df, AU = hp)
ICS$estimate

RLA::PBR(N = 46797, cv = 0.139, Fr = 0.1)
conf_logN(N = 46797, cv = 0.139)

# "Greater North Sea"
GNS <- get_cds("13", survey = scansI, df = scans_df, AU = hp)
GNS$estimate
GNS <- get_cds("13", survey = scansII, df = scansII_df, AU = hp)
GNS$estimate
GNS <- get_cds("13", survey = scansIII, df = scansIII_df, AU = hp)
GNS$estimate

# 
theme_set(theme_bw())
ggplot() +
  geom_sf(data = scansI,
          aes(fill = Stratum)
          ) +
  geom_sf(data = hp %>%
            filter(ID %in% c("10", "11 + 12", "13", "17")),
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
  geom_sf(data = hp %>%
            filter(ID %in% c("10", "11 + 12", "13", "17")),
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
  geom_sf(data = hp %>%
            filter(ID %in% c("10", "11 + 12", "13", "17")),
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
