# remotes::install_gitlab(repo = "pelaverse/RLA", host = "gitlab.univ-lr.fr")
# remotes::install_gitlab(repo = "pelaverse/pelaDSM", host = "gitlab.univ-lr.fr")

lapply(c("sf", "readxl", "ggthemes", "ggspatial", "tidyverse", 
         "RLA", "pelaDSM"
         ),
       library, character.only = TRUE
       )

data(NEA)

# shapefiles
scansI <- st_read(paste("data", "QSR2023_scans_design_WGS84.gpkg", sep = "/"),
                  layer = "SCANS1994"
                  ) %>%
  st_transform(crs = 3035)
# scansI %>% st_area() %>% units::set_units("km^2") %>% floor()

scansII <- st_read(paste("data", "QSR2023_scans_design_WGS84.gpkg", sep = "/"),
                   layer = "SCANS2005"
                   ) %>%
  st_transform(crs = 3035)
# scansII %>% st_area() %>% units::set_units("km^2") %>% floor()

scansIII <- st_read(paste("data", "QSR2023_scans_design_WGS84.gpkg", sep = "/"),
                    layer = "SCANS2016"
                    ) %>%
  st_transform(crs = 3035)
# scansIII %>% st_area() %>% units::set_units("km^2") %>% floor()

# function to compute the CDS estimate
get_cds <- function(x, survey, df, AU) {
  y <- st_intersection(AU %>% 
                         filter(ID == x),
                       survey
                       ) %>% 
    st_sf()
  
  x <- y %>%
    mutate(area = st_area(.) %>% 
             units::set_units("km^2")
           ) %>%
    st_drop_geometry() %>%
    left_join(df %>%
                select(Block, Dens, CV) %>%
                rename(Stratum = Block),
              by = "Stratum"
              ) %>%
    mutate(Dens = ifelse(is.na(Dens), 0, Dens),
           CV = ifelse(is.na(CV), 0, CV),
           Abund = area * Dens,
           Abund = units::drop_units(Abund),
           Var_abund = Abund * CV * Abund * CV
           ) 
  z <- x %>% 
    summarise(Abund_tot = sum(Abund), 
              Surf_tot = sum(area), 
              Dens_tot = Abund_tot/Surf_tot,
              Var_tot = sum(Var_abund), 
              CV_tot = sqrt(Var_tot)/Abund_tot
              )
  return(list(poly = y, tab = x, estimate = z))
}

conf_logN <- function(N, cv) {
  data.frame(N = N,
             cv = cv
             ) %>%
    mutate(sigma = sqrt(log1p(cv * cv)),
           mu = log(N) - 0.5 * sigma * sigma,
           lower = floor(qlnorm(0.025, mu, sigma)),
           upper = ceiling(qlnorm(0.975, mu, sigma))
           )
}
