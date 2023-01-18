load(file = "dsm/bacu_calibrationdata.RData")

## same as above taking into account the coastline and lands
bacu_mod <- gam(bacu ~ 1 + offset(I(log(Effort))) +
                s(lon, lat, bs = 'so', xt = list(bnd = bnd), by = msfd) +
                s(Bathy, bs = "cs", k = 4) +
                s(Slope, bs = "cs", k = 4) +
                s(Aspect, bs = "cs", k = 4) +
                s(mSST, bs = "cs", k = 4) +
                s(mSST, season, k = 4, bs = "fs") +
                s(gradSST, bs = "cs", k = 4) +
                s(gradSST, season, k = 4, bs = "fs") +
                s(EKE, bs = "cs", k = 4) +
                s(EKE, season, k = 4, bs = "fs") +
                s(NPPV, bs = "cs", k = 4) +
                s(NPPV, season, k = 4, bs = "fs"),
              family = nb(),
              method = "REML",
              data = obs,
              drop.unused.levels = FALSE,
              knots = knots
              ) 

save(list = c("bacu_mod"), file = "dsm/bacu_dsm_output.RData")
