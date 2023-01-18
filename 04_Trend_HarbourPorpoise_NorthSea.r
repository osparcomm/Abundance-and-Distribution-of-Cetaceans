##--------------------------------------------------------------------------------------------------------
## SCRIPT : North Sea trend analysis on density estimates
##
## Authors : Matthieu Authier & Anita Gilles
## Last update : 2022-02-24
##
## R version 4.0.5 (2021-03-31) -- "Shake and Throw"
## Copyright (C) 2021 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

lapply(c("arm", "tidyverse"),
       library, character.only = TRUE
       )

rm(list = ls())

### these are the density estimates for the North Sea AU taken from script
## AbundanceEstimates_HarbourPorpoise.r
hpnsdata <- data.frame(year = c(1994, 2005, 2016),
                       density = c(0.479, 0.556, 0.485),
                       cv = c(0.135, 0.219, 0.098)
                       ) %>%
  mutate(p_hat = density / density[1],
         p_hat = ifelse(p_hat <= 0, NA, p_hat),
         x = (year - min(year)) / (max(year) - min(year))
         )

### trend estimation
mod <- arm::bayesglm(log(p_hat) ~ -1 + x,
                     family = gaussian,
                     data = hpnsdata,
                     prior.mean = 0,
                     prior.scale = -log(2) / tan(pi * (0.025 - 0.5)),
                     prior.df = 1,
                     epsilon = 1e-7, maxit = 500
                     )

### statistical significance
out <- data.frame(theta = as.numeric(coef(mod)),
                  theta_se = as.numeric(sqrt(vcov(mod)))
                  ) %>%
  mutate(wald = theta / theta_se,
         pval = 2 * min(1 - pnorm(wald), pnorm(wald))
         )

### trend (on log scale) is theta
out

### trend on natural scale is
(exp(out$theta) - 1) # over 23 years
exp(out$theta)^(1/(2016 - 1994 + 1)) - 1 # yearly decline

### plot
ggplot() +
  geom_point(data = hpnsdata,
             aes(x = year, y = p_hat)
             ) +
  geom_line(data = hpnsdata %>% 
               mutate(p_fitted = 1 + x * (exp(out$theta) - 1)),
             aes(x = year, y = p_fitted), color = "midnightblue"
             ) +
  scale_y_sqrt(name = "% of population (baseline = 1994)",
               breaks = c(0.5, 1, 2)
               ) +
  coord_cartesian(ylim = c(0.1, 3.0)) +
  theme_bw()
