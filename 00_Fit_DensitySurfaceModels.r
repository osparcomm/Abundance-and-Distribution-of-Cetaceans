##--------------------------------------------------------------------------------------------------------
## SCRIPT : Fit dsm on collated data for M4
##
## Authors : Matthieu Authier
## Last update : 2022-02-24
##
## R version 4.0.5 (2021-03-31) -- "Shake and Throw"
## Copyright (C) 2021 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------
library(mgcv)

rm(list = ls())

for(j in c("ppho", "ttru", "bacu", "bphy")) {
  source(paste("code/", j, "_dsm.r", sep = ""))
  rm(list = ls()[-match(x = "j", table = ls())])
}; rm(j)