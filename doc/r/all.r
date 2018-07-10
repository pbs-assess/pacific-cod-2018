## all.r
## This is the master file - it loads all packages and sources all
##  other R source code files.
## To debug in an R session, run these commands first:
## source("all.r");load.models.into.parent.env();source("custom-knitr-variables.r")

rm(list = ls(all = TRUE))

## Need to do this first to provide install.packages.if.needed()
if(!("iscamtext" %in% rownames(installed.packages()))){
  devtools::install_github("cgrandin/iscamtex")
}
library(iscamtex)

install.packages.if.needed("coda")
install.packages.if.needed("knitr")
install.packages.if.needed("lubridate")
## install.packages.if.needed("PBSmodelling")
install.packages.if.needed("xtable")
install.packages.if.needed("RColorBrewer")

library(coda)
library(knitr)
library(lubridate)
## library(PBSmodelling)
library(xtable)
library(tidyverse)
library(RColorBrewer)

## Code to setup the model names, and start/end years for various things
##  in the models
source("model-setup.r")
## Code to setup forecast model runs
## source("forecast-catch-levels.r")
## Code to setup retro model runs.
## source("retrospective-setup.r")

## Set up variables for data tables from csv files
source("data-tables.r")
