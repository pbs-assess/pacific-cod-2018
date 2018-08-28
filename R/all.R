# all.r
# This is the master file - it loads all packages and sources all
#  other R source code files.
#
# To debug in an R session, run these 3 commands first:
# source(file.path(here::here(), "R/all.r"))
# load.models.into.parent.env()
# source(file.path(here::here(), "R/custom-knitr-variables.r"))

# rm(list = ls(all = TRUE))

# Need to do this first to provide install.packages.if.needed()
# if(!("iscamtext" %in% rownames(installed.packages()))){
#   devtools::install_github("cgrandin/iscamtex")
# }
# library(iscamtex)

# install.packages.if.needed("coda")
# install.packages.if.needed("knitr")
# install.packages.if.needed("lubridate")
# install.packages.if.needed("PBSmodelling")
# install.packages.if.needed("xtable")
# install.packages.if.needed("RColorBrewer")

library(lubridate)
library(dplyr)
library(ggplot2)
library(gfplot)
library(purrr)
library(coda)
library(knitr)
library(PBSmodelling)
library(xtable)
library(tidyverse)
library(RColorBrewer)

rootd <- here::here()
rootd.R <- file.path(rootd, "R")
rootd.data <- file.path(rootd, "data")
rootd.models <- file.path(rootd, "models")
rootd.sens <- file.path(rootd.models, "SensitivityFigures")

source(file.path(rootd.R, "utilities.R"))
source(file.path(rootd.R, "verify.R"))
source(file.path(rootd.R, "model-setup.R"))
source(file.path(rootd.R, "load-models.R"))
source(file.path(rootd.R, "mcmc-diagnostics.R"))
source(file.path(rootd.R, "figures-biomass.R"))
source(file.path(rootd.data, "get-data.R"))
##
## # Code to setup the model names, and start/end years for various things
## #  in the models
## source(file.path(rootd.R, "model-setup.R"))
##
## # Code to setup forecast model runs
## # source("forecast-catch-levels.r")
## # Code to setup retro model runs.
## # source("retrospective-setup.r")
##
## # Set up variables for data tables from csv files
## # source("data-tables.r")
##
##
## # Load the raw data
dat.file <- file.path(rootd.data,
                      "pcod-cache",
                      "pacific-cod.rds")
if(!file.exists(dat.file)){
  cache_pbs_data(species = "pacific cod",
                 path = file.path(rootd.data,
                                  "pcod-cache"))
}
dat <- readRDS(dat.file)
