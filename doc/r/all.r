## all.r
## This is the master file - it loads all packages and sources all
##  other R source code files.
## To debug in an R session, run these commands first:
## source("all.r");load.models.into.parent.env();source("custom-knitr-variables.r")

## Clean your workspace before loading
rm(list = ls(all = TRUE))

## The purpose of the r-functions directory is to separate the
##  r code which is not commonly changed so that the files which are
##  can be clearly seen.
func.dir <- "r-functions"

## Need to source utilities.r before everything because it contains the functions
##  install.packages.if.needed and remove.all.except
source(file.path(func.dir, "utilities.r"))

install.packages.if.needed("coda")
install.packages.if.needed("knitr")
install.packages.if.needed("lubridate")
# install.packages.if.needed("PBSmodelling")  # Loading this package was preventing Jaclyn from building the pdf on her mac
install.packages.if.needed("xtable")
install.packages.if.needed("RColorBrewer")

require(coda)
require(knitr)
require(lubridate)
# require(PBSmodelling)
require(xtable)
require(tidyverse)
require(RColorBrewer)

## Code to load the catch/TAC data, making catch figures, and making tables
##  for catch/TAC.
source(file.path(func.dir, "catches.r"))
## Code to load the models from the model directories
source(file.path(func.dir, "load-models.r"))
## Code to load the survey data, making survey figures, and making tables
##  for survey.
##source(file.path(func.dir, "survey.r"))
## Code to load data tables from the data directory
source(file.path(func.dir, "load-data.r"))
## Code to access mcmc parameters
source(file.path(func.dir, "mcmc-diagnostics.r"))
## Code to read a user file into an R list (for model setup)
## source(file.path(func.dir, "read-list.r"))

source(file.path(func.dir, "tables-activities-management.r"))
source(file.path(func.dir, "tables-parameters.r"))

source(file.path(func.dir, "tables-likelihood.r"))
source(file.path(func.dir, "tables-decisions.r"))
source(file.path(func.dir, "tables-maturity.r"))
source(file.path(func.dir, "figures-nage2.r"))
source(file.path(func.dir, "figures-q.r"))
source(file.path(func.dir, "figures-m.r"))
source(file.path(func.dir, "figures-age.r"))
## source(file.path(func.dir, "figures-catch.r"))
source(file.path(func.dir, "figures-mcmc-diagnostics.r"))
## source(file.path(func.dir, "figures-hcr.r"))
source(file.path(func.dir, "figures-indices.r"))
source(file.path(func.dir, "figures-selectivity.r"))
## source(file.path(func.dir, "figures-reference-points.r"))
source(file.path(func.dir, "figures-biomass.r"))
source(file.path(func.dir, "figures-recruitment.r"))
source(file.path(func.dir, "figures-mortality.r"))

## Code to verify the model setup
source(file.path(func.dir, "verify.r"))
## Code to setup the model names, and start/end years for various things
##  in the models
source("model-setup.r")
## Code to setup forecast model runs
## source("forecast-catch-levels.r")
## Code to setup retro model runs.
## source("retrospective-setup.r")

## Set up variables for data tables from csv files
source("data-tables.r")

