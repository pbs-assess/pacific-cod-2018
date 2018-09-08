# all.r
# This is the master file - it loads all packages and sources all
#  other R source code files.
#
# To debug in an R session, run these 3 commands first:
# source(file.path(here::here(), "R/all.r"));load.models.into.parent.env();source(file.path(here::here(), "R/custom-knitr-variables.r"))

library(lubridate)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gfplot)
library(purrr)
library(scales)
library(cowplot)
library(coda)
library(knitr)
library(PBSmodelling)
library(xtable)
library(tidyverse)
library(RColorBrewer)
library(kableExtra)

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
source(file.path(rootd.R, "figures-recruitment.R"))
source(file.path(rootd.R, "figures-catch.R"))
source(file.path(rootd.R, "figures-indices.R"))
source(file.path(rootd.R, "figures-fishing-mortality.R"))
source(file.path(rootd.R, "figures-mcmc-diagnostics.R"))
source(file.path(rootd.R, "tables-catch.R"))
source(file.path(rootd.R, "get-alk.R"))
source(file.path(rootd.R, "get-age-sample.R"))
source(file.path(rootd.data, "get-data.R"))


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

## ggplot globals for project
ggplot2::theme_set(gfplot::theme_pbs())
scale_colour_continuous <- scale_colour_viridis_c
scale_fill_continuous <- scale_fill_viridis_c
scale_colour_discrete <- function(...) scale_colour_brewer(..., palette = "Set1")
scale_fill_discrete <- function(...) scale_fill_brewer(... , palette = "Set1")
