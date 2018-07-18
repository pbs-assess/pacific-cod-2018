library(tidyverse)
library(readr)
library(psych)
library(reshape2)

source("get-data.R")

#' Save mean weight data to CSV files for each area
#'
#' @param dat A tibble of the commercial samples from gfplot package
#' @param out.dir The directory to output the CSV files to
#'
#' @return Nothing
save.mean.weight.data <- function(dat,
                                  out.dir = "results"){
  ## Length-weight parameters
  ## Coastwide
  .ALPHA <- 6.79e-06
  .BETA <- 3.11

  ## 3CD
  .ALPHA3 <- 7.43e-06
  .BETA3 <- 3.09

  ## 5ABCD
  .ALPHA5 <- 6.52e-06
  .BETA5 <- 3.12

  ## 2013 assessment
  .ALPHA2013 <- 7.377e-06
  .BETA2013 <- 3.0963

  areas <- c("3[CD]+", "5[AB]+", "5[CD]+")
  d$area <- NA
  for (i in seq_along(areas)) {
    dat[grepl(areas[[i]],
              dat$major_stat_area_name),
        "area"] <- gsub("\\[|\\]|\\+", "", areas[[i]])
  }
  specific_areas <- c("3C", "3D", "5A", "5B", "5C", "5D")
  dat$specific_area <- NA
  for (i in seq_along(specific_areas)) {
    dat[grepl(specific_areas[[i]],
              dat$major_stat_area_name),
        "specific_area"] <- specific_areas[[i]]
  }

  ## Get mean weights by area

  ## 5AB
  df <- filter(dat, area == "5AB") %>%
    get.mean.weight(d$catch,
                    areas = NULL,
                    a = .ALPHA5,
                    b = .BETA5)
  write_csv(df, file.path(out.dir, "AnnualMeanWeight_5AB.csv"))

  ## 5CD
  df <- filter(dat, area == "5CD") %>%
    get.mean.weight(d$catch,
                    areas = NULL,
                    a = .ALPHA5,
                    b = .BETA5)
  write_csv(df, file.path(out.dir, "AnnualMeanWeight_5CD.csv"))

  ## 5ABCD
  df <- filter(dat, is.element(area,c("5AB", "5CD"))) %>%
    get.mean.weight(d$catch,
                    areas = NULL,
                    a = .ALPHA5,
                    b = .BETA5)
  write_csv(df, file.path(out.dir, "AnnualMeanWeight_5ABCD.csv"))

  ## 3CD
  df <- filter(dat, area == "3CD") %>%
    get.mean.weight(d$catch,
                    areas = NULL,
                    a = .ALPHA3,
                    b = .BETA3)
  write_csv(df, file.path(out.dir, "AnnualMeanWeight_3CD.csv"))
}
