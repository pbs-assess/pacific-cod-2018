## To extract PCod data and get catch, survey indices, and mean weights:
## For indices:
## 5CD - use HSMAS, HSSS, and CPUE
## 5AB - use QCSSS and CPUE
## 3CD - use WCVISS and CPUE
##
## extract.data()
## load.data()
##
## ca5ab <- get.total.catch.yr(areas = "5[AB]+")
## ca5cd <- get.total.catch.yr(areas = "5[CD]+")
## ca3cd <- get.total.catch.yr(areas = "3[CD]+")
## qcsss <- get.survey.index(survey.series.id = 1)
## hsmas <- get.survey.index(survey.series.id = 2)
## hsss <- get.survey.index(survey.series.id = 3)
## wcviss <- get.survey.index(survey.series.id = 4)
## mw5ab <- get.mean.weight(areas = "5[AB]+")
## mw5cd <- get.mean.weight(areas = "5[CD]+")
## mw3cd <- get.mean.weight(areas = "3[CD]+")

library(gfplot)
library(ggplot2)
library(dplyr)
library(rstan)
library(lubridate)

#' Extract species data from database to RDS files
#'
#' @param species Species code or name
#' @param cache.dir Reletive name of the directory to hold the RDS files
#' @param end.year End year for the data extraction
#' @param unsorted.only Only include 'unsorted' samples. Removes 'keepers'
#'   and 'sorted'
#'
#' @return Nothing, creates files in the cache.dir directory
extract.data <- function(species = "pacific cod",
                         cache.dir = "cache",
                         end.year,
                         unsorted.only = FALSE){
  cache_pbs_data(species,
                 path = cache.dir,
                 unsorted_only = unsorted.only,
                 survey_sets = FALSE)

  ## Extract CPUE data
  database <- c("GFFOS", "GFCatch", "PacHarvest")
  cpue <- get_cpue_historic(species,
                            alt_year_start_date = "04-01",
                            areas = c("3[CD]+", "5[AB]+", "5[CDE]+"),
                            end_year = end.year)
  saveRDS(cpue,
          file = file.path(cache.dir, "pbs-cpue.rds"),
          compress = FALSE)
  message("All data extracted and saved in the folder `",
          cache.dir, "`.")

}

#' Load the data from the RDS files produced by extract.data()
#'
#' @param cache.dir Reletive name of the directory to hold the RDS files
#'
#' @return The data object as returned from gfplot package
load.data <- function(cache.dir = "cache"){
  readRDS(file.path(cache.dir, "pacific-cod.rds"))
}

#' Calculate the total catch by year and quarter of year
#'
#' @param dat  A tibble of the catch from gfplot package
#'
#' @return A tibble with year, quarter, and catch weight
total.catch.yr.qtr <- function(dat){
  mutate(dat,
         month = month(best_date),
         quarter = case_when(
           month %in% seq(1, 3) ~ 4,
           month %in% seq(4, 6) ~ 1,
           month %in% seq(7, 9) ~ 2,
           month %in% seq(10, 12) ~ 3
         )) %>%
    select(-month) %>%
    mutate(year = if_else(quarter == 4, year - 1, year)) %>%
    group_by(year, quarter) %>%
    summarize(catch_weight = sum(landed_kg) + sum(discarded_kg)) %>%
    ungroup()
}

#' Extract table of commercial specimens for areas requested
#'
#' @param dat A tibble of the commercial samples from gfplot package
#' @param areas A regexp like this: "5[CD]+"
#' @param a Growth parameter alpha
#' @param b Growth parameter beta
#'
#' @return A tibble of all commercial specimens for the areas requested,
#'   with new columns 'month', 'quarter', and 'calc.weight' added
#'   calc.weight is calculated from length according to the Appendix in the
#'   2018 PCod stock assessment.
comm.specimens  <- function(dat,
                            areas = NULL,
                            a,
                            b){
  if(!is.null(areas)){
    dat <- dat %>% inner_join(gfplot::pbs_areas, by = "major_stat_area_code") %>%
      filter(grepl(areas, major_stat_area_description))
  }

  dat <- mutate(dat,
                month = month(trip_start_date),
                quarter = case_when(
                  month %in% seq(1, 3) ~ 4,
                  month %in% seq(4, 6) ~ 1,
                  month %in% seq(7, 9) ~ 2,
                  month %in% seq(10, 12) ~ 3
                )) %>% select(-month) %>%
    mutate(calc.weight = a * length ^ b) %>%
    filter(year >= 1956)

  ## Filter the data as shown in the table in the appendix for the calculation
  ##  of mean weight data (Table C1 in 2013 assessment document)
  ## Removed filtration of KEEPERS and UNSORTED so that data match 2014 data.
  dat %>% filter(trip_sub_type_code %in% c(1, 4)) %>%
    filter(gear == 1) %>%
    ## filter(year <= 1996 & sampling_desc == "KEEPERS" |
    ##       year > 1996 & sampling_desc == "UNSORTED") %>%
    filter(sample_type_code %in% c(1, 2, 6, 7)) %>%
    filter(!sample_id %in% c(173726,
                             173740,
                             191471,
                             184243,
                             184159,
                             215903,
                             223726))

}

#' Calculate the mean weight by sample
#'
#' @param dat A tibble of the catch from gfplot package
#'
#' @return A tibble of mean weight by sample
calc.mean.weight <- function(dat){
  ## Eq C3 in 2013 PCod assessment
  dat <- dat %>% group_by(year, quarter, sample_id) %>%
    mutate(calc.sample.weight = sum(calc.weight, na.rm = TRUE),
           sample_weight = ifelse(!is.na(sample_weight),
                                  sample_weight,
                                  calc.sample.weight)) %>%
    summarize(numerator = sum(mean(calc.weight, na.rm = TRUE) * sample_weight[1]),
              denominator = sample_weight[1],
              catch_weight = catch_weight[1]) %>%
    group_by(year, quarter) %>%
    summarize(ws = sum(numerator) / sum(denominator),
              catch_weight = sum(catch_weight))
  ## Eq C4 in 2013 PCod assessment
  dat %>% group_by(year, quarter) %>%
    summarize(numerator = sum(ws * catch_weight),
              denominator = sum(catch_weight),
              wf = numerator / denominator) %>%
    ungroup()

}

#' Calculate the total catch by year
#'
#' @param dat A tibble of the catch from gfplot package
#' @param areas A regexp like this: "5[CD]+"
#'
#' @return A tibble with the year and sum of landed and discarded catch
#'   by year
get.total.catch.yr <- function(dat,
                               areas = NULL){
  if(!is.null(areas)){
    dat <- dat %>%
      inner_join(gfplot::pbs_areas, by = "major_stat_area_code") %>%
      filter(grepl(areas, major_stat_area_description))
  }

  dat <- dat %>%
    mutate(year = if_else(month(dat$best_date) <= 3, year - 1, year)) %>%
    group_by(year) %>%
    summarize(catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0)

  ## Bring in USA landings and add to the catch
  areas <- gsub("\\[|\\]|\\+", "", areas)
  fn <- paste0("usa-catch-", areas, ".csv")
  usa <- as_tibble(read.csv(fn))
  left_join(dat, usa, by = "year") %>%
    rowwise() %>%
    mutate(total_catch = sum(usa_catch, catch_weight, na.rm = TRUE)) %>%
    mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
    ungroup()
}

#' Extract survey biomass and CV for the requested survey
#'
#' @param dat A tibble of the survey index from gfplot package
#' @param survey.series.id 1 = QCSSS, 2 = HSMAS, 3 = HSSS, 4 = WCVISS
#'
#' @return A tibble of the survey biomass index and wt for the requested survey
get.survey.index <- function(dat,
                             survey.series.id){
  dat %>%
    filter(survey_series_id == survey.series.id) %>%
    mutate(wt = 1/re) %>%
    select(year, biomass, wt) %>%
    mutate(biomass = sprintf("%0.1f", biomass / 1000),
           wt = sprintf("%0.1f", wt))
}

#' Calculate the mean weight by year
#'
#' @param dat.catch A tibble of the catch from gfplot package
#' @param dat.comm.samples A tibble of the commercial samples from gfplot package
#' @param areas A regexp like this: "5[CD]+"
#' @param a Growth parameter alpha
#' @param b Growth parameter beta
#'
#' @return A tibble of year and mean weight
get.mean.weight <- function(dat.comm.samples,
                            dat.catch,
                            areas = NULL,
                            a,
                            b){
  if(is.null(areas)){
    warning("You didn't specify any areas. ",
            "The whole dataset is being processed.")
  }
  cs <- comm.specimens(dat.comm.samples, areas, a, b)
  cs$catch_weight <- NULL
  tot.catch <- total.catch.yr.qtr(dat.catch)
  left_join(cs, tot.catch, by = c("year", "quarter")) %>%
    calc.mean.weight() %>%
    group_by(year) %>%
    summarize(mean_weight = mean(wf)) %>%
    mutate(mean_weight = as.numeric(sprintf("%0.3f", mean_weight)))
}
