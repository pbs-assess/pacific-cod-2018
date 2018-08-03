## Extract PCod data and get catch, survey indices, and mean weights:
## For indices:
## 5CD - use HSMAS, HSSS, and CPUE
## 5AB - use QCSSS and CPUE
## 3CD - use WCVISS and CPUE

library(gfplot)
library(ggplot2)
library(dplyr)
library(rstan)
library(lubridate)

#' Load the data from the RDS files produced by extract.data()
#'
#' @param cache.dir Reletive name of the directory to hold the RDS files
#'
#' @return The data object as returned from gfplot package
load.data <- function(cache.dir = "cache"){
  readRDS(file.path(cache.dir, "pacific-cod.rds"))
}

#' Extract table of commercial specimens for areas requested
#'
#' @param dat A tibble of the commercial samples from gfplot package
#' @param a Growth parameter alpha
#' @param b Growth parameter beta
#'
#' @return A tibble of all commercial specimens,
#'   with new columns 'month', 'quarter', and 'calc.weight' added
#'   calc.weight is calculated from length according to the Appendix in the
#'   2018 PCod stock assessment.
comm.specimens  <- function(dat,
                            a,
                            b){

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

#' Calculate the total catch by year and quarter
#'
#' @param dat A tibble of the catch from gfplot package
#' @param area A vector of regexps like this: c("3[CD]+", "5[CD]+")
#'   Used only for USA data.
#' @param include.usa Toggle to include the USA catch in the table and total_catch
#'  column sums
#'
#' @return A tibble with the year and sum of landed and discarded catch
#'   by year and quarter.
total.catch.yr.qtr <- function(dat,
                               areas = NULL,
                               include.usa = FALSE){
  if(is.null(areas)){
    stop("You must supply at least one area.")
  }
  ## Bring in USA landings from csv file
  areas <- gsub("\\[|\\]|\\+", "", areas)
  area.num <- substr(areas[1],
                     grep("[0-9]", areas),
                     grep("[0-9]", areas))

  usa <- NULL
  if(length(grep("AB", areas))){
    fn <- paste0("usa-catch-", area.num, "AB.csv")
    usa <- as_tibble(read.csv(fn))
  }
  if(length(grep("CD", areas))){
    fn <- paste0("usa-catch-", area.num, "CD.csv")
    usa.cd <- as_tibble(read.csv(fn))
    if(!is.null(usa)){
      ## Merge the two area catches (sum) into a single tibble
      usa <- left_join(usa, usa.cd, by = "year") %>%
        mutate(usa_catch.x, usa_catch.x = if_else(is.na(usa_catch.x),
                                                  0L,
                                                  usa_catch.x)) %>%
        mutate(usa_catch.y, usa_catch.y = if_else(is.na(usa_catch.y),
                                                  0L,
                                                  usa_catch.y)) %>%
        mutate(usa_catch = usa_catch.x + usa_catch.y) %>%
        select(-c("usa_catch.x", "usa_catch.y"))
    }else{
      usa <- usa.cd
    }
  }
  if(!include.usa){
    ## Set all USA catch to 0
    usa <- mutate(usa, usa_catch = 0L)
  }

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
    summarize(catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0) %>%
    left_join(usa, by = "year") %>%
    rowwise() %>%
    mutate(usa_catch, usa_catch1 = if_else(quarter == 1,
                                           usa_catch,
                                           0L)) %>%
    mutate(usa_catch1, usa_catch2 = if_else(is.na(usa_catch1),
                                            0L,
                                            usa_catch1)) %>%
    mutate(total_catch = sum(usa_catch2, catch_weight)) %>%
    mutate(total_catch = round(total_catch, 3)) %>%
    ## mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
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
                            areas =  c("3[CD]+", "5[AB]+", "5[CD]+"),
                            include.usa = FALSE,
                            a,
                            b){
  dat.comm.samples <- mutate(dat.comm.samples,
                             area = assign_areas(major_stat_area_name,
                                                 area_regex = areas)) %>%
    filter(!is.na(area))

  dat.catch <- mutate(dat.catch,
                      area = assign_areas(major_stat_area_name,
                                          area_regex = areas)) %>%
    filter(!is.na(area))

  cs <- comm.specimens(dat.comm.samples, a, b)
  cs$catch_weight <- NULL
  tot.catch <- total.catch.yr.qtr(dat.catch,
                                  areas = areas,
                                  include.usa = include.usa)

  left_join(cs, tot.catch, by = c("year", "quarter")) %>%
    calc.mean.weight() %>%
    group_by(year) %>%
    summarize(mean_weight = mean(wf)) %>%
    filter(!is.na(mean_weight)) %>%
    mutate(mean_weight = as.numeric(sprintf("%0.3f", mean_weight)))
}
