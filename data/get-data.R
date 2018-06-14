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

## Growth parameters for length/weight relationship
.ALPHA <- 7.377e-06
.BETA <- 3.0963

library(gfplot)
library(ggplot2)
library(dplyr)
library(rstan)
library(lubridate)

extract.data <- function(species = "pacific cod",
                         cache.dir = "cache",
                         unsorted_only = FALSE){
  ## Extract the data into the cache directory

  cache_pbs_data(species,
                 path = cache.dir,
                 unsorted_only = unsorted_only,
                 survey_sets = FALSE)

  ## Extract CPUE data
  database <- c("GFFOS", "GFCatch", "PacHarvest")
  sql <- readLines("pcod-cpue.sql")
  pcod_cpue <- run_sql(database = database, query = sql)
  saveRDS(pcod_cpue, file = file.path(cache.dir, "pbs-cpue.rds"),
          compress = FALSE)
  message("All data extracted and saved in the folder `",
          cache.dir, "`.")

}

load.data <- function(cache.dir = "cache"){
  ## Assumes you have run extract.data()

  d_survey_sets     <<- readRDS(file.path(cache.dir, "pbs-survey-sets.rds"))
  d_survey_samples  <<- readRDS(file.path(cache.dir, "pbs-survey-samples.rds"))
  d_comm_samples    <<- readRDS(file.path(cache.dir, "pbs-comm-samples.rds"))
  d_catch           <<- readRDS(file.path(cache.dir, "pbs-catch.rds"))
  d_cpue_spatial    <<- readRDS(file.path(cache.dir, "pbs-cpue-spatial.rds"))
  d_cpue_spatial_ll <<- readRDS(file.path(cache.dir, "pbs-cpue-spatial-ll.rds"))
  d_survey_index    <<- readRDS(file.path(cache.dir, "pbs-survey-index.rds"))
  d_age_precision   <<- readRDS(file.path(cache.dir, "pbs-age-precision.rds"))
  d_cpue_index      <<- readRDS(file.path(cache.dir, "pbs-cpue-index.rds"))
  d_cpue            <<- as_tibble(readRDS(file.path(cache.dir, "pbs-cpue.rds")))
}

total.catch.yr.qtr <- function(d = d_catch){
  ## Return a tbl of the total catch by year and quarter
  mutate(d,
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

comm.specimens  <- function(d = d_comm_samples,
                            areas = NULL,
                            a = .ALPHA,
                            b = .BETA){
  ## Return a tbl of all commercial specimens for the areas requested,
  ##  with new columns 'month', 'quarter', and 'calc.weight' added
  ##  calc.weight is calculated from length according to the Appendix in the
  ##  2018 PCod stock assessment.
  ## areas can be a regexp like this: "5[CD]+"
  ## a and b are the growth parameters: weight = a*length^b

  if(!is.null(areas)){
    d <- d %>% inner_join(gfplot::pbs_areas, by = "major_stat_area_code") %>%
      filter(grepl(areas, major_stat_area_description))
  }

  d <- mutate(d,
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
  d %>% filter(trip_sub_type_code %in% c(1, 4)) %>%
    filter(gear == 1) %>%
    filter(year <= 1996 & sampling_desc == "KEEPERS" |
           year > 1996 & sampling_desc == "UNSORTED") %>%
    filter(sample_type_code %in% c(1, 2, 6, 7)) %>%
    filter(!sample_id %in% c(173726,
                             173740,
                             191471,
                             184243,
                             184159,
                             215903,
                             223726))

}

calc.mean.weight <- function(d){
  ## Return a tbl of the mean weight by sample
  ## d is the pre-filtered tbl from function comm.specimens.pcod()

  ## Eq C3 in 2013 PCod assessment
  d <- d %>% group_by(year, quarter, sample_id) %>%
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
  d %>% group_by(year, quarter) %>%
    summarize(numerator = sum(ws * catch_weight),
              denominator = sum(catch_weight),
              wf = numerator / denominator) %>%
    ungroup()

}

get.total.catch.yr <- function(d = d_catch,
                               areas = NULL){
  ## Return a tbl with the year and sum of landed and discarded catch
  ##  by year
  ## areas can be a regexp like this: "5[CD]+"

  if(!is.null(areas)){
    d <- d %>%
      inner_join(gfplot::pbs_areas, by = "major_stat_area_code") %>%
      filter(grepl(areas, major_stat_area_description))
  }

  d <- d %>%
    mutate(year = if_else(month(d$best_date) <= 3, year - 1, year)) %>%
    group_by(year) %>%
    summarize(catch_weight = (sum(landed_kg) + sum(discarded_kg)) / 1000.0)

  ## Bring in USA landings and add to the catch
  areas <- gsub("\\[|\\]|\\+", "", areas)
  fn <- paste0("usa-catch-", areas, ".csv")
  usa <- as_tibble(read.csv(fn))
  left_join(d, usa, by = "year") %>%
    rowwise() %>%
    mutate(total_catch = sum(usa_catch, catch_weight, na.rm = TRUE)) %>%
    mutate(total_catch = sprintf("%0.3f", round(total_catch, 3))) %>%
    ungroup()
}

get.survey.index <- function(d = d_survey_index,
                             survey.series.id){
  ## Return a tbl of the survey biomass index and wt for the requested survey
  ## eg. survey_series_id:
  ## 1 = QCSSS
  ## 2 = HSMAS
  ## 3 = HSSS
  ## 4 = WCVISS

  d %>%
    filter(survey_series_id == survey.series.id) %>%
    mutate(wt = 1/re) %>%
    select(year, biomass, wt) %>%
    mutate(biomass = sprintf("%0.1f", biomass / 1000),
           wt = sprintf("%0.1f", wt))
}

get.mean.weight <- function(areas = NULL){
  ## Return a tbl of year and mean weight
  if(is.null(areas)){
    warning("You didn't specify any areas. ",
            "The whole dataset is being processed.")
  }
  cs <- comm.specimens(d_comm_samples, areas)
  cs$catch_weight <- NULL
  tot.catch <- total.catch.yr.qtr(d_catch)
  left_join(cs, tot.catch, by = c("year", "quarter")) %>%
    calc.mean.weight() %>%
    group_by(year) %>%
    summarize(mean_weight = mean(wf)) %>%
    mutate(mean_weight = sprintf("%0.3f", mean_weight))
}
