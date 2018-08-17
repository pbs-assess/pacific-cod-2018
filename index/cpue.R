if (params$era == "modern") {
  fi <- here::here("data/cpue-modern.rds")
  if (!file.exists(fi)) {
    d1996 <- gfplot::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996)
    write_rds(d1996, fi)
  } else {
    d1996 <- read_rds(fi)
  }
} else {
  fi <- here::here("data/cpue-historic.rds")
  if (!file.exists(fi)) {
    d <- gfplot::get_cpue_historic(species = NULL, end_year = 1995,
      alt_year_start_date = "04-01")
    write_rds(d, fi)
  } else {
    d <- read_rds(fi)
  }
}

if (params$era == "modern") {
  define_fleet <- function(area, area_name) {
    out <- tidy_cpue_index(d1996,
      species_common = tolower(params$species_proper),
      gear = "bottom trawl",
      alt_year_start_date = "04-01",
      use_alt_year = params$april1_year,
      year_range = c(1996, 2017),
      lat_range = c(48, Inf),
      min_positive_tows = 100,
      min_positive_trips = 5,
      min_yrs_with_trips = 5,
      depth_band_width = 25,
      area_grep_pattern = area,
      depth_bin_quantiles = c(0.001, 0.999),
      min_bin_prop = 0.001,
      lat_band_width = 0.1)
    out$area <- area_name
    out
  }
  dfleet <- map2(params$area, params$area_name, define_fleet)
} else {
  define_fleet <- function(area, area_name) {
    out <- tidy_cpue_historic(d,
      species_common = tolower(params$species_proper),
      use_alt_year = params$april1_year,
      year_range = c(1956, 1995),
      depth_band_width = 25,
      area_grep_pattern = area,
      depth_bin_quantiles = c(0.001, 0.999),
      min_bin_prop = 0.001)
    out$area <- area_name
    out
  }
  dfleet <- map2(params$area, params$area_name, define_fleet)
}

gg_cpue$catch_effort <- dfleet %>% bind_rows() %>%
   group_by(year, area) %>%
   summarise(
     `Species catch` = sum(spp_catch)/1000,
     `Hours fished` = sum(hours_fished)/1000) %>%
   reshape2::melt(id.vars = c("year", "area")) %>%
   ggplot(aes(year, value)) +
   geom_line() +
   facet_grid(variable~area, scales = "free_y") +
   ylab("Value (1000 kg or 1000 hours)") + xlab("") +
   ylim(0, NA)

depth_bands <- as.numeric(as.character(unique(bind_rows(dfleet)$depth)))

gg_cpue$depth <- dfleet %>%
   bind_rows() %>%
   mutate(`Trip or fishing event\ncaught this species` =
       ifelse(pos_catch == 1, "Yes", "No")) %>%
   ggplot(aes(best_depth, fill = `Trip or fishing event\ncaught this species`)) +
   geom_histogram(binwidth = 10) +
   ylim(0, NA) +
   geom_vline(xintercept = depth_bands, lty = 2, col = "grey80") +
   coord_cartesian(expand = FALSE) +
   facet_wrap(~area, ncol = 2)

group <- if (params$era == "modern") "fishing_event_id" else "trip_id"
for (i in seq_along(dfleet)) {
  gg_cpue$bubble_loc <- gfplot:::plot_predictor_bubbles(dfleet[[i]], "locality", reorder_group = TRUE,
    group = group) %>% print()

  gg_cpue$bubble_dep <- dfleet[[i]] %>% mutate(depth = as.factor(as.character(depth))) %>%
    gfplot:::plot_predictor_bubbles("depth", reorder_group = FALSE,
      group = group)

  if (params$era == "modern") {
    gg_cpue$bubble_lat <- dfleet[[i]] %>% mutate(latitude = as.factor(as.character(latitude))) %>%
      gfplot:::plot_predictor_bubbles("latitude", reorder_group = FALSE,
        group = group)

 gg_cpue$bubble_ves <-  gfplot:::plot_predictor_bubbles(dfleet[[i]], "vessel", reorder_group = TRUE,
    group = group)
  }

 gg_cpue$bubble_mon <-  dfleet[[i]] %>% mutate(month = as.factor(as.character(month))) %>%
    gfplot:::plot_predictor_bubbles("month", reorder_group = FALSE,
      group = group)
}

for (i in seq_along(dfleet)) {
  dfleet[[i]]$year_locality <- paste(dfleet[[i]]$year_factor, dfleet[[i]]$locality)
}

if (params$era == "modern") {
  formulas <- data_frame(
    formula = c(
      "cpue ~ 0 + year_factor",
      "cpue ~ 0 + year_factor + depth",
      "cpue ~ 0 + year_factor + month",
      "cpue ~ 0 + year_factor + latitude",
      "cpue ~ 0 + year_factor + (1 | locality)",
      "cpue ~ 0 + year_factor + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel)",
      "cpue ~ 0 + year_factor + depth + month + latitude + (1 | locality) + (1 | vessel) + (1 | year_locality)"
    ),
    formula_version = c(
      "Unstandardized",
      "Depth",
      "Month",
      "Latitude",
      "Locality",
      "Vessel",
      "Full standardization minus interactions",
      "Full standardization"
    )
  )
} else {
  formulas <- data_frame(
  formula = c(
    "cpue ~ 0 + year_factor",
    "cpue ~ 0 + year_factor + depth",
    "cpue ~ 0 + year_factor + month",
    "cpue ~ 0 + year_factor + (1 | locality)",
    "cpue ~ 0 + year_factor + depth + month + (1 | locality)",
    "cpue ~ 0 + year_factor + depth + month + (1 | locality) + (1 | year_locality)"
  ),
  formula_version = c(
    "Unstandardized",
    "Depth",
    "Month",
    "Locality",
    "Full standardization minus interactions",
    "Full standardization"
  )
)
}

torun <- expand.grid(formula = formulas$formula,
  area = params$area_name, stringsAsFactors = FALSE)
torun <- inner_join(torun, formulas, by = "formula")

if (params$skip_single_variable_models) {
  torun <- filter(torun,
    formula_version %in% c("Unstandardized", "Full standardization minus interactions",
      "Full standardization"))
}

file_model <- here::here(paste0("data/generated/cpue-models-", 
    spp, "-", params$era, ".rds"))
if (!file.exists(file_model)) {
  system.time({
  model <- plyr::mlply(torun, function(formula, area, formula_version) {
    df <- dfleet[[which(params$area_name == area)]]
    message("Fitting area ", area, " and model ", formula)
    fit_cpue_index_glmmtmb(df, as.formula(formula))
    })
  })
  saveRDS(model, file_model)
} else {
  model <- readRDS(file_model)
}

predictions <- plyr::ldply(model, predict_cpue_index_tweedie)
## write_csv(predictions,
##   here::here(paste0("data/generated/cpue-predictions-", spp, "-", params$era, ".csv")))

arith_cpue <- dfleet %>%
  bind_rows() %>%
  group_by(area, year) %>%
  summarise(est = sum(spp_catch) / sum(hours_fished)) %>%
  mutate(model = "Combined") %>%
  group_by(area) %>%
  mutate(geo_mean = exp(mean(log(est)))) %>%
  mutate(est = est/geo_mean) %>%
  ungroup()

gg_cpue$pred <- predictions %>%
  filter(formula_version %in% c("Unstandardized", "Full standardization")) %>%
  gfplot:::plot_cpue_predictions("Combined", scale = TRUE) +
  geom_line(data = arith_cpue, aes(year, est),
    inherit.aes = FALSE, lty = 2) +
  scale_x_continuous(breaks = seq(1990, 2050, 5))

## tpredictions %>%
##   group_by(formula_version, model, area) %>%
##   mutate(geo_mean = exp(mean(log(est)))) %>%
##   mutate(upr = upr / geo_mean, lwr = lwr / geo_mean, est = est / geo_mean) %>%
##   ungroup() %>%
##   ggplot(aes(year, est, ymin = lwr, ymax = upr,
##     colour = formula_version, fill = formula_version)) + geom_line() +
##   geom_ribbon(alpha = 0.5) +
##   facet_wrap(~area, ncol = 1) +
##   ylab("CPUE (kg/hour) divided\nby geometric mean")
