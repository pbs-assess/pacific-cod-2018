## Put any variables you intend to use in the text here.
## The function f() is for formatting and is defined in
##  r-functions/utilities.r
##
## The variables defined here depend on the structure of the
##  model-setup.r source code.

################################################################################
fish.name <- "Pacific cod"
science.name <- "Gadus macrocephalus"
common.name <- "Pacific cod"
Common.name <- "Pacific cod"
bc <- "British Columbia"

# Catch data for each area 3 = 3CD, 5 = 5ABCD

if (!"major_stat_area_name" %in% names(dat$catch)) {
  dat$catch <- left_join(dat$catch, gfplot::pbs_areas, by = "major_stat_area_code") %>%
    rename(major_stat_area_name = major_stat_area_description)
}

catch.3 <- total.catch.yr.qtr(dat$catch,
                              areas = "3[CD]+",
                              include.usa = TRUE)

catch.3.discards <- total.catch.discards(dat$catch, areas="3[CD]+")

## Example of how to view by year for 3CD:
## c3cd <- catch.3 %>%
##   group_by(year) %>%
##   summarize(canada = sum(catch_weight),
##             usa = sum(usa_catch2),
##             total_catch = sum(total_catch))

catch.5 <- total.catch.yr.qtr(dat$catch,
                              areas = "5[ABCD]+",
                              include.usa = TRUE)
catch.5.discards <- total.catch.discards(dat$catch, areas="5[ABCD]+")

catch.5ab <- total.catch.yr.qtr(dat$catch,
                                areas = "5[AB]+",
                                include.usa = TRUE)
catch.5ab.discards <- total.catch.discards(dat$catch, areas="5[AB]+")

catch.5cd <- total.catch.yr.qtr(dat$catch,
                                areas = "5[CD]+",
                                include.usa = TRUE)
catch.5cd.discards <- total.catch.discards(dat$catch, areas="5[CD]+")

catch.5e <- total.catch.yr.qtr(dat$catch,
                                areas = "5[E]+",
                                include.usa = FALSE)
catch.5e.discards <- total.catch.discards(dat$catch, areas="5[E]+")

q.5abcd.desc <- paste0("$q_1$ = Hecate Strait Assemblage survey, ",
                       "$q_2$ = Queen Charlotte Sound Synoptic Survey, ",
                       "$q_3$ = Hecate Strait Synoptic Survey, ",
                       "$q_4$ = Commercial CPUE pre-1996, and ",
                       "$q_5$ = Commercial CPUE post-1995.")

q.3cd.desc <- paste0("$q_1$ = West Coast Vancouver Island Synoptic Survey, ",
                     "$q_2$ = Commercial CPUE pre-1996, ",
                     "$q_3$ = Commercial CPUE post-1995, and ",
                     "$q_4$ = NMFS Triennial Survey (Canadian portion).")

b5 <- base.model.5abcd[[1]]
start.yr5 <- b5$dat$syr
end.yr5 <- b5$dat$nyr
sage5 <- b5$dat$sage
nage5 <- b5$dat$nage
linf5 <- b5$dat$linf
k5 <- b5$dat$k
lwscal5 <- b5$dat$lwscal
lwpow5 <- b5$dat$lwpow
t05 <- b5$dat$to
alpha.g5 <- b5$dat$dd.alpha.g
rho.g5 <- b5$dat$dd.rho.g
wk5 <- b5$dat$dd.wk

b3 <- base.model.3cd[[1]]
start.yr3 <- b3$dat$syr
end.yr3 <- b3$dat$nyr
sage3 <- b3$dat$sage
nage3 <- b3$dat$nage
linf3 <- b3$dat$linf
k3 <- b3$dat$k
lwscal3 <- b3$dat$lwscal
lwpow3 <- b3$dat$lwpow
t03 <- b3$dat$to
alpha.g3 <- b3$dat$dd.alpha.g
rho.g3 <- b3$dat$dd.rho.g
wk3 <- b3$dat$dd.wk
