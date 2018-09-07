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

## Example of how to view by year for 3CD:
## c3cd <- catch.3 %>%
##   group_by(year) %>%
##   summarize(canada = sum(catch_weight),
##             usa = sum(usa_catch2),
##             total_catch = sum(total_catch))

catch.5 <- total.catch.yr.qtr(dat$catch,
                              areas = "5[ABCD]+",
                              include.usa = TRUE)

catch.5ab <- total.catch.yr.qtr(dat$catch,
                                areas = "5[AB]+",
                                include.usa = TRUE)

catch.5cd <- total.catch.yr.qtr(dat$catch,
                                areas = "5[CD]+",
                                include.usa = TRUE)

catch.5e <- total.catch.yr.qtr(dat$catch,
                                areas = "5[E]+",
                                include.usa = FALSE)

################################################################################
## Base models and some of their outputs simplified
## Base model Haida Gwaii
## hg.b <- base.models[[1]][[1]]
## hg.am1 <- sens.models.1[[1]][[1]]
## hg.am1.constm <- sens.models.2[[1]][[1]]
## hg.am1.tvm <- sens.models.3[[1]][[1]]
## hg.am2.constm <- sens.models.4[[1]][[1]]
## hg.am2.tvm <- sens.models.5[[1]][[1]]
## hg.qa <- sens.models.6[[1]][[1]]
## hg.qb <- sens.models.7[[1]][[1]]
## hg.qc <- sens.models.8[[1]][[1]]

## hg.b.params <- as.data.frame(hg.b$mcmc$params)
## hg.b.mcc <- hg.b$mcmccalcs
## hg.b.p.quants <- as.data.frame(hg.b.mcc$p.quants)
## hg.b.r.quants <- as.data.frame(hg.b.mcc$r.quants)
## hg.sbt <- hg.b.mcc$sbt.quants
## hg.sbt.final <- f(1000 * hg.sbt[, ncol(hg.sbt)])
## hg.sbt.final.yr <- as.numeric(colnames(hg.sbt)[ncol(hg.sbt)])
## hg.sbt.first.yr <- as.numeric(colnames(hg.sbt)[1])
## hg.f.mort <- hg.b.mcc$f.mort.quants[[1]]
## hg.max.f.mort <- max(hg.f.mort[2,])
## hg.max.f.mort.ind <- which(hg.f.mort[2,] == hg.max.f.mort)
## hg.max.f.mort.yr <- colnames(hg.f.mort)[hg.max.f.mort.ind]
## hg.max.f.mort <- f(hg.f.mort[, hg.max.f.mort.ind], 3)
## hg.last.f.mort.yr <- as.numeric(colnames(hg.f.mort)[ncol(hg.f.mort)])
## hg.last.f.mort <- f(hg.f.mort[, ncol(hg.f.mort)], 3)
## hg.median.bio.min  <- f(min(hg.b.mcc$sbt.quants[2,]), 3)
## hg.median.bio.min.year <- names(which.min(min(hg.b.mcc$sbt.quants[2,])))
## hg.median.bio.max  <- f(max(hg.b.mcc$sbt.quants[2,]), 3)
## hg.median.bio.max.year <- names(which.max(hg.b.mcc$sbt.quants[2,]))
## hg.priors <- as.data.frame(hg.b$ctl$params)
## ## Steepness prior for base
## hg.h.prior <- hg.priors[rownames(hg.priors) == "h",]
## hg.h.prior.alpha <- hg.h.prior$p1
## hg.h.prior.beta <- hg.h.prior$p2
## hg.h.prior.mean <- hg.h.prior.alpha / (hg.h.prior.alpha + hg.h.prior.beta)
## hg.h.prior.cv <- f(sqrt((hg.h.prior.alpha * hg.h.prior.beta) /
##                         ((hg.h.prior.alpha + hg.h.prior.beta)^2 *
##                          (hg.h.prior.alpha + hg.h.prior.beta + 1))) /
##                    hg.h.prior.mean, 2)
## hg.h.prior.alpha <- f(hg.h.prior.alpha, 1)
## hg.h.prior.beta <- f(hg.h.prior.beta, 1)
## hg.h.prior.mean <- f(hg.h.prior.mean, 2)
## ## Survey Q
## hg.q <- as.data.frame(hg.b$ctl$surv.q)[,1]
## hg.q.mean <- f(exp(hg.q[2]), 1)
## hg.q.sd <- f(hg.q[3], 1)
## ## Age comp data values
## ## Gear indices, note they are not the indices in the model,
## ##  but the indices of the list elements in the model$dat$age.comps list
## hg.fishery.gear.1 <- 1
## hg.fishery.gear.2 <- 2
## hg.fishery.gear.3 <- 3
## hg.survey.gear.1 <- 4
## hg.survey.gear.2 <- 5
## hg.fishery.gear.1.yrs <- sort(unique(hg.b$dat$age.comps[[hg.fishery.gear.1]][,1]))
## hg.fishery.gear.1.yrs <- paste0(min(hg.fishery.gear.1.yrs), "--", max(hg.fishery.gear.1.yrs))
## hg.fishery.gear.2.yrs <- sort(unique(hg.b$dat$age.comps[[hg.fishery.gear.2]][,1]))
## hg.fishery.gear.2.yrs <- paste0(min(hg.fishery.gear.2.yrs), "--", max(hg.fishery.gear.2.yrs))
## hg.fishery.gear.3.yrs <- sort(unique(hg.b$dat$age.comps[[hg.fishery.gear.1]][,1]))
## hg.fishery.gear.3.yrs <- paste0(min(hg.fishery.gear.3.yrs), "--", max(hg.fishery.gear.3.yrs))
## ## Catch data values
## hg.catch.yrs <- sort(unique(hg.b$dat$catch[,1]))
## hg.catch.yrs <- paste0(min(hg.catch.yrs), "--", max(hg.catch.yrs))
## hg.sig.tau <- calc.sig.tau(hg.b$ctl$params[6, 1], hg.b$ctl$params[7, 1])

## ## Base model Prince Rupert
## pr.b <- base.models[[2]][[1]]
## pr.am1 <- sens.models.1[[2]][[1]]
## pr.am1.constm <- sens.models.2[[2]][[1]]
## pr.am1.tvm <- sens.models.3[[2]][[1]]
## pr.am2.constm <- sens.models.4[[2]][[1]]
## pr.am2.tvm <- sens.models.5[[2]][[1]]
## pr.qa <- sens.models.6[[2]][[1]]
## pr.qb <- sens.models.7[[2]][[1]]
## pr.qc <- sens.models.8[[2]][[1]]

## ## Base model Central coast
## cc.b <- base.models[[3]][[1]]
## cc.am1 <- sens.models.1[[3]][[1]]
## cc.am1.constm <- sens.models.2[[3]][[1]]
## cc.am1.tvm <- sens.models.3[[3]][[1]]
## cc.am2.constm <- sens.models.4[[3]][[1]]
## cc.am2.tvm <- sens.models.5[[3]][[1]]
## cc.qa <- sens.models.6[[3]][[1]]
## cc.qb <- sens.models.7[[3]][[1]]
## cc.qc <- sens.models.8[[3]][[1]]

## ################################################################################
## ## Base model Strait of Georgia
## sog.b <- base.models[[4]][[1]]
## sog.am1 <- sens.models.1[[4]][[1]]
## sog.am1.constm <- sens.models.2[[4]][[1]]
## sog.am1.tvm <- sens.models.3[[4]][[1]]
## sog.am2.constm <- sens.models.4[[4]][[1]]
## sog.am2.tvm <- sens.models.5[[4]][[1]]
## sog.qa <- sens.models.6[[4]][[1]]
## sog.qb <- sens.models.7[[4]][[1]]
## sog.qc <- sens.models.8[[4]][[1]]

## sog.b.params <- as.data.frame(sog.b$mcmc$params)
## sog.b.mcc <- sog.b$mcmccalcs
## sog.b.p.quants <- as.data.frame(sog.b.mcc$p.quants)
## sog.b.r.quants <- as.data.frame(sog.b.mcc$r.quants)
## sog.sbt <- sog.b.mcc$sbt.quants
## sog.sbt.final <- f(1000 * sog.sbt[, ncol(sog.sbt)])
## sog.sbt.final.yr <- as.numeric(colnames(sog.sbt)[ncol(sog.sbt)])
## sog.sbt.first.yr <- as.numeric(colnames(sog.sbt)[1])
## sog.f.mort <- sog.b.mcc$f.mort.quants[[1]]
## sog.max.f.mort <- max(sog.f.mort[2,])
## sog.max.f.mort.ind <- which(sog.f.mort[2,] == sog.max.f.mort)
## sog.max.f.mort.yr <- colnames(sog.f.mort)[sog.max.f.mort.ind]
## sog.max.f.mort <- f(sog.f.mort[, sog.max.f.mort.ind], 3)
## sog.last.f.mort.yr <- as.numeric(colnames(sog.f.mort)[ncol(sog.f.mort)])
## sog.last.f.mort <- f(sog.f.mort[, ncol(sog.f.mort)], 3)
## sog.median.bio.min  <- f(min(sog.b.mcc$sbt.quants[2,]), 3)
## sog.median.bio.min.year <- names(which.min(min(sog.b.mcc$sbt.quants[2,])))
## sog.median.bio.max  <- f(max(sog.b.mcc$sbt.quants[2,]), 3)
## sog.median.bio.max.year <- names(which.max(sog.b.mcc$sbt.quants[2,]))
## sog.priors <- as.data.frame(sog.b$ctl$params)
## ## Steepness prior for base
## sog.h.prior <- sog.priors[rownames(sog.priors) == "h",]
## sog.h.prior.alpha <- sog.h.prior$p1
## sog.h.prior.beta <- sog.h.prior$p2
## sog.h.prior.mean <- sog.h.prior.alpha / (sog.h.prior.alpha + sog.h.prior.beta)
## sog.h.prior.cv <- f(sqrt((sog.h.prior.alpha * sog.h.prior.beta) /
##                          ((sog.h.prior.alpha + sog.h.prior.beta)^2 *
##                           (sog.h.prior.alpha + sog.h.prior.beta + 1))) /
##                     sog.h.prior.mean, 2)
## sog.h.prior.alpha <- f(sog.h.prior.alpha, 1)
## sog.h.prior.beta <- f(sog.h.prior.beta, 1)
## sog.h.prior.mean <- f(sog.h.prior.mean, 2)
## ## Survey Q
## sog.q <- as.data.frame(sog.b$ctl$surv.q)[,1]
## sog.q.mean <- f(exp(sog.q[2]), 1)
## sog.q.sd <- f(sog.q[3], 1)
## ## Age comp data values
## ## Gear indices, note they are not the indices in the model,
## ##  but the indices of the list elements in the model$dat$age.comps list
## sog.fishery.gear.1 <- 1
## sog.fishery.gear.2 <- 2
## sog.fishery.gear.3 <- 3
## sog.survey.gear.1 <- 4
## sog.survey.gear.2 <- 5
## sog.fishery.gear.1.yrs <- sort(unique(sog.b$dat$age.comps[[sog.fishery.gear.1]][,1]))
## sog.fishery.gear.1.yrs <- paste0(min(sog.fishery.gear.1.yrs), "--", max(sog.fishery.gear.1.yrs))
## sog.fishery.gear.2.yrs <- sort(unique(sog.b$dat$age.comps[[sog.fishery.gear.2]][,1]))
## sog.fishery.gear.2.yrs <- paste0(min(sog.fishery.gear.2.yrs), "--", max(sog.fishery.gear.2.yrs))
## sog.fishery.gear.3.yrs <- sort(unique(sog.b$dat$age.comps[[sog.fishery.gear.1]][,1]))
## sog.fishery.gear.3.yrs <- paste0(min(sog.fishery.gear.3.yrs), "--", max(sog.fishery.gear.3.yrs))
## ## Catch data values
## sog.catch.yrs <- sort(unique(sog.b$dat$catch[,1]))
## sog.catch.yrs <- paste0(min(sog.catch.yrs), "--", max(sog.catch.yrs))
## sog.sig.tau <- calc.sig.tau(sog.b$ctl$params[6, 1], sog.b$ctl$params[7, 1])

## ## Base model WCVI
## wcvi.b <- base.models[[5]][[1]]
## wcvi.am1 <- sens.models.1[[5]][[1]]
## wcvi.am1.constm <- sens.models.2[[5]][[1]]
## wcvi.am1.tvm <- sens.models.3[[5]][[1]]
## wcvi.am2.constm <- sens.models.4[[5]][[1]]
## wcvi.am2.tvm <- sens.models.5[[5]][[1]]
## wcvi.qa <- sens.models.6[[5]][[1]]
## wcvi.qb <- sens.models.7[[5]][[1]]
## wcvi.qc <- sens.models.8[[5]][[1]]

## ################################################################################
## ## Number of mcmc samples, min and max median biomass
## mcmc.num.samples <- nrow(hg.b.params)
## mcmc.burnin <- f(mcmc.num.samples - nrow(hg.b.mcc$p.dat))
## mcmc.num.samples <- f(mcmc.num.samples)
## mcmc.length <- "5 million"
## mcmc.samp.freq <- f(1000)
## mcmc.ci <- "90\\%"

## ################################################################################
## ## Values for assessment
## ##bo <- f(1000 * b.r.quants["bo", -1])
## ##bmsy <- f(1000 * b.r.quants["bmsy", -1])
## ##fmsy <- f(b.r.quants["fmsy", -1], 2)

## ##s6.r.quants <- as.data.frame(sens.models.3[[1]]$mcmccalcs$r.quants)
## ##sens.6.bo <- f(1000 * s6.r.quants["bo", -1])

## ##depl <- b.mcc$depl.quants
## ##last.depl.yr <- colnames(depl)[ncol(depl)]
## ##last.depl <- f(depl[, ncol(depl)], 3)

## ## survey.1.q <- f(b.p.quants$q1, 2)
## ## survey.1.q <- f(b.p.quants$q2, 2)

## ## trawl.a50 <- f(b.p.quants$sel1, 2)
## ## qcsss.a50 <- f(b.p.quants$sel2, 2)

## ##s13.sel <- as.data.frame(sens.models.8[[2]]$ctl$sel)
## ##s13.trawl.a50 <- s13.sel[rownames(s13.sel) == "agelen50log", 1]

## ##h.post <- f(b.p.quants$h, 3)
## ##sens.6.h.post <- f(as.data.frame(sens.models.3[[1]]$mcmccalcs$p.quants)$h, 3)

## ## Projection values and probabilities
## ## tacs <- b$proj$tac.vec
## ## min.tac <- f(1000 * min(tacs))
## ## max.tac <- f(1000 * max(tacs))

## ## tac.probs <- b.mcc$proj.dat
## ## tac.0.prob <- f(100 * tac.probs[1, 4])
## ## tac.30.prob <- f(100 * tac.probs[which(tac.probs[, 1] == 30), 4], 1)
## ## tac.50.prob <- f(100 * tac.probs[which(tac.probs[, 1] == 50), 4], 1)

## ##tac.4bo <- tac.probs[, 5]
## ##min.tac.4b0 <- f(100 * min(tac.4bo), 1)
## ##max.tac.4b0 <- f(100 * max(tac.4bo), 1)

## ##tac.depl <- tac.probs[, 7]
## ##min.tac.depl <- f(100 * min(tac.depl), 1)
## ##max.tac.depl <- f(100 * max(tac.depl), 1)

## ################################################################################
## ## Priors settings from the control file
## ## priors <- as.data.frame(b$ctl$params)
## ##s6.priors <- as.data.frame(sens.models.3[[1]]$ctl$params)
## ##s7.priors <- as.data.frame(sens.models.4[[1]]$ctl$params)
## ##s8.priors <- as.data.frame(sens.models.4[[2]]$ctl$params)
## ##s9.priors <- as.data.frame(sens.models.5[[1]]$ctl$params)

## ## M priors
## ##base.m.prior <- priors[rownames(priors) == "log_m",]
## ##base.m.prior.mean <- f(exp(base.m.prior$p1), 1)
## ##base.m.prior.sd <- f(base.m.prior$p2, 2)

## ##s7.prior <- s7.priors[rownames(s7.priors) == "log_m",]
## ##sens.7.m.prior.mean <- f(exp(s7.prior$p1), 1)
## ##sens.7.m.prior.sd <- f(s7.prior$p2, 2)

## ##s8.prior <- s8.priors[rownames(s8.priors) == "log_m",]
## ##sens.8.m.prior.mean <- f(exp(s8.prior$p1), 1)
## ##sens.8.m.prior.sd <- f(s8.prior$p2, 2)

## ##s9.prior <- s9.priors[rownames(s9.priors) == "log_m",]
## ##sens.9.m.prior.mean <- f(exp(s9.prior$p1), 1)
## ##sens.9.m.prior.sd <- f(s9.prior$p2, 2)

## ## Low steepness scenario
## ## sens.6.h.prior <- s6.priors[rownames(s6.priors) == "h",]
## ## sens.6.h.prior.alpha <- sens.6.h.prior$p1
## ## sens.6.h.prior.beta <- sens.6.h.prior$p2
## ## sens.6.h.prior.mean <- sens.6.h.prior.alpha / (sens.6.h.prior.alpha + sens.6.h.prior.beta)
## ## sens.6.h.prior.cv <- f(sqrt((sens.6.h.prior.alpha * sens.6.h.prior.beta) /
## ##                    ((sens.6.h.prior.alpha + sens.6.h.prior.beta)^2 *
## ##                     (sens.6.h.prior.alpha + sens.6.h.prior.beta + 1))) /
## ##                 sens.6.h.prior.mean, 2)
## ## sens.6.h.prior.alpha <- f(sens.6.h.prior.alpha, 1)
## ## sens.6.h.prior.beta <- f(sens.6.h.prior.beta, 1)
## ## sens.6.h.prior.mean <- f(sens.6.h.prior.mean, 2)

## ## Survey Q
## ##q <- as.data.frame(b$ctl$surv.q)[,1]
## ##q.mean <- f(exp(q[2]), 1)
## ##q.sd <- f(q[3], 1)

## ## sens.10.q <- as.data.frame(sens.models.6[[1]]$ctl$surv.q)[,1]
## ## sens.10.q.mean <- f(exp(sens.10.q[2]), 1)
## ## sens.10.q.sd <- f(sens.10.q[3], 1)

## ## sens.11.q <- as.data.frame(sens.models.7[[1]]$ctl$surv.q)[,1]
## ## sens.11.q.mean <- f(exp(sens.11.q[2]), 1)
## ## sens.11.q.sd <- f(sens.11.q[3], 1)

## ################################################################################

## ################################################################################
## ## Values for posteriors
## ##base.m.quants <- f(b.p.quants$m, 3)
## ##base.bo.quants <- f(1000 * b.r.quants[1, 2:4])

## ##sens.7.m.quants <- f(as.data.frame(sens.models.4[[1]]$mcmccalcs$p.quants)$m, 3)
## ##sens.7.bo.quants <- f(1000 * sens.models.4[[1]]$mcmccalcs$r.quants[1, 2:4])

## ##sens.8.m.quants <- f(as.data.frame(sens.models.4[[2]]$mcmccalcs$p.quants)$m, 3)
## ##sens.9.m.quants <- f(as.data.frame(sens.models.5[[1]]$mcmccalcs$p.quants)$m, 3)

## ################################################################################

## ################################################################################

## ## MPD calculation for sigma and tau for sensitivity 3
## ##tau.3 <- f(sens.models.1[[2]]$mpd$tau, 2)
## ##sigma.3 <- f(sens.models.1[[2]]$mpd$sig, 2)

## ## Min and max proportions female for app-propfemale
## ##min.prop.female <- f(min(prop.female[,-1], na.rm = TRUE), 3)
## ##max.prop.female <- f(max(prop.female[,-1], na.rm = TRUE), 3)
