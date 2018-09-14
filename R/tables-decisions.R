decision.table <- function(models,
                           models.names,
                           burnin = 1000,
                           thin = 1,
                           caption = ""){

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))

  colnames(dat) <- c("$2018$ Catch (mt)",
                     "$P(B_{2019} < B_{2018})$",
                     "$P(F_{2018} > F_{2017})$",
                     "$P(B_{2019} < \\mathrm{LRP})$",
                     "$P(B_{2019} < \\mathrm{USR})$",
                     "$P(F_{2018} > \\mathrm{LRR})$")

  ## Assume all models have the same TACs
  tac <- models[[1]]$proj$tac.vec
  for(t in seq_along(tac)){
    proj <- lapply(models,
                   function(x){
                     j <- as.data.frame(x$mcmc$proj)
                     j <- j[j$TAC == tac[t],]
                     j <- mcmc.thin(j, burnin, thin)
                   })
    names(proj) <- models.names
    d <- bind_rows(proj)

    dat[t, 1] <- f(tac[t], 0)
    dat[t, 2] <- f(mean(d$B2019B2018 < 1), 2)   #P(B2019<B2018)
    dat[t, 3] <- f(mean(d$F2018F2017 > 1), 2)   #P(F2018>F2017)
    dat[t, 4] <- f(mean(d$B2019Bmin < 1), 2)   #P(B2019<Bmin)
    dat[t, 5] <- f(mean(d$B2019BAvgS < 1), 2)  #P(B2019<BAvg) #Avg 1956-2004
    dat[t, 6] <- f(mean(d$F2018FAvgS > 1), 2)  #P(F2018>FAvg) #Avg 1956-2004
  }

  kable(dat,
        caption = caption,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
  kableExtra::column_spec(1, width = "2.7cm") %>%
  kableExtra::column_spec(2:6, width = "2.0cm")
}
