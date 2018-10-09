decision.table <- function(models,
                           models.names,
                           burnin = 1000,
                           thin = 1,
                           caption = "",
                           make.table = TRUE,
                           format = "pandoc",
                           tac.vec = NA){

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))

  col.names <- c("$2019$ Catch (mt)",
                 "$P(B_{2020} < B_{2019})$",
                 "$P(F_{2019} > F_{2018})$",
                 "$P(B_{2020} < \\mathrm{LRP})$",
                 "$P(B_{2020} < \\mathrm{USR})$",
                 "$P(F_{2019} > \\mathrm{LRR})$")

  if(format == "html"){
    col.names <- c("2019 Catch (mt)",
                   "P(B2020 < B2019)",
                   "P(F2019 > F2018)",
                   "P(B2020 < LRP)",
                   "P(B2020 < USR)",
                   "P(F2019 > LRR)")
  }
  ## Assume all models have the same TACs
  tac <- models[[1]]$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }
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
    dat[t, 2] <- f(mean(d$B2020B2019 < 1), 2)   #P(B2019<B2018)
    dat[t, 3] <- f(mean(d$F2019F2018 > 1), 2)   #P(F2018>F2017)
    dat[t, 4] <- f(mean(d$B2020Bmin < 1), 2)   #P(B2019<Bmin)
    dat[t, 5] <- f(mean(d$B2020BAvgS < 1), 2)  #P(B2019<BAvg) #Avg 1956-2004
    dat[t, 6] <- f(mean(d$F2019FAvgS > 1), 2)  #P(F2018>FAvg) #Avg 1956-2004
  }

  dat <- mutate_at(dat, -1, function(x) gsub('0.00', '<0.01', x))
  dat <- mutate_at(dat, -1, function(x) gsub('1.00', '>0.99', x))
  if(make.table){
    kable(dat,
          caption = caption,
          booktabs = TRUE,
          longtable = TRUE,
          linesep = "",
          escape = FALSE,
          format = format,
          col.names = col.names) %>%
      kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
      kableExtra::column_spec(1, width = "2.7cm") %>%
      kableExtra::column_spec(2:6, width = "2.0cm")
  }else{
    dat
  }
}
