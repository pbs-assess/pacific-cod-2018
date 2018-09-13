decision.table <- function(models,
                           burnin = 1000,
                           thin = 1,
                           caption = ""){

  ## Assume all models have the same TACs
  tac <- models[[1]]$proj$tac.vec
  proj <- models[[1]]$mcmc$proj

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))
  # colnames(dat) <- c("$TAC$",
  #                    "$P(B_{2019} LESS THAN B_{2018})$",
  #                    "$P(F_{2018} LESS THAN  F_{2017})$",
  #                    #"$P(B_{2019}<B_{0.8B_{MSY}})$",
  #                    #"$P(B_{2019}<B_{0.4B_{MSY}})$",
  #                    #"$P(F_{2018}>F_{MSY})$",
  #                    "$P(B_{2019} LESS THAN B_{min}}$)",
  #                    "$P(B_{2019} LESS THAN B_{AVG})$",
  #                    "$P(F_{2018} MORE THAN F_{AVG})$")
  colnames(dat) <- c("2018 Catch (mt)",
                     "P(B2019<B2018)",
                     "P(F2018>F2017)",
                     #"P(B2019<0.8BMSY)",
                     #"P(B2019<0.4BMSY)",
                     #"P(F2018>FMSY)",
                     "P(B2019<LRP)",
                     "P(B2019<USR)",
                     "P(F2018>LRR)")

  proj.dat <- data.frame()
  for(t in seq_along(tac)){
    results <- list()

    for(m in 1:length(models)){
      results[[m]] <- proj[proj$TAC == tac[t],]
      results[[m]] <- mcmc.thin(results[[m]], burnin, thin)
    }

    if(length(models) > 1){
      d <- plyr::rbind.fill(results)
    }else{
      d <- results[[1]]
    }

    dat[t, 1] <- tac[t]
    dat[t, 2] <- mean(d$B2019B2018 < 1)   #P(B2019<B2018)
    dat[t, 3] <- mean(d$F2018F2017 > 1)   #P(F2018>F2017)
#    dat[t, 4] <- mean(d$B20190.8BMSY < 1) #P(B2019<0.8BMSY)
#    dat[t, 5] <- mean(d$B20190.4BMSY < 1) #P(B2019<0.4BMSY)
#    dat[t, 6] <- mean(d$F2018FMSY > 1)    #P(F2018>FMSY)
    dat[t, 4] <- mean(d$B2019Bmin < 1)   #P(B2019<Bmin)
    dat[t, 5] <- mean(d$B2019BAvgS < 1)  #P(B2019<BAvg) #Avg 1956-2004
    dat[t, 6] <- mean(d$F2018FAvgS > 1)  #P(F2018>FAvg) #Avg 1956-2004
  }

  kable(dat,
        caption = caption,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE) %>%
#        align = c("l", "r", "r", "r", "r", "r")) %>%
#    column_spec(c(2, 4, 5, 6), width = "2cm") %>%
#    column_spec(3, width = "4cm") %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"))
}
