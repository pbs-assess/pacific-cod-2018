b.plot <- function(models,
                   models.names,
                   depl = FALSE,
                   add.hist.ref = FALSE,
                   lrp = NA,
                   usr = NA){
  ## lrp usr are year ranges (2-element vectors) to take the mean of
  ## the biomass for the reference points

  ## Biomass or Depletion
  if(depl){
    bt.quants <- lapply(models,
                        function(x){
                          x$mcmccalcs$depl.quants})
  }else{
    bt.quants <- lapply(models,
                        function(x){
                          x$mcmccalcs$sbt.quants})
  }

  names(bt.quants) <- models.names
  bt.quants <- lapply(bt.quants,
                      function(x){
                        tmp <- as.data.frame(t(x))
                        tmp %>% mutate(Year = rownames(tmp))})
  bt <- bind_rows(bt.quants, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Biomass (t)` = `50%`) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  ## B0 values
  r.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$r.quants})
  bo.raw <- lapply(r.quants,
                   function(x){
                     x[rownames(x) == "bo", ]})
  bo <- lapply(bo.raw,
               function(x){
                 as.numeric(x[,2:4])})
  bo <- as.data.frame(do.call(rbind, bo))
  bo <- cbind(models.names, bo)
  names(bo) <- c("Sensitivity", "5%", "50%", "95%")
  bo <- as.tibble(bo) %>%
    mutate(Year = min(bt$Year)) %>%
    rename("Biomass (t)" = "50%")

  horiz.offset <- 1.7
  p <- ggplot(bt, aes(x = Year,
                       y = `Biomass (t)`,
                       ymin = `5%`,
                       ymax = `95%`,
                       fill = Sensitivity)) +
    geom_ribbon(alpha = 0.2) +
    geom_line(aes(color = Sensitivity),
              size = 1) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    xlim(c(min(bt$Year - 1), NA)) +
    scale_x_continuous(breaks = seq(0, 3000, 5))

  if(!depl){
    p <- p + geom_pointrange(data = bo,
                             size = 0.25,
                             position = position_dodge(width = horiz.offset),
                             mapping = aes(color = Sensitivity),
                             show.legend = FALSE)
  }

  if(!depl & add.hist.ref){
    if(is.na(lrp) || is.na(usr)){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.val <- mean(cal$`Biomass (t)`)

      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2])
      usr.val <- mean(cau$`Biomass (t)`)
      j <- data.frame("Intercept" = c(lrp.val, usr.val),
                      "Color" = c("red", "green"))
      p <- p +
        geom_hline(data = j,
                   aes(yintercept = Intercept),
                   color = j$Color,
                   linetype = "dashed",
                   size = 1,
                   show.guide = TRUE)
    }
  }

  if(depl){
    p <- p + ylab("Reletive biomass")
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}

b.plot.mpd <- function(models,
                       models.names,
                       depl = FALSE){
  yrs <- lapply(models,
                function(x){
                  x$mpd$syr:(x$mpd$nyr + 1)})

  if(depl){
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt / x$mpd$sbo })
  }else{
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt})
  }
  bt <- lapply(1:length(bt),
               function(x){
                 tmp <- as.data.frame(t(bt[[x]]))
                 rownames(tmp) <- "Biomass (t)"
                 colnames(tmp) <-  yrs[[x]]
                 tmp})
  names(bt) <- models.names

  bt <- bind_rows(bt, .id = "Sensitivity") %>%
    melt() %>%
    as.tibble() %>%
    mutate(Year = variable, `Biomass (t)` = value) %>%
    select(-c(variable, value)) %>%
    mutate(Year = as.numeric(as.character(Year))) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0))

  bo <- lapply(models,
               function(x){
                 x$mpd$sbo
               })
  names(bo) <- models.names
  bo <- t(bind_cols(bo))
  bo <- cbind(rownames(bo), bo, min(bt$Year))
  colnames(bo) <- c("Sensitivity", "Biomass (t)", "Year")
  bo <- bo %>%
    as.tibble() %>%
    mutate(`Biomass (t)` = as.numeric(`Biomass (t)`),
           Year = as.numeric(Year))

  horiz.offset <- 1.7
  p <- ggplot(bt, aes(x = Year,
                      y = `Biomass (t)`,
                      #ymin = 0,
                      #ymax = max(`Biomass (t)`),
                      group = Sensitivity)) +
    geom_line(aes(color = Sensitivity),
              size = 1) +
        theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 5))

  if(!depl){
    p <- p + geom_point(data = bo,
                        size = 2,
                        position = position_dodge(width = horiz.offset),
                        mapping = aes(color = Sensitivity),
                        show.legend = FALSE)
  }

  if(depl){
    p <- p + ylab("Relative biomass")
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }

  p
}
