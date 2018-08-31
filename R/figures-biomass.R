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
    xlim(c(min(bt$Year - 1), NA))

  if(!depl){
    p <- p + geom_pointrange(data = bo,
                             size = 0.5,
                             position = position_dodge(width = horiz.offset),
                             mapping = aes(color = Sensitivity))
  }

  if(!depl & add.hist.ref){
    if(is.na(lrp) | is.na(usr)){
      cat0("Supply year ranges for both lrp and usr when add.hist.ref is TRUE")
    }else{
      cal <- bt %>%
        filter(Year >= lrp[1] & Year <= lrp[2])
      lrp.val <- mean(cal$`Biomass (t)`)

      cau <- bt %>%
        filter(Year >= usr[1] & Year <= usr[2])
      usr.val <- mean(cau$`Biomass (t)`)

      p <- p + geom_hline(aes(yintercept = lrp.val),
                          linetype = "dashed",
                          color = "red",
                          size = 1,
                          show.guide = TRUE) +
        geom_hline(aes(yintercept = usr.val),
                   linetype = "dashed",
                   color = "green",
                   size = 1,
                   show.guide = TRUE)
    }
  }

  if(depl){
    p <- p + ylab("Reletive biomass")
  }

  p
}

make.depletion.mcmc.plot <- function(models,
                                     model.names = NULL,
                                     ylim = c(0, 1),
                                     opacity = 75,
                                     append.base.txt = NULL,
                                     ind.letter = NULL,
                                     leg = NULL,
                                     ...
                                     ){
  ## Plot the depletion with credibility intervals for the mcmc
  ##  case of the model
  ##
  ## opacity - how opaque the envelope is
  ## append.base.txt - text to append to the name of the first model

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  depl <- lapply(models,
                 function(x){
                   x$mcmccalcs$depl.quants})
  yrs <- lapply(depl,
                function(x){
                  as.numeric(colnames(x))})
  xlim <- lapply(1:length(yrs),
                 function(x){
                   c(min(yrs[[x]]), max(yrs[[x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  if(is.null(dev.list())){
    ## If layout() is used outside this function,
    ##  it calls plot.new and will mess up the figures
    ##  if we call it again
    plot.new()
  }
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")

  lapply(1:length(yrs),
         function(x){
           draw.envelope(yrs[[x]],
                         depl[[x]],
                         ylab = "",
                         xlab = "",
                         col = x,
                         las = 1,
                         xlim = xlim,
                         ylim = ylim,
                         opacity = opacity,
                         first = ifelse(x == 1, TRUE, FALSE),
                         ...)})
  mtext("Year", 1, line = 3)
  mtext("Depletion", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    if(!is.null(append.base.txt)){
      model.names[[1]] <- paste0(model.names[[1]],
                                 append.base.txt)
    }
    legend(leg,
           model.names,
           bg = "transparent",
           col = 1:length(models),
           lty = 1,
           lwd = 2)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

make.biomass.retro.mpd.plot <- function(base.model,
                                        models,
                                        model.names = NULL,
                                        ylim,
                                        offset = 0.1,
                                        show.bo.line = FALSE,
                                        ind.letter = NULL,
                                        leg = TRUE,
                                        color.brew.class = "Paired",
                                        ...
                                        ){
  ## Plot the biomass for the mpd case of the models
  ##
  ## bo.offset - offset to the left for the bo point an bars
  ## offset - the amount on the x-axis to offset each point and line for
  ##  multiple models
  ## append.base.txt - text to append to the name of the first model
  ## show.bo.line - show the reference lines 0.2 and 0.4bo

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  sbt <- lapply(models,
                function(x){
                  x[[1]]$mpd$sbt})

  sbo <- base.model$mpd$sbo

  yrs <- lapply(models,
                function(x){
                  x[[1]]$mpd$syr:(x[[1]]$mpd$nyr + 1)})
  xlim <- lapply(1:length(yrs),
                 function(x){
                   c(min(yrs[[x]]), max(yrs[[x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  if(is.null(dev.list())){
    ## If layout() is used outside this function,
    ##  it calls plot.new and will mess up the figures
    ##  if we call it again
    plot.new()
  }
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")

  base.yrs <- base.model$mpd$syr:(base.model$mpd$nyr + 1)
  base.sbt <- base.model$mpd$sbt

  cols <- colorRampPalette(c("red", "blue", "green"))(length(model.names))

  if(leg){
    layout(1:2, heights = c(1, 5))
    par(mar = rep(0, 4))
    plot(0, 0, type="n", ann=FALSE, axes=FALSE)
    legend("center",
           model.names,
           bg = "transparent",
           ncol = 6,
           col = c(1, cols),
           lty = c(1, rep(2, length(model.names) - 1)),
           lwd = 2)
    par(mar=c(5,4,0,2))
  }
  plot(base.yrs,
       base.sbt,
       col = 1,
       lwd = 3,
       las = 1,
       lty = 1,
       xlim = xlim,
       ylim = ylim,
       xlab = "",
       ylab = "",
       type = "l",
       ...)
  lapply(1:length(yrs),
         function(x){
           lines(yrs[[x]],
                 sbt[[x]],
                 xlab = "",
                 ylab = "",
                 col = cols[x],
                 las = 1,
                 lwd = 2,
                 lty = 2,
                 xlim = xlim,
                 ylim = ylim,
                 ...)})

  if(show.bo.line){
    abline(h = 0.3 * sbo,
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.3SB"[0]),
          4,
          at = 0.3 * sbo,
          col = "red",
          las = 1)
  }

  mtext("Year", 1, line = 3)
  mtext("Biomass (1000 mt)", 2, line = 3)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
