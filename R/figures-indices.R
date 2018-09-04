i.plot <- function(models,
                   models.names,
                   ind,
                   pan.let = ""){

  index.fit <- lapply(models,
                      function(x){
                        tmp <- x$mpd$it_hat[ind,]
                        tmp[!is.na(tmp)]
                      })
  index.dat <- lapply(models,
                      function(x){
                        as.data.frame(x$dat$indices[[ind]])
                      })

  i <- lapply(1:length(models),
              function(x){
                cbind(index.dat[[x]], fit = index.fit[[x]])
              })
  names(i) <- models.names
  i <- bind_rows(i, .id = "Sensitivity") %>%
    as.tibble() %>%
    mutate(lowercv = it - it * (1 / wt),
           uppercv = it + it * (1 / wt)) %>%
    rename("Year" = iyr,
           "Survey biomass index (t)" = it) %>%
    select(-c(gear, area, group, sex, wt, timing)) %>%
    mutate(Year = as.integer(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0))


  p <- ggplot(i) +
    aes(x = Year, y = `Survey biomass index (t)`) +
    geom_pointrange(aes(ymin = lowercv,
                        ymax = uppercv),
                    size = 0.25) +
    geom_line(aes(color = Sensitivity),
              y = i$fit,
              size = 1) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 1),
                       limits = c(min(i$Year - 1),
                                  max(i$Year + 1))) +
    geom_text(x = min(i$Year),
              y = max(0.9 * max(i$uppercv, i$fit)),
              label = ggpanel.letter(pan.let))

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
