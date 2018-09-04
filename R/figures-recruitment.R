r.plot <- function(models,
                   models.names,
                   type = 1,
                   add.meds = FALSE){
  ## type: 1 = recruitment, not 1 = recruitment deviations
  ## add.meds: Add median and mean line

  rt <- lapply(models,
               function(x){
                 if(type == 1){
                   x$mcmccalcs$recr.quants
                 }else{
                   x$mcmccalcs$recr.devs.quants
                 }})
  yrs <- lapply(rt,
                function(x){
                  as.numeric(colnames(x))})

  names(rt) <- models.names
  rt <- lapply(rt,
               function(x){
                 tmp <- as.data.frame(t(x))
                 tmp %>% mutate(Year = rownames(tmp))})
  rt <- bind_rows(rt, .id = "Sensitivity") %>%
    as.tibble() %>%
    rename(`Recruits (thousands)` = `50%`) %>%
    mutate(Year = as.numeric(Year)) %>%
    mutate(Sensitivity = forcats::fct_relevel(Sensitivity,
                                              models.names[1],
                                              after = 0)) %>%
    select(-MPD)

  rt.median <- median(rt$`Recruits (thousands)`)
  rt.mean <- mean(rt$`Recruits (thousands)`)

  horiz.offset <- 2
  p <- ggplot(rt, aes(x = Year,
                      y = `Recruits (thousands)`,
                      ymin = `5%`,
                      ymax = `95%`)) +
    geom_pointrange(data = rt,
                    size = 0.25,
                    position = position_dodge(width = horiz.offset),
                    mapping = aes(color = Sensitivity)) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.title = element_blank()) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(0, 3000, 5))

  if(add.meds){
    j <- data.frame("Intercept" = c(rt.median, rt.mean),
                    "Color" = c("blue", "green"))
    p <- p + geom_hline(data = j,
                        aes(yintercept = Intercept),
                        color = j$Color,
                        linetype = "dashed",
                        size = 1,
                        show.guide = FALSE)
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }
  p
}
