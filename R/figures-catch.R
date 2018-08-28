make.catches.plot <- function(dat,
                              by.quarter = FALSE){

  if(!by.quarter){
    dat <- dat %>%
      select(-total_catch) %>%
      group_by(year) %>%
      summarize(usa_catch = sum(usa_catch),
                canada_catch = sum(canada_catch))
    dat <- melt(dat, id.vars = "year")
    ggplot(dat) +
      aes(x = year, y = value, fill = variable) +
      geom_col() +
      coord_cartesian(expand = FALSE) +
      labs(x = "Year",
           y = "Catch (t)",
           fill = "") +
      scale_fill_hue(labels = c("USA", "Canada")) +
      scale_y_continuous(labels = comma,
                         limits = c(min(dat$value),
                                    1.1 * max(dat$value))) +
      ##scale_x_continuous(
      theme_pbs() +
      theme(axis.text.x = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 20,
                                      face = "bold",
                                      color = "darkgreen"))
  }
}
