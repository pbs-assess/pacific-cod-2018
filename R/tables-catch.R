catch.table <- function(dat,
                        dat.disc,
                        area = "NA"){
  ## dat is what comes out of data/get-data.R/total.catch.yr.qtr
  ## dat.disc is what comes out of data/get-data.R/total.catch.discards

  dat <- dat %>%
    group_by(year) %>%
    summarize(Year = year[1],
              USA = sum(usa_catch)) %>%
    select(-year)

  dat.disc <- dat.disc %>%
    group_by(year) %>%
    summarize(Year = year[1],
              `Canada landed` = sum(landed_kg),
              `Canada released at sea` = sum(discarded_kg),
              `Canada total` = `Canada landed` + `Canada released at sea`) %>%
    select(-year)

  j <- left_join(dat, dat.disc, by = "Year") %>%
    mutate(`Canada released at sea` = if_else(is.na(`Canada released at sea`),
                                       0,
                                       `Canada released at sea`),
                                        `Total catch` = `Canada total` +`USA`)
  j <- j[!is.na(j$`Total catch`),]

  j <- j[c("Year", "Canada landed", "Canada released at sea", "Canada total", "USA", "Total catch")]

  j[,-c(1,5)] = j[,-c(1,5)] / 1000
  j[,-1] <- round(j[,-1], 0)

  kable(j, caption = paste0("Reported catch (mt) of Pacific Cod in Area ", area,
                            " by Canada and the USA, ", min(j$Year), "--", max(j$Year),
                            ". The reported discards for the period ", min(j$Year), "--1995 are ",
                            "unrepresentative of true discarding because the estimates were taken ",
                            "from logbooks. Discard estimates since 1996 are based on at-sea ",
                            "observations and are considered to be more representative of true discarding."),
        #latex = TRUE,
        booktabs = TRUE,
        longtable = TRUE,
        linesep = "",
        escape = FALSE) %>%
    ##    kable_styling() %>%
    column_spec(c(2, 4, 5, 6), width = "2cm") %>%
    column_spec(3, width = "4cm") %>%
    kable_styling(latex_options = c("hold_position", "repeat_header"))
}
