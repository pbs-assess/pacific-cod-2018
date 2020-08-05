decision.table <- function(model,
                           caption = "",
                           make.table = TRUE,
                           format = "pandoc",
                           tac.vec = NA,
                           make.lt.gt = TRUE,
                           french=FALSE){
  ## make.lt.gt = add less than and greater than
  ## sybols in table. Changes those columns to character

  model <- model[[1]]

  dat <- as.data.frame(matrix(NA,
                              ncol = 6,
                              nrow = length(tac)))
  if(format == "html"){
    col.names <- c(paste("2019", en2fr("Catch", translate = french, allow_missing = TRUE), "(mt)"),
                   "P(B2020 < B2019)",
                   "P(F2019 > F2018)",
                   "P(B2020 < LRP)",
                   "P(B2020 < USR)",
                   "P(F2019 > LRR)")

  }else{
    col.names <- c(paste("2019", en2fr("Catch", translate = french, allow_missing = TRUE), "(mt)"),
                   "$P(B_{2020} < B_{2019})$",
                   "$P(F_{2019} > F_{2018})$",
                   "$P(B_{2020} < \\mathrm{LRP})$",
                   "$P(B_{2020} < \\mathrm{USR})$",
                   "$P(F_{2019} > \\mathrm{LRR})$")
  }
  tac <- model$proj$tac.vec
  if(!is.na(tac.vec[1])){
    tac <- tac.vec[tac.vec %in% tac]
  }
  for(t in seq_along(tac)){
    d <- as.data.frame(model$mcmccalcs$proj.dat)
    d <- d[d$TAC == tac[t],]
    dat[t, 1] <- f(tac[t], 0)
    dat[t, 2] <- f(mean(d$B2020B2019 < 1), 2)
    dat[t, 3] <- f(mean(d$F2019F2018 > 1), 2)
    dat[t, 4] <- f(mean(d$B2020Bmin < 1), 2)
    dat[t, 5] <- f(mean(d$B2020BAvgS < 1), 2)
    dat[t, 6] <- f(mean(d$F2019FAvgS > 1), 2)
  }

  if(make.lt.gt){
    dat <- mutate_at(dat, -1,
                     function(x) gsub('0.00', '<0.01', x))
    dat <- mutate_at(dat, -1,
                     function(x) gsub('1.00', '>0.99', x))
  }

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

suggested.ref.points <- function(french=FALSE){

  #FIXME: é displaying incorrectly in pdf. RF has tried substituting Ã© for é in various parts in the if(french=TRUE)
  # section below and getting random results. The text is too long to add to the rosettafish library

  if(french==TRUE){
    definition_text <-c(latex.mlc(c("La biomasse estimée la plus faible représentant",
                                    "un état non souhaitable à éviter ($B_{\t{2000}}$",
                                    "en  5ABCD; $B_{\t{1986}}$ en 3CD)"),
                                  make.bold = FALSE),
                        "La biomasse moyenne à long terme (1956-2004)",
                        "La mortalité par pêche moyenne à long terme (1956-2004)",
                        "La biomasse en 2018",
                        "La mortalité par pêche en 2017")
    caption_text <- paste("Points de référence pour les modèles du scénario de référence ",
      "pour les zones 5ABCD et 3CD.")

  }else {
    definition_text <-c(latex.mlc(c("Lowest estimated biomass agreed to be an",
                                  "undesirable state to avoid ($B_{\t{2000}}$",
                                  "in  5ABCD; $B_{\t{1986}}$ in 3CD)"),
                                make.bold = FALSE),
                      "Average biomass for the period 1956-2004",
                      "Average fishing mortality for the period 1956-2004",
                      "Biomass in 2018",
                      "Fishing mortality in 2017")
    caption_text <- paste0("Reference points for the Reference Case ",
                         "5ABCD and 3CD models.")
  }

   df <- data.frame(
    referencepoint = c("$B_{\t{Min}}$",
                       "$B_{\t{Avg}}$",
                       "$F_{\t{Avg}}$",
                       "$B_{\t{2018}}$",
                       "$F_{\t{2017}}$"),
    Definition = definition_text,
    Role = c(en2fr("LRP", translate=french, allow_missing=TRUE),
             en2fr("USR", translate=french, allow_missing=TRUE),
             en2fr("LRR", translate=french, allow_missing=TRUE),
             en2fr("Benchmark", translate=french, allow_missing=TRUE),
                   en2fr("Benchmark", translate=french, allow_missing=TRUE))) %>%
    rename("Reference point" = referencepoint)

  colnames(df) <- en2fr(colnames(df), translate = french, allow_missing = TRUE)
  colnames(df) <- latex.bold(colnames(df))
  kable(df,
        caption = caption_text,
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        format = "pandoc",
        align = c("l", "l", "l")) %>%
    column_spec(2, width = "10cm") %>%
    kableExtra::kable_styling(latex_options = "hold_position")
}
