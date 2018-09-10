make.parameters.table <- function(model,
                                  caption = "default"){

  get.bounds <- function(ind){
    ## Return the bounds string for row ind of the parameters
    paste0("[",
      params$lb[ind],
      ", ",
      params$ub[ind],
      "]")
  }

  get.vals <- function(ind){
    ## Return a 3-element vector for the number estimated, bounds, and prior
    ##  dist mean and SD or "Uniform" for the row ind of the parameters
    ##
    ## vec is a vector of 4 values, the phase, prior type (0=uniform, 1=normal,
    ##  2=lognormal, 3=beta, 4=gamma), the first and second parameter
    ##  of the prior.
    vec <- as.numeric(params[ind, 4:7])
    if(vec[1] < 1){
      return(c(0,
        "Fixed",
        paste0("$", f(params[ind, 1], 3), "$")))
    }else if(vec[2] == 0){
      return(c(1,
        get.bounds(ind),
        "Uniform"))
    }else if(vec[2] == 1){
      return(c(1,
        get.bounds(ind),
        paste0("Normal($ln(",
          vec[4],
          "), ",
          vec[4],
          "$)")))
    }else if(vec[2] == 2){
      return(c(1,
        get.bounds(ind),
        paste0("Lognormal($",
          vec[3],
          ", ",
          vec[4],
          "$)")))
    }else if(vec[2] == 3){
      return(c(1,
        get.bounds(ind),
        paste0("Beta($\\alpha = ",
          vec[3],
          ", \\beta = ",
          vec[4],
          "$)")))
    }else if(vec[2] == 4){
      return(c(1,
        get.bounds(ind),
        paste0("Gamma($k = ",
          vec[3],
          "), \\theta = ",
          vec[4],
          "$)")))
    }
    invisible()
  }

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  ctl <- model$ctl

  params <- as.data.frame(ctl$params)
  stopifnot(row.names(ctl$params) ==
      c("log_ro", "steepness", "log_m", "log_avgrec", "log_recinit",
    "rho", "kappa"))

  tab <- data.frame(param = character(),
    num.est = character(),
    bounds = character(),
    prior = character(),
    stringsAsFactors = FALSE)

  param.text <- c("Log recruitment ($ln(R_0)$)",
                  "Steepness ($h$)",
                  "Log natural mortality ($ln(M)$)",
                  "Log mean recruitment ($\\ln(\\overline{R})$)",
                  "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
                  "Variance ratio ($\\rho$)",
                  "Inverse total variance ($\\vartheta^2$)")

  param.vals <- do.call(rbind, lapply(1:nrow(params), get.vals))

  ## Selectivity parameters
  ## sel data frame has one column for each gear and 10 rows:
  ## 1  - selectivity type:
  ##       1) logistic selectivity parameters
  ##       2) selectivity coefficients
  ##       3) a constant cubic spline with age-nodes
  ##       4) a time varying cubic spline with age-nodes
  ##       5) a time varying bicubic spline with age & year nodes
  ##       6) fixed logistic (set isel_type=6, and estimation phase to -1)
  ##       7) logistic function of body weight.
  ##       8) logistic with weight deviations (3 parameters)
  ##       11) logistic selectivity with 2 parameters based on mean length
  ##       12) length-based selectivity coefficients with spline interpolation
  ## 2  - Age/length at 50% selectivity (logistic)
  ## 3  - STD at 50% selectivity (logistic)
  ## 4  - No. of age nodes for each gear (0=ignore)
  ## 5  - No. of year nodes for 2d spline(0=ignore)
  ## 6  - Phase of estimation (-1 for fixed) If neg number, it reflects a
  ##       mirroring of another gear's selectivity.
  ## 7  - Penalty wt for 2nd differences w=1/(2*sig^2)
  ## 8  - Penalty wt for dome-shaped w=1/(2*sig^2)
  ## 9  - Penalty wt for time-varying selectivity
  ## 10 - n_sel_blocks (number of selex blocks)

  sel <- ctl$sel
  dat <- model$dat
  indices <- dat$indices
  indices.df <- as.data.frame(do.call(rbind, indices))
  surv.gear.nums <- unique(indices.df$gear)
  surv.sel <- as.data.frame(sel[,surv.gear.nums])
  fish.sel <- as.data.frame(sel[,-surv.gear.nums])
  ## Get number estimated by looking at the phase row in the sel data frame
  surv.est <- surv.sel[6,]
  surv.est <- sum(surv.est > 0)
  fish.est <- fish.sel[6,]
  fish.est <- sum(fish.est > 0)
  ## Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  param.vals <- rbind(param.vals,
    c(surv.est,
      "[0, 1]",
      "None"),
    c(fish.est,
      "[0, 1]",
      "None"),
    c(surv.est,
      "[0, Inf)",
      "None"),
    c(fish.est,
      "[0, Inf)",
      "None"))

  param.text <- c(param.text,
    "Survey age at 50\\% selectivity ($\\hat{a}_k$)",
    "Fishery age at 50\\% selectivity ($\\hat{a}_k$)",
    "Survey SD of logistic selectivity ($\\hat{\\gamma}_k$)",
    "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)")

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  q <- ctl$surv.q
  num.inds <- ctl$num.indices
  param.vals <- rbind(param.vals,
    c(num.inds,
      "None",
      "Normal($0.5, 1$)"))

  param.text <- c(param.text,
    "Survey catchability ($q_k$)")

  ## Fishing mortality and recruitment parameters
  ##
  par <- model$par
  num.f.params <- length(par$log_ft_pars)
  num.rec.params <- length(par$log_rec_devs)
  num.init.rec.params <- length(par$init_log_rec_devs)
  param.vals <- rbind(param.vals,
    c(num.f.params,
      "[-30, 3]",
      "[-30, 3]"),
    c(num.rec.params,
      "None",
      "Normal($0, \\tau$)"),
    c(num.init.rec.params,
      "None",
      "Normal($0, \\tau$)"))

  param.text <- c(param.text,
    "Log fishing mortality values ($\\Gamma_{k,t}$)",
    "Log recruitment deviations ($\\omega_t$)",
    "Initial log recruitment deviations ($\\omega_{init,t}$)")

  tab <- cbind(param.text, param.vals)
  colnames(tab) <- c(latex.bold("Parameter"),
    latex.mlc(c("Number",
      "estimated")),
    latex.mlc(c("Bounds",
      "[low, high")),
    latex.mlc(c("Prior (mean, SD)",
      "(single value = fixed)")))

  knitr::kable(tab,
    caption = caption,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE)
}

make.parameters.est.table <- function(model,
  digits = 3,
  caption = "default"){
  ## Returns an xtable in the proper format for parameter estimates and priors
  ##
  ## digits - number of decimal points on % columns
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document


  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    if(pname == "m1"){
      pname <- "m"
    }
    if(pname == "bo"){
      pname <- "sbo"
    }
    match.sel <- grep("sel[[:digit:]]+",
      pname)
    match.sel.sd <- grep("selsd[[:digit:]]+",
      pname)
    match.q <- grep("q[[:digit:]]+",
      pname)
    ## Age value at 50%
    sel.pars <- mpd$sel_par[,3]
    ## Age SD at 50%
    sel.sd.pars <- mpd$sel_par[,4]
    q.pars <- mpd$q
    if(length(match.sel) > 0){
      ## The parameter starts with "sel"
      split.val <- strsplit(pname,
        "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.pars[sel.num]
    }else if(length(match.sel.sd) > 0){
      ## The parameter starts with "selsd"
      split.val <- strsplit(pname,
        "[^[:digit:]]")[[1]]
      sel.num <- as.numeric(split.val[length(split.val)])
      this.par <- sel.sd.pars[sel.num]
    }else if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
        "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else{
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    j <- mpd.param.vals <- c(mpd.param.vals, this.par)
  }
  names(mpd.param.vals) <- mcmc.names
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- "MPD"
  rownames(tab) <- row.n
  tab <- f(t(tab), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.

  new.col <- rownames(tab)
  new.col <- gsub("^ro$", "$R_0$", new.col)
  new.col <- gsub("^h$", "Steepness ($h$)", new.col)
  new.col <- gsub("^m$", "$M$", new.col)
  new.col <- gsub("^rbar$", "$\\\\overline{R}$", new.col)
  new.col <- gsub("^rinit$", "$\\\\overline{R}_{init}$", new.col)
  new.col <- gsub("^vartheta$", "$\\\\vartheta$", new.col)
  new.col <- gsub("^sbo$", "$B_0$", new.col)
  new.col <- gsub("^q([0-9]+)$", "$q_\\1$", new.col)

  col.names <- colnames(tab)
  col.names <- latex.bold(latex.perc(col.names))
  col.names <- c(latex.bold("Parameter"), col.names)

  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

  knitr::kable(tab,
    caption = caption,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE)
}

make.ref.points.table <- function(model,
  digits = 3,
  caption = "default"
  ){
  ## Returns an xtable in the proper format for reference points
  ##
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  tab <- model$mcmccalcs$r.quants
  tab[,-1] <- f(tab[,-1], digits)

  knitr::kable(tab,
    caption = caption,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", row.names = FALSE, escape = FALSE)
}

make.value.table <- function(model,
  type,
  digits = 3,
  caption = "default"
  ){
  ## Returns an xtable in the proper format for values (biomasas, recr, etc)
  ##
  ## out.dat - one of the quants objects as output by the calc.mcmc function
  ## type - 1=biomass, 2=recruitment, 3=F, 4=U, 5=depletion
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  col.names <- colnames(tab)
  col.names[1] <- "Year"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  knitr::kable(tab,
    caption = caption,
    longtable = TRUE,
    align = get.align(ncol(tab))[-1],
    booktabs = TRUE, linesep = "", escape = FALSE, row.names = FALSE) %>%
    kableExtra::kable_styling(latex_options = c("hold_position", "repeat_header"))
}
