## -----------------------------------------------------------------------------
## Set verbosity for this project (R code) and SS shell calls
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.05, 0.95)

## Fixed cutoffs for decision tables, corresponsing to the stock order
## HG, PRD, CC, SOG, WCVI
fixed.cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)

## -----------------------------------------------------------------------------
## iscam files with names that don't change depending on model
rep.file <- "iscam.rep"
par.file <- "iscam.par"
mcmc.file <- "iscam_mcmc.csv"
mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
mcmc.recr.file <- "iscam_rt_mcmc.csv"
mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
mcmc.natural.mort.file <- "iscam_m_mcmc.csv"
mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
mpd.proj.file <- "iscammpd_proj_Gear1.csv"
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Year for this assessment - default is current year
## -----------------------------------------------------------------------------
assess.yr <- as.numeric(substr(Sys.Date(), 1, 4))
if(verbose) cat0("Assessment year: \n  ", assess.yr)

## -----------------------------------------------------------------------------
## Year for last assessment - default is current year - 1
## -----------------------------------------------------------------------------
last.assess.yr <- assess.yr - 1
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## -----------------------------------------------------------------------------
model.dir <- file.path("..", "..", "models")
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## Directory in which the retrospective model directories reside
## -----------------------------------------------------------------------------
retro.dir <- file.path("..", "..", "retrospectives")
if(verbose) cat0("Retrospectives directory: \n  ", retro.dir)

## -----------------------------------------------------------------------------
## File names which must exists in each model directory
## -----------------------------------------------------------------------------
exe.file.name <- "iscam.exe"
if(verbose) cat0("iscam executable file: \n  ", exe.file.name)
starter.file.name <- "iscam.dat"
if(verbose) cat0("iscam starter file: \n  ", starter.file.name)

## -----------------------------------------------------------------------------
## Data start and endpoint variables
## -----------------------------------------------------------------------------
## Start year for the models
start.yr <- 1951
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}
## Start year for the fishery age comps
start.yr.age.comps <- 1951
if(verbose){
  cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
}
## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2016
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2016
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Directories and names of stocks
## -----------------------------------------------------------------------------
stock.dir <- list()
stock.name <- list()
stock.dir[[1]] <- "HG"
stock.name[[1]] <- "Haida Gwaii"
stock.dir[[2]] <- "PRD"
stock.name[[2]] <- "Pr. Rupert"
stock.dir[[3]] <- "CC"
stock.name[[3]] <- "Central Coast"
stock.dir[[4]] <- "SOG"
stock.name[[4]] <- "SOG"
stock.dir[[5]] <- "WCVI"
stock.name[[5]] <- "WCVI"

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
base.model.name <- lapply(1:length(stock.name),
                          function(x){
                            paste("Reference model", stock.name[[x]])})

base.model.dir.name <- lapply(1:length(stock.dir),
                              function(x){
                                file.path(stock.dir[[x]], "AM2")})

lapply(1:length(base.model.dir.name),
       function(x){
         verify.models(model.dir,
                       base.model.dir.name[[x]],
                       base.model.name[[x]])})

if(verbose){
  lapply(1:length(base.model.dir.name),
         function(x){
           cat0("Base model directory name for ",
                stock.name[[x]],
                ":\n  ",
                base.model.dir.name[[x]])
           cat0("Base model pretty name for ",
                stock.name[[x]],
                ":\n  ",
                base.model.name[[x]])})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## This is a list of the AM1 models, one for each stock
## -----------------------------------------------------------------------------
sens.model.dir.name.1 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1")})

sens.model.name.1 <- "AM1"

lapply(1:length(sens.model.dir.name.1),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.1[[x]],
                       sens.model.name.1)})

if(verbose){
  lapply(1:length(sens.model.dir.name.1),
         function(x){
           print.model.message(sens.model.dir.name.1[[x]],
                               sens.model.name.1,
                               1,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## This is a list of the constant natural mortality sensitivities for AM1
## -----------------------------------------------------------------------------
## Redefine stock.dir so that sensitivities can be loaded
stock.dir <- list()
stock.dir[[1]] <- "HG-natural-mortality"
stock.dir[[2]] <- "PRD-natural-mortality"
stock.dir[[3]] <- "CC-natural-mortality"
stock.dir[[4]] <- "SOG-natural-mortality"
stock.dir[[5]] <- "WCVI-natural-mortality"
sens.model.dir.name.2 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1_constantM")})

sens.model.name.2 <- "AM1 constant M"

lapply(1:length(sens.model.dir.name.2),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.2[[x]],
                       sens.model.name.2)})

if(verbose){
  lapply(1:length(sens.model.dir.name.2),
         function(x){
           print.model.message(sens.model.dir.name.2[[x]],
                               sens.model.name.2,
                               2,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## This is a list of the time varying natural mortality sensitivities for AM1
## -----------------------------------------------------------------------------
sens.model.dir.name.3 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1_timevaryingM")})

sens.model.name.3 <- "AM1 time-varying M"

lapply(1:length(sens.model.dir.name.3),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.3[[x]],
                       sens.model.name.3)})

if(verbose){
  lapply(1:length(sens.model.dir.name.3),
         function(x){
           print.model.message(sens.model.dir.name.3[[x]],
                               sens.model.name.3,
                               3,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## This is a list of the constant natural mortality sensitivities for AM2
## -----------------------------------------------------------------------------
sens.model.dir.name.4 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM2_constantM")})

sens.model.name.4 <- "AM2 constant M"

lapply(1:length(sens.model.dir.name.4),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.4[[x]],
                       sens.model.name.4)})

if(verbose){
  lapply(1:length(sens.model.dir.name.4),
         function(x){
           print.model.message(sens.model.dir.name.4[[x]],
                               sens.model.name.4,
                               4,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 5
## This is a list of the time varying natural mortality sensitivities for AM2
## -----------------------------------------------------------------------------
sens.model.dir.name.5 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM2_timevaryingM")})

sens.model.name.5 <- "AM2 time-varying M"

lapply(1:length(sens.model.dir.name.5),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.5[[x]],
                       sens.model.name.5)})

if(verbose){
  lapply(1:length(sens.model.dir.name.5),
         function(x){
           print.model.message(sens.model.dir.name.5[[x]],
                               sens.model.name.5,
                               5,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 6
## This is a list of the q priors "a" sensitivities for AM1
## -----------------------------------------------------------------------------
## Redefine stock.dir so that sensitivities can be loaded
stock.dir <- list()
stock.dir[[1]] <- "HG-q-priors"
stock.dir[[2]] <- "PRD-q-priors"
stock.dir[[3]] <- "CC-q-priors"
stock.dir[[4]] <- "SOG-q-priors"
stock.dir[[5]] <- "WCVI-q-priors"
sens.model.dir.name.6 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1qa")})

sens.model.name.6 <- "AM1 q-a"

lapply(1:length(sens.model.dir.name.6),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.6[[x]],
                       sens.model.name.6)})

if(verbose){
  lapply(1:length(sens.model.dir.name.6),
         function(x){
           print.model.message(sens.model.dir.name.6[[x]],
                               sens.model.name.6,
                               6,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 7
## This is a list of the q priors "b" sensitivities for AM1
## -----------------------------------------------------------------------------
sens.model.dir.name.7 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1qb")})

sens.model.name.7 <- "AM1 q-b"

lapply(1:length(sens.model.dir.name.7),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.7[[x]],
                       sens.model.name.7)})

if(verbose){
  lapply(1:length(sens.model.dir.name.7),
         function(x){
           print.model.message(sens.model.dir.name.7[[x]],
                               sens.model.name.7,
                               7,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Sensitivity models group 8
## This is a list of the q priors "c" sensitivities for AM1
## -----------------------------------------------------------------------------
sens.model.dir.name.8 <- lapply(1:length(stock.dir),
                                function(x){
                                  file.path(stock.dir[[x]], "AM1qc")})

sens.model.name.8 <- "AM1 q-c"

lapply(1:length(sens.model.dir.name.8),
       function(x){
         verify.models(model.dir,
                       sens.model.dir.name.8[[x]],
                       sens.model.name.8)})

if(verbose){
  lapply(1:length(sens.model.dir.name.8),
         function(x){
           print.model.message(sens.model.dir.name.8[[x]],
                               sens.model.name.8,
                               8,
                               model.type = "Sensitivity")})
}

## -----------------------------------------------------------------------------
## Retrospectives
## -----------------------------------------------------------------------------
RETRO.STRING <- c(paste0("0", 1:9), 10:15)
stock.dir[[1]] <- "HG"
stock.dir[[2]] <- "PRD"
stock.dir[[3]] <- "CC"
stock.dir[[4]] <- "SOG"
stock.dir[[5]] <- "WCVI"
retro.names.am1 <- list()
retro.names.am2 <- list()
retro.yr <- list()
for(i in 1:length(stock.dir)){
  retro.names.am1[[i]] <- list()
  retro.names.am2[[i]] <- list()
  for(j in 1:length(RETRO.STRING)){
    if(stock.dir[i] == "HG"){
      which.stock <- 1
    }else if(stock.dir[i] == "PRD"){
      which.stock <- 2
    }else if(stock.dir[i] == "CC"){
      which.stock <- 3
    }else if(stock.dir[i] == "SOG"){
      which.stock <- 4
    }else if(stock.dir[i] == "WCVI"){
      which.stock <- 5
    }else{
      stop("The retrospectives directory does not contain the known stock names.")
    }
    ## Create lists of the names of the retrospectives for AM1 and AM2
    retro.names.am1[[i]][[j]] <- file.path(stock.dir[i],
                                           RETRO.STRING[j],
                                           stock.dir[i],
                                           "AM1")
    retro.names.am2[[i]][[j]] <- file.path(stock.dir[i],
                                           RETRO.STRING[j],
                                           stock.dir[i],
                                           "AM2")
  }
}

## -----------------------------------------------------------------------------
## Vector of directory names for all models referenced above
## -----------------------------------------------------------------------------
## ALL models must be in this list!
## Each model directory listed here will have an RData file in it,
##  or one will be created depending on what is found in the directory.
##  i.e. mcmc, retrospective, or forecast directories.
model.dir.names <- c(base.model.dir.name,
                     unlist(sens.model.dir.name.1),
                     unlist(sens.model.dir.name.2),
                     unlist(sens.model.dir.name.3),
                     unlist(sens.model.dir.name.4),
                     unlist(sens.model.dir.name.5),
                     unlist(sens.model.dir.name.6),
                     unlist(sens.model.dir.name.7),
                     unlist(sens.model.dir.name.8))

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.models <<- lapply(base.model.dir.name,
                         function(x){
                           load.models(model.dir, x)})
  sens.models.1 <<- lapply(sens.model.dir.name.1,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.2 <<- lapply(sens.model.dir.name.2,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.3 <<- lapply(sens.model.dir.name.3,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.4 <<- lapply(sens.model.dir.name.4,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.5 <<- lapply(sens.model.dir.name.5,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.6 <<- lapply(sens.model.dir.name.6,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.7 <<- lapply(sens.model.dir.name.7,
                           function(x){
                             load.models(model.dir, x)})
  sens.models.8 <<- lapply(sens.model.dir.name.8,
                           function(x){
                             load.models(model.dir, x)})

  base.retro.models <<- lapply(retro.names.am2,
                               function(x){
                                 lapply(x,
                                        function(y){
                                          tmp <- load.models(retro.dir, y)
                                        })})

  am1.retro.models <<- lapply(retro.names.am1,
                              function(x){
                                lapply(x,
                                       function(y){
                                         tmp <- load.models(retro.dir, y)
                                       })})

  ## Remove NULL entries in retrospective lists. This allows for
  ##  each stock to have a different length retrospective period
  ## A helper function that tests whether an object is either NULL
  ##  or a list of NULLs
  is.null.elem <- function(x){
    is.null(x) | all(sapply(x, is.null))
  }

  ## Recursively step down into list, removing all such objects
  remove.null.elems <- function(x){
    x <- Filter(Negate(is.null.elem), x)
    lapply(x,
           function(x){
             if(is.list(x)){
               remove.null.elems(x)
             }else{
               x
             }})
  }

  base.retro.models <<- remove.null.elems(base.retro.models)
  am1.retro.models <<- remove.null.elems(am1.retro.models)

}

build <- function(ovwrt.base = FALSE,
                  ovwrt.sens = FALSE,
                  ovwrt.retro = FALSE){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.
  ##
  ## ovwrt.base - overwrite the RData file for the base model?
  ## ovwrt.sens - overwrite the RData files for the sensitivity models?
  ## ovwrt.retro - overwrite the RData files for the retrospective runs?

  ## Base models
  invisible(lapply(1:length(base.model.dir.name),
                   function(x){
                     ## Determine which stock is being loaded
                     bm <- base.model.dir.name[[x]]
                     if(length(grep("HG", bm))){
                       which.stock <- 1
                     }else if(length(grep("PRD", bm))){
                       which.stock <- 2
                     }else if(length(grep("CC", bm))){
                       which.stock <- 3
                     }else if(length(grep("SOG", bm))){
                       which.stock <- 4
                     }else if(length(grep("WCVI", bm))){
                       which.stock <- 5
                     }else{
                       which.stock <- 0
                     }
                     create.rdata.file(model.name = bm,
                                       ovwrt.rdata = ovwrt.base,
                                       load.proj = TRUE,
                                       low = confidence.vals[1],
                                       high = confidence.vals[2],
                                       burnin = 0,
                                       which.stock = which.stock,
                                       which.model = 2,
                                       fixed.cutoffs = fixed.cutoffs,
                                       verbose = ss.verbose)}))

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the lapply below to work right
  sens.model.names.list <- c(unlist(sens.model.dir.name.1),
                             unlist(sens.model.dir.name.2),
                             unlist(sens.model.dir.name.3),
                             unlist(sens.model.dir.name.4),
                             unlist(sens.model.dir.name.5),
                             unlist(sens.model.dir.name.6),
                             unlist(sens.model.dir.name.7),
                             unlist(sens.model.dir.name.8))

  ## Sensitivity models
  invisible(lapply(1:length(sens.model.names.list),
                   function(x){
                     ## Determine which stock is being loaded
                     sm <- sens.model.names.list[[x]]
                     if(length(grep("HG", sm))){
                       which.stock <- 1
                     }else if(length(grep("PRD", sm))){
                       which.stock <- 2
                     }else if(length(grep("CC", sm))){
                       which.stock <- 3
                     }else if(length(grep("SOG", sm))){
                       which.stock <- 4
                     }else if(length(grep("WCVI", sm))){
                       which.stock <- 5
                     }else{
                       stop("The directory ", sm, " does not contain the known stock names.")
                     }
                     if(length(grep("AM1", sm))){
                       which.model = 1
                     }else if(length(grep("AM2", sm))){
                       which.model = 2
                     }else{
                       stop("The directory ", sm, " does not contain the known model names AM1 or AM2.")
                     }
                     create.rdata.file(
                       model.name = sm,
                       ovwrt.rdata = ovwrt.sens,
                       load.proj = TRUE,
                       low = confidence.vals[1],
                       high = confidence.vals[2],
                       burnin = 0,
                       which.stock = which.stock,
                       which.model = which.model,
                       fixed.cutoffs = fixed.cutoffs,
                       verbose = ss.verbose)}))

  ## Retrospective models
  stock.dir[[1]] <- "HG"
  stock.dir[[2]] <- "PRD"
  stock.dir[[3]] <- "CC"
  stock.dir[[4]] <- "SOG"
  stock.dir[[5]] <- "WCVI"
  retro.dirs.am1 <- list()
  retro.dirs.am2 <- list()
  retro.yr <- list()
  for(i in 1:length(stock.dir)){
    retro.dirs.am1[[i]] <- list()
    retro.dirs.am2[[i]] <- list()
    for(j in 1:length(RETRO.STRING)){
      if(stock.dir[i] == "HG"){
        which.stock <- 1
      }else if(stock.dir[i] == "PRD"){
        which.stock <- 2
      }else if(stock.dir[i] == "CC"){
        which.stock <- 3
      }else if(stock.dir[i] == "SOG"){
        which.stock <- 4
      }else if(stock.dir[i] == "WCVI"){
        which.stock <- 5
      }else{
        stop("The retrospectives directory does not contain the known stock names.")
      }
      create.rdata.file.retro(
        model.dir = file.path(retro.dir,
                              stock.dir[i],
                              RETRO.STRING[j],
                              stock.dir[i],
                              "AM1"),
        ovwrt.rdata = ovwrt.retro,
        load.proj = TRUE,
        low = confidence.vals[1],
        high = confidence.vals[2],
        burnin = 0,
        which.stock = which.stock,
        which.model = 1)
      create.rdata.file.retro(
        model.dir = file.path(retro.dir,
                              stock.dir[i],
                              RETRO.STRING[j],
                              stock.dir[i],
                              "AM2"),
        ovwrt.rdata = ovwrt.retro,
        load.proj = TRUE,
        low = confidence.vals[1],
        high = confidence.vals[2],
        burnin = 0,
        which.stock = which.stock,
        which.model = 2)
    }
  }
}
