## -----------------------------------------------------------------------------
## Set verbosity for this project (R code)
## -----------------------------------------------------------------------------
verbose <- TRUE

## Custom class types
model.class <- "model"
model.lst.class <- "model.list"

## Values to use in the mcmc calculations along with the median
confidence.vals <- c(0.05, 0.95)

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
last.assess.yr <- 2013
if(verbose) cat0("Last assessment year: \n  ", last.assess.yr)

## -----------------------------------------------------------------------------
## Directory in which the model directories reside
## rootd is defined in all.r
## -----------------------------------------------------------------------------
model.dir <- file.path(rootd, "models")
if(verbose) cat0("Models directory: \n  ", model.dir)

## -----------------------------------------------------------------------------
## Directory in which the retrospective model directories reside
## -----------------------------------------------------------------------------
retro.dir <- file.path(model.dir, "0_1_5CD_2018_Base", "retrospectives")
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
start.yr <- 1956
if(verbose){
  cat0("Start year for catch data: \n  ", start.yr)
}

## Start year for the fishery age comps
## start.yr.age.comps <- 1951
## if(verbose){
##   cat0("Start year for fishery age comps data: \n  ", start.yr.age.comps)
## }

## The last non-forecast year in the model. This is the year for which the
## mcmc outputs will be used in reference point calculations.
end.yr <- 2017
if(verbose){
  cat0("End year for model: \n  ", end.yr)
}
## Final year of data (This is what is the end year is in the model data files)
last.data.yr <- 2016
if(verbose){
  cat0("Last year of model data: \n  ", last.data.yr)
}

## -----------------------------------------------------------------------------
## Base model names and directories
## -----------------------------------------------------------------------------
base.model.name <- "Reference model"
base.model.dir.name <- "0_1_5CD_2018_Base"

verify.models(model.dir, base.model.dir.name, base.model.name)

if(verbose){
  cat0("Base model directory name for reference model:\n", base.model.dir.name)
  cat0("Base model pretty name for reference model:\n", base.model.name)
}

## -----------------------------------------------------------------------------
## Sensitivity models group 1
## -----------------------------------------------------------------------------
sens.models.dir.name.1 <- c("0_2_5CD_2018_sig02",
                            "0_2a_5CD_2018_sig015",
                            "0_2b_5CD_2018_sig01")
sens.models.name.1 <- c("Sigma = 0.2",
                        "Sigma = 0.15",
                        "Sigma = 0.1")
verify.models(model.dir,
              sens.models.dir.name.1,
              sens.models.name.1)

if(verbose){
  print.model.message(sens.models.dir.name.1,
                      sens.models.name.1,
                      1,
                      model.type = "Sensitivity")
}

## -----------------------------------------------------------------------------
## Retrospectives
## -----------------------------------------------------------------------------
retro.dir.names <- c("Retrospective01",
                     "Retrospective02",
                     "Retrospective03",
                     "Retrospective04")
retro.names <- c("- 1 year",
                 "- 2 years",
                 "- 3 years",
                 "- 4 years")

## This function must be called from within the first knitr code chunk
## in the document. It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model.dir.names above..
load.models.into.parent.env <- function(){
  base.model <<- load.models(model.dir, base.model.dir.name)
  sens.models.1 <<- load.models(model.dir, sens.models.dir.name.1)

  base.retro.models <<- load.models(retro.dir, retro.dir.names)

}

build <- function(ovwrt.base = FALSE,
                  ovwrt.sens = FALSE,
                  ovwrt.retro = FALSE,
                  burnin = 1000,
                  thin = 1){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files. Each model defined in the models-setup.r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme.md file.
  ##
  ## ovwrt.base - overwrite the RData file for the base model?
  ## ovwrt.sens - overwrite the RData files for the sensitivity models?
  ## ovwrt.retro - overwrite the RData files for the retrospective models?

  ## Base model
  create.rdata.file(model.name = base.model.dir.name,
                    ovwrt.rdata = ovwrt.base,
                    load.proj = TRUE,
                    burnin = burnin,
                    thin = thin,
                    low = confidence.vals[1],
                    high = confidence.vals[2],
                    verbose = ss.verbose)

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the lapply below to work right
  sens.models.names.list <- c(unlist(sens.models.dir.name.1))
                             ## unlist(sens.model.dir.name.2),
                             ## unlist(sens.model.dir.name.3),
                             ## unlist(sens.model.dir.name.4),
                             ## unlist(sens.model.dir.name.5),
                             ## unlist(sens.model.dir.name.6),
                             ## unlist(sens.model.dir.name.7),
                             ## unlist(sens.model.dir.name.8))
  ## Sensitivity models
  for(model.nm in sens.models.names.list){
    create.rdata.file(
      model.name = model.nm,
      ovwrt.rdata = ovwrt.sens,
      load.proj = TRUE,
      burnin = burnin,
      thin = thin,
      low = confidence.vals[1],
      high = confidence.vals[2],
      verbose = verbose)
  }

  ## Retrospective models
  for(model.nm in retro.dir.names){
    if(dir.exists(retro.dir)){
      if(dir.exists(file.path(retro.dir, model.nm))){
        create.rdata.file(
          models.dir = retro.dir,
          model.name = model.nm,
          ovwrt.rdata = ovwrt.retro,
          load.proj = TRUE,
          burnin = burnin,
          thin = thin,
          low = confidence.vals[1],
          high = confidence.vals[2],
          verbose = verbose)
      }
    }
  }
}
