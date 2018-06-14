# pcod2018
Pacific Cod assessment for 2018

____
# csas-latex

**Updated July 7, 2017**

Latex/knitr document building for CSAS Research Documents. The Arrowtooth
Flounder assessment is used as an example. To get the model output
directories needed to make it work (they are too large to add to the
repository), please contact me.

You will also need to fork and clone the repository at:

https://github.com/cgrandin/csas-style

It contains the style file which is used for these assessments. Make sure
that csas-style and csas-latex directories are in the same subdirectory
and the code will work as-is. If you want to put it somewhere else,
you'll need to change the following line in the **res-doc.rnw** file to
reflect the location:

```latex
\usepackage{../../csas-style/res-doc}
```
_____________________________________________________________

## Prerequisites
* Windows only - **MikTex**. The first time you try to run, many packages will
  be installed automatically. After that, the document will be built more
  quickly.
* MAC - **LaTeX**
* R
* R Packages:
    * coda
    * knitr
    * lubridate
    * PBSmodelling
    * xtable

* Rscript.exe must be on your PATH if you want to use
  **Method 1 for building the document** (explained below).

---
## How to create the RData files required for the document to build

* Place all model directories in the **models** directory. The base model must
  have an **mcmc** subdirectory; its main directory holds the MPD run and the
  mcmc subdirectory holds the mcmc run for the same model. The model directory
  can contain models which aren't used in the assessment, as the ones used are
  set in the **model-setup.r** file.

* Navigate to the **doc/r** directory and setup the base model and the
  sensitivity model groups by editing the **model-setup.r** file.

* To build the RData files for the the base model and the bridge/sensitivity
  models, open R in the **doc/r** directory and run the following:
  ```R
    source("all.r")
    build()
  ```

  * Once finished, you can see that each model defined in **model-setup.r**
    now has an RData file inside its directory with the same name.

  * To delete all existing Rdata files and rebuild them again from the model
    outputs, open R in the **doc/r** directory and run the following::
    ```R
      source("all.r")
      delete.rdata.files()
      build()
    ```

  * You don't need to save the workspace when you close R, it isn't used by
    latex/knitr.

## How to create resDoc.pdf

* **The RData files must have been created using the method above before
    the document can be built.**

* **The resDoc.pdf file must not be open in Acrobat or the build will fail.**

* **Method 1 for building the document** (Without RGui or Rstudio):
  This method is simpler to run, and all logs are recorded into logfiles
  which can be viewed and searched when errors occur.

  * Navigate to the **doc** subdirectory and run the **build-pdf.bat** file.
  * To see the output from the knitr part of the process, look at the file
    **knitrOutput.log**.
  * To see the output from the Latex part of the process, look at the file
    **latexOutput.log**.
  * If the compilation seems to hang, check the two log files to see where
    it stopped.

* **Method 2 for building the document** (With Rgui or Rstudio):
  This method is faster after the first time, because the models will already
  be loaded into the workspace and won't be reloaded every time you build
  the document.

  * Open R in the **doc/r** directory and run the following:
    ```R
      source("all.r")
      setwd("..")
      build.doc()
    ```

  * After the first time you do this, the models will be loaded into the R
    workspace. You can then edit the various **RNW** files and set the
    first knitr code chunk up so that it doesn't load the models every
    time you build the document. The value in the if statement should be
    changed to **FALSE**.

    ```R
      if(TRUE){
        load.models.into.parent.env()
      }
    ```
* To clean up the build, including removal of the cached figures and tables,
  run the **fresh.bat** batch file, or manually delete the **knitr-cache**
  directory. If you don't do this, figures and tables built previously
  will be used. To keep the cached figures and tables, but remove all other
  traces of the build including the PDF, run **clean.bat**. Note that the
  PDF must not be open in Acrobat or it will not be removed.

## How to debug functions used in the knitr chunks in the **.rnw** files

* Open R in the **doc/r** directory and use this one-liner so that you can
  use the R history (up and down-arrows) This is important while debugging
  the R code, because you will need to run this each time you make a change
  in the code and want to test it, or if you insert a **browser()** command
  somewhere:
  ```R
	source("all.r");load.models.into.parent.env();source("custom-knitr-variables.r")
  ```
* Cut-and-paste the figure/table code from the knitr chunk you want to debug
  into R and the output will be exactly what will appear in the document.

## Installation of R packages

* The code will automatically install packages from CRAN or GitHub - see
  the **install.packages.if.needed()** function in
  **doc/r/r-functions/utilities.r** and the calls to it in **all.r**.

---

## How the R environment is set up

* When the document is built, all of the model RData files which were previously
  built are loaded into the workspace that is seen by **knitr**. All the
  lengthy R processes are done ahead of time from the **build()** function to
  make the document building quicker, since RData files are loaded instead of
  raw model output.

The following depicts the object structure of each model's .RData file:

```R
model                 # A list created by the build() function
model$path            # The relative (to doc/r) path where this model is located
model$dat.file        # The relative (to doc/r) path with data file name for this model
model$ctl.file        # The relative (to doc/r) path with control file name for this model
model$proj.file       # The relative (to doc/r) path with projection file name for this model
model$dat             # data file contents (see r-functions/load-models.r)
model$ctl             # control file contents (see r-functions/load-models.r)
model$proj            # projection file contents (see r-functions/load-models.r)
model$par             # par file contents (see r-functions/load-models.r)
model$mpd             # MPD output (see r-functions/load-models.r)
model$mcmc            # MCMC output from the model (see r-functions/load-models.r)
model$mcmcpath        # The relative (to doc/r) path where the MCMC model is located
model$mcmccalcs       # MCMC output (burned in) and credibility intervals for all parameters
model$mcmccalcs$
  p.dat               # leading parameter estimates (burned in and thinned)
  p.quants            # leading parameter estimates (median and CI's)
  p.dat.log           # log of leading parameter estimates (burned in and thinned)
  p.quants            # log of leading parameter estimates (median and CI's)
  r.dat               # calculated parameters (burned in and thinned)
  r.quants            # selected calculated parameters (median and CI's, as latex table)
  sbt.dat             # spawning biomass (burned in and thinned)
  sbt.quants          # spawning biomass (median and CI's, incl. MPD)
  depl.dat            # relative spawning biomass (burned in and thinned)
  depl.quants         # relative spawning biomass (median and CI's, incl. MPD)
  recr.dat            # recruitment (burned in and thinned)
  recr.quants         # recruitment (median and CI's)
  recr.devs.dat       # recruitment deviations (burned in and thinned)
  recr.devs.quants    # recruitment deviations (median and CI's)
  q.dat               # catchability (burned in and thinned)
  q.quants            # catchability (median and CI's)
  vuln.dat            # vulnerable biomass (burned in and thinned, list of gears)
  vuln.quants         # vulnerable biomass (median and CI's, list of gears, incl. MPD)
  f.mort.dat          # fishing mortality (burned in and thinned, list of gears, incl. MPD)
  f.mort.quants       # fishing mortality (median and CI's, list of gears, incl. MPD)
  u.mort.dat          # annual fishing mortality (burned in and thinned, list of gears, incl. MPD)
  u.mort.quants       # annual fishing mortality (median and CI's, list of gears, incl. MPD)
  proj.dat            # reference points projection table (latex style)
```

These are some of the other variables in the global workspace. These can be
directly referenced using \Sexpr{} in inline latex code, or in a knitr code
chunk. This list is just a sample, there are many more in the
**custom-knitr-variables.r** file, which is where any new ones should be placed.

```R
b                       # The base model object.
b.mcc                   # An alias for b$mcmccalcs
b.p.quants              # An alias for b$mcmccalcs$p.quants
b.r.quants              # An alias for b$mcmccalcs$r.quants
fish.name               # Name of the species in the assessment
science.name            # Scientific name of the species
family.name             # Scientific family name
common.name             # Colloquial name for the species
bo                      # Inital biomass (vector of 3 - median and ci's)
bmsy                    # Bmsy (vector of 3 - median and ci's)
fmsy                    # Fmsy (vector of 3 - median and ci's)
sbt                     # Spawning biomass - alias for b$mcmccalcs$sbt.quants
sbt.final               # Spawning biomass for the final year (vector of 4 - median, ci's, MPD)
sbt.final.yr            # Last year of estimated biomass
sbt.first.yr            # First year of estimated biomass

h.prior                 # Setup as seen in the control file for steepness
h.prior.alpha           # Alpha parameter for steepness
h.prior.beta            # Beta parameter for steepness
h.prior.mean            # Calculated mean for steepness prior
h.prior.cv              # Calculated CV for steepness prior

q                       # Initial value of survey q

trawl.yrs               # Range of years for which there are age comp data for trawl fishery

catch.yrs               # Range of years of catch

rho                     # Initial value of the rho parameter
vartheta                # Initial value of the vartheta parameter
tau                     # Calculated value for tau
sigma                   # Calculated value for sigma

f                       # The function used to format numbers - this is used around all numbers in the doc
```
