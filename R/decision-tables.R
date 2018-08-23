#**********************************************************************************
#This code creates a model-averaged decision table based on X scenarios. 
#It reads in the iscammcmc_proj_Gear1.csv file from each of the scenarios to be used in the decision table

#This table calculates probabilities of performance measures based on the combined posterior samples of performance measure quantities in pbs_pcod2013ddmcmc.proj
#	 For example: A performance measure is B2019/0.8Bmsy. 
#  A posterior sample of length 1000 will have 1000 estimates of B2019/0.8Bmsy
#  We are interested in P(B2019 < 0.8Bmsy), i.e. proportion of samples where B2019/0.8Bmsy <1
#	 Therefore calculate the proportion of posterior samples for B2019/0.8Bmsy that are less than 1

#Code makes four decision tables
#"5ABCD_Model_Averaged" :: AreaArea[1] and BaseCaseOnly = FALSE
#"5ABCD_Base_only"      :: AreaArea[1] and BaseCaseOnly = TRUE
#"3CD_Model_Averaged"   :: AreaArea[2] and BaseCaseOnly = FALSE
#"3CD_Base_only"        :: AreaArea[2] and BaseCaseOnly = TRUE

Areas <- c("5ABCD", "3CD")
Burn=1000 #burn in

#Set Area Here   
Area <- Areas[2]
BaseCaseOnly <- TRUE

if(BaseCaseOnly==TRUE) {
  dtname <- paste0(Area,"BaseCaseOnly")
} else dtname <- paste0(Area,"ModelAveraged") 

#set up directories --- need to hardwire scenario choices for model averaging
dir <- here::here()

if(Area == "5ABCD"){
  basedir <- paste0(dir,"/models/0_1a_5ABCD_BASE_loc-yr-interact_rsoleq_06_019-10m/")
  sens1dir <- paste0(dir,"/models/0_1c_5ABCD_NO_loc-yr-interact_rsoleq_06_019_doubleCV/")
  
  #Read in mcmc projection files
  scenarios <- list()
  scenarios[[1]] <- read.csv(paste0(basedir,"iscammcmc_proj_Gear1.csv" ), header=T)
  scenarios[[2]] <-read.csv(paste0(sens1dir,"iscammcmc_proj_Gear1.csv" ), header=T)
  
}
if(Area == "3CD"){
  basedir <- paste0(dir,"/models/1_1a_3CD_BASE_loc-yr-interact_rsoleq_0228sd03-10m/")
  sens1dir <- paste0(dir,"/models/1_1c_3CD_NO_loc-yr-interact_rsoleq_0228sd03_doubleCV/")
  
  #Read in mcmc projection files
  scenarios <- list()
  scenarios[[1]] <- read.csv(paste0(basedir,"iscammcmc_proj_Gear1.csv" ), header=T)
  scenarios[[2]] <-read.csv(paste0(sens1dir,"iscammcmc_proj_Gear1.csv" ), header=T)
  
}


if(BaseCaseOnly==TRUE) {
  nscenarios <- 1
} else nscenarios <- length(scenarios)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Get TACs and set up blank decision table
tac <- unique(scenarios[[1]]$TAC)
decisiontable <- as.data.frame(matrix(NA,ncol=9, nrow=length(tac)))
colnames(decisiontable)<-c("TAC",
                     "P(B2019<B2018)",
                     "P(F2018>F2017)",
                     "P(B2019<0.8BMSY)",
                     "P(B2019<0.4BMSY)",
                     "P(F2018>FMSY)",
                     "P(B2019<Bmin)",
                     "P(B2019<BAvg)",
                     "P(F2018>FAvg)")


#Loop over TAC
#Need to combine the posterior samples for each TAC from all scenarios (if model averaging)

for(i in seq_along(tac)){
  results <- list()
  
  for(j in 1:nscenarios) {
    #get results only for tac[i]
    results[[j]] <- subset(scenarios[[j]], TAC==tac[i])
    
    #remove burn in
    nsamp <- nrow(results[[j]])
    results[[j]] <- results[[j]][(Burn+1):nsamp,]
  }
  
  #if only the base case then just take the first scenario's results for tac[i]. 
  #Otherwise use rbind.fill to rbind all the result matrices for tac[i]
  if(BaseCaseOnly==TRUE) {
    d <- results[[1]]
  } else d <- plyr::rbind.fill(results)
  
  npostsamp <- nrow(d) #this will usually be nscenarios x 1000)
  
  #Now calculate probabilities and fill rows of decision table
  decisiontable[i,1] <- tac[i]  #TAC
  decisiontable[i,2] <- length(which(d$B2019B2018<1))/npostsamp   #P(B2019<B2018)
  decisiontable[i,3] <- length(which(d$F2018F2017>1))/npostsamp   #P(F2018>F2017)
  decisiontable[i,4] <- length(which(d$B20190.8BMSY<1))/npostsamp #P(B2019<0.8BMSY)
  decisiontable[i,5] <- length(which(d$B20190.4BMSY<1))/npostsamp #P(B2019<0.4BMSY)
  decisiontable[i,6] <- length(which(d$F2018FMSY>1))/npostsamp    #P(F2018>FMSY)
  decisiontable[i,7] <- length(which(d$B2019Bmin<1))/npostsamp    #P(B2019<Bmin)
  decisiontable[i,8] <- length(which(d$B2019BAvgS<1))/npostsamp   #P(B2019<BAvg) #Avg 1956-2004
  decisiontable[i,9] <- length(which(d$F2018FAvgS>1))/npostsamp   #P(F2018>FAvg) #Avg 1956-2004

}  

#Write decision table
readr::write_csv(decisiontable,paste0(dir,"/decisiontable",Area,"_", dtname,".csv"))


