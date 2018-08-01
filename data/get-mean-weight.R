#Code by Chris Grandin, Sean Anderson and Robyn Forrest.

#Modified by Robyn Forrest July 16 2018
graphics.off()
rm(list=ls(all=TRUE))
library(gfplot)
library(tidyverse)
library(readr)
library(lubridate)
library(psych)
library(reshape2)

#Length-weight parameters
#coastwide
.ALPHA <- 6.79e-06
.BETA <- 3.11

#3CD
.ALPHA3 <- 7.43e-06
.BETA3 <- 3.09

#5ABCD
.ALPHA5 <- 6.52e-06
.BETA5 <- 3.12

#old (2013 assessment)
.ALPHA2013 <- 7.377e-06
.BETA2013 <- 3.0963

outDir <- file.path(getwd(), "results")

prevMeanWeight <- read_csv("MeanWeights_previous.csv")

source("get-data.R")

if(FALSE){
  extract.data(species = "pacific cod",
               cache.dir = "pcod-cache",
               end.year = 2017,
               unsorted.only = FALSE)
}

dat <- load.data(cache.dir = "pcod-cache")
d <- dat$commercial_samples

areas <- c("3[CD]+", "5[AB]+", "5[CD]+")
d$area <- NA
for (i in seq_along(areas)) {
  d[grepl(areas[[i]], d$major_stat_area_name), "area"] <-
    gsub("\\[|\\]|\\+", "", areas[[i]])
}

################################################################
#Get mean weight
#5AB
df5AB <- filter(d,area=="5AB") %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA5,
                  b = .BETA5)

write_csv(df5AB,file.path(outDir,"AnnualMeanWeight_5AB.csv"))

#5AB - OLD LW pars
df5ABoldpars <- filter(d,area=="5AB") %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA2013,
                  b = .BETA2013)

#5CD
df5CD <- filter(d,area=="5CD") %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA5,
                  b = .BETA5)
write_csv(df5CD,file.path(outDir,"AnnualMeanWeight_5CD.csv"))

#5CD - OLD LW pars
df5CDoldpars <- filter(d,area=="5CD") %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA2013,
                  b = .BETA2013)

#3CD
df3CD <- filter(d,area=="3CD") %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA3,
                  b = .BETA3)
write_csv(df3CD,file.path(outDir,"AnnualMeanWeight_3CD.csv"))

#5ABCD
df5ABCD <- filter(d,is.element(area,c("5AB", "5CD"))) %>%
  get.mean.weight(dat$catch,
                  areas=NULL,
                  a = .ALPHA5,
                  b = .BETA5)
write_csv(df5ABCD,file.path(outDir,"AnnualMeanWeight_5ABCD.csv"))

#################################################################
#Plot results
#5AB
df <- df5AB
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5AB")
ggsave(file.path(outDir,"AnnualMeanWeight_5AB.png"), width=8, height=6, units="in")

#5CD
df <- df5CD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5CD")
ggsave(file.path(outDir,"AnnualMeanWeight_5CD.png"), width=8, height=6, units="in")

#3CD
df <- df3CD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 3CD")
ggsave(file.path(outDir,"AnnualMeanWeight_3CD.png"), width=8, height=6, units="in")

#5ABCD
df <- df5ABCD
ggplot(data=df, aes(x=year,y=mean_weight, group=1)) +
  geom_line(lwd=1, colour=2) +
  ylim(0,1.1*max(df$mean_weight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$year),max(df$year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5ABCD")
ggsave(file.path(outDir,"AnnualMeanWeight_5ABCD.png"), width=8, height=6, units="in")

#########################################################################################
#Compare to previous analyses
#5AB
Analysis <- "2018 new LW pars"
df <- df5AB
df <- cbind(df,rep(Analysis,nrow(df)))
colnames(df) <- c("Year","MeanWeight","Analysis")
Analysis <- "2018 old LW pars"
df2 <- df5ABoldpars
df2 <- cbind(df2,rep(Analysis,nrow(df2)))
colnames(df2) <- c("Year","MeanWeight","Analysis")

dfcompare <- subset(prevMeanWeight, Area=="5AB", select=-Area)
dfcompare <- rbind(dfcompare,df,df2)
ggplot(data=dfcompare, aes(x=Year,y=MeanWeight, group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  ylim(0,1.1*max(dfcompare$MeanWeight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5AB")
ggsave(file.path(outDir,"AnnualMeanWeightCompare_5AB.png"), width=8, height=6, units="in")

#5CD
Analysis <- "2018 new LW pars"
df <- df5CD
df <- cbind(df,rep(Analysis,nrow(df)))
colnames(df) <- c("Year","MeanWeight","Analysis")
Analysis <- "2018 old LW pars"
df2 <- df5CDoldpars
df2 <- cbind(df2,rep(Analysis,nrow(df2)))
colnames(df2) <- c("Year","MeanWeight","Analysis")

dfcompare <- subset(prevMeanWeight, Area=="5CD", select=-Area)
dfcompare <- rbind(dfcompare,df,df2)
ggplot(data=dfcompare, aes(x=Year,y=MeanWeight, group=Analysis, colour=Analysis)) +
  geom_line(lwd=1) +
  ylim(0,1.1*max(dfcompare$MeanWeight)) +
  theme(plot.title=element_text(size=14,face="bold",hjust=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_continuous(breaks=seq(min(df$Year),max(df$Year),by=5)) +
  labs(x= "Fishing Year", y = "Annual Mean Weight (Kg)", title="Area 5CD")
ggsave(file.path(outDir,"AnnualMeanWeightCompare_5CD.png"), width=8, height=6, units="in")
