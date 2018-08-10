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

source("data/get-data.R")

if(FALSE){
  cache_pbs_data(species = "pacific cod",
                 path = "data/pcod-cache",
                 unsorted_only = FALSE)
}

dat <- load.data(cache.dir = "data/pcod-cache")
d <- dat$commercial_samples

################################################################
## Get mean weight
include.usa <- TRUE
## 5AB
df5AB <- get.mean.weight(d,
                         dat$catch,
                         areas = "5[AB]+",
                         include.usa = include.usa,
                         a = .ALPHA5,
                         b = .BETA5)

write_csv(df5AB,file.path(outDir,"AnnualMeanWeight_5AB.csv"))

## 5AB - OLD LW pars
df5ABoldpars <- get.mean.weight(d,
                                dat$catch,
                                areas = "5[AB]+",
                                include.usa = include.usa,
                                a = .ALPHA2013,
                                b = .BETA2013)

## 5CD
df5CD <- get.mean.weight(d,
                         dat$catch,
                         areas = "5[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA5,
                         b = .BETA5)
write_csv(df5CD,file.path(outDir,"AnnualMeanWeight_5CD.csv"))

## 5CD - OLD LW pars
df5CDoldpars <- get.mean.weight(d,
                                dat$catch,
                                areas = "5[CD]+",
                                include.usa = include.usa,
                                a = .ALPHA2013,
                                b = .BETA2013)

## 3CD
df3CD <- get.mean.weight(d,
                         dat$catch,
                         areas = "3[CD]+",
                         include.usa = include.usa,
                         a = .ALPHA3,
                         b = .BETA3)
write_csv(df3CD,file.path(outDir,"AnnualMeanWeight_3CD.csv"))

## 5ABCD
df5ABCD <- get.mean.weight(d,
                           dat$catch,
                           areas = c("5[AB]+", "5[CD]+"),
                           include.usa = include.usa,
                           a = .ALPHA5,
                           b = .BETA5)
write_csv(df5ABCD,file.path(outDir,"AnnualMeanWeight_5ABCD.csv"))

#################################################################
## Plot results
## 5AB
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

## 5CD
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

## 3CD
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

## 5ABCD
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
## Compare to previous analyses
## 5AB
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

## 5CD
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
