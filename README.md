---
title: "CenterstoneBCA"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
CCPE Baseline Project RSKCIG, RSKMJ, RSKALC, PEERBINGE_A, WRGBINGE_A, CIG30D, BINGE530D, MJ30D
Three models that I put together look at the number of days people use alcohol, weed, and cigarettes. All are statistically significantly negative.  Maybe the theory of planned behavior might be a good fit for this data.

1. Impute data 
2. Include relevant covariates
3. Rerun with relevant covariates
```{r}
require(ggplot2)
require(sandwich)
require(msm)
require(foreign)
require(ggplot2)
require(MASS)
library(AER)
library(BaylorEdPsych)
library(mvnmle)
install.packages("MissMech")
library(MissMech)
library(plyr)
library(Amelia)

CCPEBaseline = data.frame(gpraAdultAll$RSKCIG.x, gpraAdultAll$RSKMJ.x,  gpraAdultAll$RSKALC.x, gpraAdultAll$PEERBINGE_A.x, gpraAdultAll$WRGBINGE_A.x, gpraAdultAll$CIG30D.x,  gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x)
CCPEBaseline = data.frame(apply(CCPEBaseline, 2, function(x)ifelse(x == 97, NA, x)))
head(CCPEBaseline)
setwd("C:/Users/Matthew.Hanauer/Desktop")
CCPEBaseline = write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header =TRUE)
head(CCPEBaseline)
LittleMCAR(CCPEBaseline)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
TestMCARNormality(data = CCPEBaseline)
```
Impute data
Get all the variables that you want
Change all of the 97's, 98, and 99's to NA
Assign them to the correct type
Check the distributons of everything and make changes
Figure out a way to turn a variable like month into a factor and run multiple of them
JAILTIME_N = 99 means they did not go to jail which needs to be changed to 0
```{r}
CCPEBaseline = data.frame(apply(gpraAdultBase, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, 0,x))))))
# start with these RSKCIG CIG30D MJ30D RSKMJ BINGE530D RSKALC

CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC)


m=5
a.out = amelia(x = CCPEBaseline, m=m, ords = c("CCPEBaseline.RSKCIG", "CCPEBaseline.RSKMJ", "CCPEBaseline.RSKALC"), logs = c("CCPEBaseline.CIG30D", "CCPEBaseline.MJ30D", "CCPEBaseline.BINGE530D"))
head(a.out$imputations$imp1)

```
Original Amelia
```{r}
a.out = amelia(x = CCPEBaseline, m = m, ords = c("EDLEVEL_N", "COLLEGE", "EMPLOY_N", "RSKCIG", "RSKMJ", "RSKALC", "PEERBINGE_A", "WRGBINGE_A", "WRGSEX_UNP_A", "RSKANYSEX_UNP", "RSKSEX_ALCDRG", "RSKNDL_SHR", "CNTRL_REFUSEMOOD", "CNTRL_WAITCNDM", "CNTRL_TREAT", "CNTRL_SEXPRAC", "CNTRL_ASKCNDM", "CNTRL_REFUSECNDM", "SPEAK_ENG", "REL_IMP", "EMO_AFT", "HINCOMEO_N", "SVY_TRUTH"), noms = c("MONTH", "INTERVENTION_A", "INTERVENTION_B", "INTERVENTION_C", "GENDER", "E_NONHISPAN", "SEX_PR", "LANG", "PMECONDITION", "HIV_SICK_N", "HIV_GAYSEX_N", "HIV_BCPILL_N", "HIV_DRGS_N", "HIV_CURE_N", "KNOW_HIV", "GET_MEDHLP", "KNOW_SA", "LIFE_RESP_SERV", "HIV_RESULTS_N", "TALK_ALLPERS_N", "CUT_ALC", "ANNOY_ALC", "GUILT_ALC", "SEX_HAD", "SEX_ANY30D", "LASTSEX_UNP", "SEX_MALEEVER", "SEX_FEMALEEVER", "MSTATUS_N", "LIVE_N", "HOMETYPE_N", "HC_HAVE_N", "DRGTST"), logs = c("CIG30D", "TOB30D","VAP30D","ALC30D","BINGE530D","MJ30D", "ILL30D", "RX30D", "SPICE30D", "INJECT30D", "MORN_ALC", "MENTLH30D", "SEX_MNY_3MOS", "PARSUPB"))

# All possible interesting variables
CCPEBaseline = data.frame(MONTH= CCPEBaseline[,6], CCPEBaseline[,11:16], CCPEBaseline[35:42], CCPEBaseline[,68:91], CCPEBaseline[,101:125], CCPEBaseline[,132:139])
summary(CCPEBaseline)
```


Need to do a completely missing at random test
```{r}
# Cig Poisson model.  Model is overdipsered so need negative binomal
cig = glm.nb(gpraAdultAll.CIG30D.x ~ gpraAdultAll.RSKCIG.x, data = CCPEBaseline)
summary(cig)
# Test of model fit relative to a Poisson
mean(CCPEBaseline$gpraAdultAll.CIG30D.x)
var(CCPEBaseline$gpraAdultAll.CIG30D.x)
cigPos = glm(gpraAdultAll.CIG30D.x ~ gpraAdultAll.RSKCIG.x, family = "poisson", data = CCPEBaseline)
dispersiontest(cigPos, alternative = c("greater")) 


# Marijuana
mar = glm.nb(gpraAdultAll.MJ30D.x ~ gpraAdultAll.RSKMJ.x, data = CCPEBaseline)
summary(mar)
# Test of model fit relative to a Poisson
mean(CCPEBaseline$gpraAdultAll.MJ30D.x)
var(CCPEBaseline$gpraAdultAll.MJ30D.x)
marPos = glm(gpraAdultAll.MJ30D.x ~ gpraAdultAll.RSKMJ.x, family = "poisson", data = CCPEBaseline)
dispersiontest(marPos, alternative = c("greater"))

# Alcohol
alcohol = glm.nb(gpraAdultAll.BINGE530D.x ~ gpraAdultAll.RSKALC.x, data = CCPEBaseline)
summary(alcohol)
# Test of model fit relative to a Poisson
mean(CCPEBaseline$gpraAdultAll.BINGE530D.x)
var(CCPEBaseline$gpraAdultAll.BINGE530D.x)
alcoholPos = glm(gpraAdultAll.BINGE530D.x ~ gpraAdultAll.RSKALC.x, family = "poisson", data = CCPEBaseline)
dispersiontest(alcoholPos, alternative = c("greater"))
```

