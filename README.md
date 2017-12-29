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
```{r}
CCPEBaseline = data.frame(apply(gpraAdultBase, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, NA,x))))))

CCPEBaseline = data.frame(CCPEBaseline[,6], CCPEBaseline[,11:16], CCPEBaseline[35:42])

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

