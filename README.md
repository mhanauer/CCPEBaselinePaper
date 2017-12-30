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
require(sandwich)
require(msm)
require(foreign)
require(MASS)
library(AER)
library(mvnmle)
library(MissMech)
library(plyr)
library(Amelia)
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


CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$GENDER, CCPEBaseline$E_NONHISPAN, CCPEBaseline$YOB, CCPEBaseline$EDLEVEL_N, CCPEBaseline$EMPLOY_N, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR)
TestMCARNormality(data = CCPEBaseline)

# Here I am getting the number of people before imputation 
dim(CCPEBaseline)
CCPEBaselineTest = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaselineTest)

colnames(CCPEBaseline) = c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "GENDER", "E_NONHISPAN", "YOB", "EDLEVEL_N", "EMPLOY_N", "HINCOMEO_N", "SEX_PR")


m=10
a.out = amelia(x = CCPEBaseline, m=m, ords = c("RSKCIG", "RSKMJ", "RSKALC", "YOB", "HINCOMEO_N"), logs = c("CIG30D", "MJ30D", "BINGE530D"), noms = c("GENDER", "E_NONHISPAN", "SEX_PR"))
head(a.out$imputations$imp1)
head(a.out$imputations$imp2)
head(a.out$imputations$imp4)
head(a.out$imputations$imp5)
head(a.out$imputations$imp7)
CCPE1 = data.frame(na.omit(a.out$imputations$imp1))
CCPE2 = data.frame(na.omit(a.out$imputations$imp2))
CCPE3 = data.frame(na.omit(a.out$imputations$imp4))
CCPE4 = data.frame(na.omit(a.out$imputations$imp5))
CCPE5 = data.frame(na.omit(a.out$imputations$imp7))


# Number of people after imputation
dim(CCPE1)
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
Extra findings
```{r}
# Don't include speak English well ordinal variable, because 95% if 1.
SPEAK_ENGTable = table(gpraAdultBase$SPEAK_ENG)
prop.table(SPEAK_ENGTable)

# No language, because 92% is English
LANGTable = table(gpraAdultBase$LANG)
prop.table(LANGTable)
```

