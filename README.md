---
title: "CCPEBaseline"
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


CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$E_NONHISPAN, CCPEBaseline$YOB, CCPEBaseline$EDLEVEL_N, CCPEBaseline$EMPLOY_N, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR, CCPEBaseline$GENDER)
#TestMCARNormality(data = CCPEBaseline)

# Here I am getting the number of people before imputation 
dim(CCPEBaseline)
#CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)

colnames(CCPEBaseline) = c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "E_NONHISPAN", "YOB", "EDLEVEL_N", "EMPLOY_N", "HINCOMEO_N", "SEX_PR", "GENDER")
```
Need to impute to the extra ~200 people
```{r}
m=10
a.out = amelia(x = CCPEBaseline, m=m, ords = c("RSKCIG", "RSKMJ", "RSKALC", "HINCOMEO_N", "EDLEVEL_N", "EMPLOY_N", "YOB", "CIG30D", "MJ30D", "BINGE530D"), noms = c("GENDER", "E_NONHISPAN", "SEX_PR"))
head(a.out$imputations$imp1)
head(a.out$imputations$imp2)
head(a.out$imputations$imp3)
head(a.out$imputations$imp5)
head(a.out$imputations$imp9)
CCPEBaseline1 = data.frame(na.omit(a.out$imputations$imp1))
CCPEBaseline2 = data.frame(na.omit(a.out$imputations$imp2))
CCPEBaseline3 = data.frame(na.omit(a.out$imputations$imp3))
CCPEBaseline4 = data.frame(na.omit(a.out$imputations$imp5))
CCPEBaseline5 = data.frame(na.omit(a.out$imputations$imp9))
summary(a.out)
disperse(a.out, dims = 1, m = 5)
overimpute(a.out, "CIG30D")
# Number of people after imputation
dim(CCPE1)
```


Need to transform the variables Gender (1 = Male,0 = Female and other identity), SEX_PR (1 = straight, 0 = all other sexual orientations).  Do this for each data set.
```{r}
GENDER = data.frame(CCPEBaseline1$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline1$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

CCPEBaseline1$GENDER = NULL
CCPEBaseline1$SEX_PR = NULL
CCPEBaseline1$GENDER = GENDER$GENDER
CCPEBaseline1$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline1)

GENDER = data.frame(CCPEBaseline2$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline2$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

CCPEBaseline2$GENDER = NULL
CCPEBaseline2$SEX_PR = NULL
CCPEBaseline2$GENDER = GENDER$GENDER
CCPEBaseline2$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline2)


GENDER = data.frame(CCPEBaseline3$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline3$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

CCPEBaseline3$GENDER = NULL
CCPEBaseline3$SEX_PR = NULL
CCPEBaseline3$GENDER = GENDER$GENDER
CCPEBaseline3$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline3)

GENDER = data.frame(CCPEBaseline4$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline4$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

CCPEBaseline4$GENDER = NULL
CCPEBaseline4$SEX_PR = NULL
CCPEBaseline4$GENDER = GENDER$GENDER
CCPEBaseline4$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline4)

GENDER = data.frame(CCPEBaseline4$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline4$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

CCPEBaseline4$GENDER = NULL
CCPEBaseline4$SEX_PR = NULL
CCPEBaseline4$GENDER = GENDER$GENDER
CCPEBaseline4$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline4)

```

Now include the new demographic variables and the regression for data set.
```{r}
# Cig Poisson model.  Model is overdipsered so need negative binomal
cig1 = glm.nb(CCPEBaseline1$CIG30D ~ CCPEBaseline1$RSKCIG + CCPEBaseline1$E_NONHISPAN + CCPEBaseline1$YOB + CCPEBaseline1$EDLEVEL_N + CCPEBaseline1$EMPLOY_N + CCPEBaseline1$HINCOMEO_N + CCPEBaseline1$SEX_PR+ CCPEBaseline1$GENDER, data = CCPEBaseline1)
summary(cig1)
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

