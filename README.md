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

Cig Models
```{r}
# Cig Poisson model.  Model is overdipsered so need negative binomal
cig1 = glm.nb(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER , data = CCPEBaseline1)
summary(cig1)

# Test of model fit relative to a Poisson
cigPos1 = glm(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER, family = "poisson", data = CCPEBaseline1)
summary(cigPos1)
mean(CCPEBaseline1$CIG30D)
var(CCPEBaseline1$CIG30D)
dispersiontest(cigPos1, alternative = c("greater")) 

cig2 = glm.nb(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER , data = CCPEBaseline2)
summary(cig2)

# Test of model fit relative to a Poisson
cigPos2 = glm(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER, family = "poisson", data = CCPEBaseline2)
summary(cigPos2)
mean(CCPEBaseline2$CIG30D)
var(CCPEBaseline2$CIG30D)
dispersiontest(cigPos2, alternative = c("greater")) 

cig3 = glm.nb(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER , data = CCPEBaseline3)
summary(cig3)

# Test of model fit relative to a Poisson
cigPos3 = glm(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER, family = "poisson", data = CCPEBaseline3)
summary(cigPos3)
mean(CCPEBaseline3$CIG30D)
var(CCPEBaseline3$CIG30D)
dispersiontest(cigPos3, alternative = c("greater")) 

cig4 = glm.nb(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER , data = CCPEBaseline4)
summary(cig4)

# Test of model fit relative to a Poisson
cigPos4 = glm(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER, family = "poisson", data = CCPEBaseline4)
summary(cigPos4)
mean(CCPEBaseline4$CIG30D)
var(CCPEBaseline4$CIG30D)
dispersiontest(cigPos4, alternative = c("greater")) 

cig5 = glm.nb(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER , data = CCPEBaseline5)
summary(cig5)

# Test of model fit relative to a Poisson
cigPos5 = glm(CIG30D ~ RSKCIG + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKCIG*E_NONHISPAN + RSKCIG*YOB + RSKCIG*EDLEVEL_N + RSKCIG*HINCOMEO_N + RSKCIG*SEX_PR +RSKCIG*GENDER, family = "poisson", data = CCPEBaseline5)
summary(cigPos5)
mean(CCPEBaseline5$CIG30D)
var(CCPEBaseline5$CIG30D)
dispersiontest(cigPos5, alternative = c("greater")) 

summary(cig1)
summary(cig2)
summary(cig3)
summary(cig4)
summary(cig5)

```
Mar Models
```{r}
mar1 = glm.nb(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER , data = CCPEBaseline1)
summary(mar1)

# Test of model fit relative to a Poisson
marPos1 = glm(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER, family = "poisson", data = CCPEBaseline1)
summary(marPos1)
mean(CCPEBaseline1$MJ30D)
var(CCPEBaseline1$MJ30D)
dispersiontest(marPos1, alternative = c("greater")) 

mar2 = glm.nb(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER , data = CCPEBaseline2)
summary(mar2)

# Test of model fit relative to a Poisson
marPos2 = glm(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER, family = "poisson", data = CCPEBaseline2)
summary(marPos2)
mean(CCPEBaseline2$MJ30D)
var(CCPEBaseline2$MJ30D)
dispersiontest(marPos2, alternative = c("greater")) 

mar3 = glm.nb(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER , data = CCPEBaseline3)
summary(mar3)

# Test of model fit relative to a Poisson
marPos3 = glm(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER, family = "poisson", data = CCPEBaseline3)
summary(marPos3)
mean(CCPEBaseline3$MJ30D)
var(CCPEBaseline3$MJ30D)
dispersiontest(marPos3, alternative = c("greater")) 

mar4 = glm.nb(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER , data = CCPEBaseline4)
summary(mar4)

# Test of model fit relative to a Poisson
marPos4 = glm(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER, family = "poisson", data = CCPEBaseline4)
summary(marPos4)
mean(CCPEBaseline4$MJ30D)
var(CCPEBaseline4$MJ30D)
dispersiontest(marPos4, alternative = c("greater")) 

mar5 = glm.nb(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER , data = CCPEBaseline5)
summary(mar5)

# Test of model fit relative to a Poisson
marPos5 = glm(MJ30D ~ RSKMJ + E_NONHISPAN + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER +RSKMJ*E_NONHISPAN + RSKMJ*YOB + RSKMJ*EDLEVEL_N + RSKMJ*HINCOMEO_N + RSKMJ*SEX_PR +RSKMJ*GENDER, family = "poisson", data = CCPEBaseline5)
summary(marPos5)
mean(CCPEBaseline5$MJ30D)
var(CCPEBaseline5$MJ30D)
dispersiontest(marPos5, alternative = c("greater")) 

summary(mar1)
summary(mar2)
summary(mar3)
summary(mar4)
summary(mar5)

```

Alcohol findgs
```{r}
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

