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
library(Hmisc)

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


CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$R_WHITE_N, CCPEBaseline$YOB, CCPEBaseline$EDLEVEL_N, CCPEBaseline$EMPLOY_N, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR, CCPEBaseline$GENDER)
TestMCARNormality(data = CCPEBaseline)

# Here I am getting the number of people before imputation 
dim(CCPEBaseline)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaselineTest)

colnames(CCPEBaseline) = c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "YOB", "EDLEVEL_N", "EMPLOY_N", "HINCOMEO_N", "SEX_PR", "GENDER")

GENDER = data.frame(CCPEBaseline$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))
CCPEBaselineFreq = describe(CCPEBaseline)

## Get correlations
CCPEBaselineCor= data.frame(cor(CCPEBaseline))
CCPEBaselineCor = round(CCPEBaselineCor, 3)
write.csv(CCPEBaselineCor, "CCPEBaselineCor.csv")
```
No missing data model
```{r}
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Test of model fit relative to a Poisson
cigPos = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline)
summary(cigPos)
mean(CCPEBaseline$CIG30D)
var(CCPEBaseline$CIG30D)
dispersiontest(cigPos, alternative = c("greater")) 

mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline)
summary(mar)

# Test of model fit relative to a Poisson
marPos = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline)
summary(marPos)
mean(CCPEBaseline$MJ30D)
var(CCPEBaseline$MJ30D)
dispersiontest(marPos, alternative = c("greater")) 

alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline)
summary(alcohol)

# Test of model fit relative to a Poisson
alcoholPos = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline)
summary(alcoholPos)
mean(CCPEBaseline$BINGE530D)
var(CCPEBaseline$BINGE530D)
dispersiontest(alcoholPos, alternative = c("greater")) 
```

Need to impute to the extra ~200 people
```{r}
m=10
a.out = amelia(x = CCPEBaseline, m=m, ords = c("RSKCIG", "RSKMJ", "RSKALC", "HINCOMEO_N", "EDLEVEL_N", "EMPLOY_N", "YOB", "CIG30D", "MJ30D", "BINGE530D"), noms = c("GENDER", "R_WHITE_N", "SEX_PR"))
head(a.out$imputations$imp1)
head(a.out$imputations$imp2)
head(a.out$imputations$imp3)
head(a.out$imputations$imp5)
head(a.out$imputations$imp7)
CCPEBaseline1 = data.frame(na.omit(a.out$imputations$imp1))
CCPEBaseline2 = data.frame(na.omit(a.out$imputations$imp2))
CCPEBaseline3 = data.frame(na.omit(a.out$imputations$imp3))
CCPEBaseline4 = data.frame(na.omit(a.out$imputations$imp5))
CCPEBaseline5 = data.frame(na.omit(a.out$imputations$imp7))
summary(a.out)
#disperse(a.out, dims = 1, m = 5)
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
Descriptive Statistics
```{r}
mean1 =apply(CCPEBaseline1, 2, mean)
mean2 =apply(CCPEBaseline2, 2, mean)
mean3 =apply(CCPEBaseline3, 2, mean)
mean4 =apply(CCPEBaseline4, 2, mean)
mean5 =apply(CCPEBaseline5, 2, mean)

sd1 = apply(CCPEBaseline1, 2, sd)
sd2 = apply(CCPEBaseline2, 2, sd)
sd3 = apply(CCPEBaseline3, 2, sd)
sd4 = apply(CCPEBaseline4, 2, sd)
sd5 = apply(CCPEBaseline5, 2, sd)


allMeans = t(as.matrix(cbind(mean1, mean2, mean3, mean4, mean5)))

allSDs = t(as.matrix(cbind(sd1, sd2, sd3, sd4, sd5)))

allMeansSDsCom = mi.meld(q = allMeans, se = allSDs)
allMeansSDsCom = t(as.data.frame(allMeansSDsCom))
write.csv(allMeansSDsCom, "allMeansSDsCom.csv")
```
$$ ln({CIG30D_{i}) = exp(\beta_{0}) + exp(\beta_{1}(RSKCIG_{i})) + exp(\beta_{x}(X_{i})) + e_{i}}~~~ (1)$$

$$ ln({MJ30D{i}) = exp(\beta_{0}) + exp(\beta_{1}(RSKMJ{i})) + exp(\beta_{x}(X_{i})) + e_{i}}~~~ (2)$$
$$ ln({BINGE530D{i}) = exp(\beta_{0}) + exp(\beta_{1}(RSKALC{i})) + exp(\beta_{x}(X_{i})) + e_{i}}~~~ (3)$$


Cig Models
```{r}
# Cig Poisson model.  Model is overdipsered so need negative binomal
cig1 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER , data = CCPEBaseline1)
summary(cig1)

# Test of model fit relative to a Poisson
cigPos1 = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline1)
summary(cigPos1)
mean(CCPEBaseline1$CIG30D)
var(CCPEBaseline1$CIG30D)
dispersiontest(cigPos1, alternative = c("greater")) 

cig2 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER , data = CCPEBaseline2)
summary(cig2)

# Test of model fit relative to a Poisson
cigPos2 = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline2)
summary(cigPos2)
mean(CCPEBaseline2$CIG30D)
var(CCPEBaseline2$CIG30D)
dispersiontest(cigPos2, alternative = c("greater")) 

cig3 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER , data = CCPEBaseline3)
summary(cig3)

# Test of model fit relative to a Poisson
cigPos3 = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline3)
summary(cigPos3)
mean(CCPEBaseline3$CIG30D)
var(CCPEBaseline3$CIG30D)
dispersiontest(cigPos3, alternative = c("greater")) 

cig4 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline4)
summary(cig4)

# Test of model fit relative to a Poisson
cigPos4 = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline4)
summary(cigPos4)
mean(CCPEBaseline4$CIG30D)
var(CCPEBaseline4$CIG30D)
dispersiontest(cigPos4, alternative = c("greater")) 

cig5 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER , data = CCPEBaseline5)
summary(cig5)

# Test of model fit relative to a Poisson
cigPos5 = glm(CIG30D ~ RSKCIG + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline5)
summary(cigPos5)
mean(CCPEBaseline5$CIG30D)
var(CCPEBaseline5$CIG30D)
dispersiontest(cigPos5, alternative = c("greater")) 

cig1 = summary(cig1)
cig2 = summary(cig2)
cig3 = summary(cig3)
cig4 = summary(cig4)
cig5 = summary(cig5)

cigPar = t(data.frame(cig1$coefficients[,1], cig2$coefficients[,1], cig3$coefficients[,1], cig4$coefficients[,1], cig5$coefficients[,1]))
rownames(cigPar) = c()

cigSE = t(data.frame(cig1$coefficients[,2], cig2$coefficients[,2], cig3$coefficients[,2], cig4$coefficients[,2], cig5$coefficients[,2]))
rownames(cigSE) = c()

cigBoth = mi.meld(q = cigPar, se = cigSE)
cigBoth
cigAllPars = t(data.frame(cigBoth$q.mi))
colnames(cigAllPars) = c("ParameterEstimate")
cigAllSE = t(data.frame(cigBoth$se.mi))
colnames(cigAllSE) = c("StandardError")

cigBoth = data.frame(cigAllPars, cigAllSE)
cigBoth$Zstat = cigBoth$ParameterEstimate/cigBoth$StandardError
cigBoth$PValue = round(pnorm(-abs(cigBoth$Zstat)),3)
cigBoth = round(cigBoth,3)
cigBoth

# Need to exp() all parameters to get the incident rate ratio.
```
All equations and interpretation.  The model below shows the negative binomial model where the natural log (i.e. ln) of the number of days the respondent did the dependent variable (i.e. smoked cigarettes, smoked marijuana, binge drank alcohol) in the past 30 days is regressed upon the variable of interest perception of risk of the dependent variable by the same respondent which is exponentiated to improve interpretation.  We also included a vector of covariates (X) with X number of beta coefficients to account for confounding factors.  Finally, there is the error term which is the difference between the respondent's response and the predicted value.
 

Mar Models
```{r}
mar1 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline1)
summary(mar1)

# Test of model fit relative to a Poisson
marPos1 = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline1)
summary(marPos1)
mean(CCPEBaseline1$MJ30D)
var(CCPEBaseline1$MJ30D)
dispersiontest(marPos1, alternative = c("greater")) 

mar2 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline2)
summary(mar2)

# Test of model fit relative to a Poisson
marPos2 = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline2)
summary(marPos2)
mean(CCPEBaseline2$MJ30D)
var(CCPEBaseline2$MJ30D)
dispersiontest(marPos2, alternative = c("greater")) 

mar3 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline3)
summary(mar3)

# Test of model fit relative to a Poisson
marPos3 = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline3)
summary(marPos3)
mean(CCPEBaseline3$MJ30D)
var(CCPEBaseline3$MJ30D)
dispersiontest(marPos3, alternative = c("greater")) 

mar4 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline4)
summary(mar4)

# Test of model fit relative to a Poisson
marPos4 = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline4)
summary(marPos4)
mean(CCPEBaseline4$MJ30D)
var(CCPEBaseline4$MJ30D)
dispersiontest(marPos4, alternative = c("greater")) 

mar5 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline5)
summary(mar5)

# Test of model fit relative to a Poisson
marPos5 = glm(MJ30D ~ RSKMJ + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline5)
summary(marPos5)
mean(CCPEBaseline5$MJ30D)
var(CCPEBaseline5$MJ30D)
dispersiontest(marPos5, alternative = c("greater")) 

mar1 = summary(mar1)
mar2 = summary(mar2)
mar3 = summary(mar3)
mar4 = summary(mar4)
mar5 = summary(mar5)

marPar = t(data.frame(mar1$coefficients[,1], mar2$coefficients[,1], mar3$coefficients[,1], mar4$coefficients[,1], mar5$coefficients[,1]))
rownames(marPar) = c()

marSE = t(data.frame(mar1$coefficients[,2], mar2$coefficients[,2], mar3$coefficients[,2], mar4$coefficients[,2], mar5$coefficients[,2]))
rownames(marSE) = c()

marBoth = mi.meld(q = marPar, se = marSE)
marBoth
marAllPars = t(data.frame(marBoth$q.mi))
colnames(marAllPars) = c("ParameterEstimate")
marAllSE = t(data.frame(marBoth$se.mi))
colnames(marAllSE) = c("StandardError")

marBoth = data.frame(marAllPars, marAllSE)
marBoth$Zstat = marBoth$ParameterEstimate/marBoth$StandardError
marBoth$PValue = round(pnorm(-abs(marBoth$Zstat)),3)
marBoth = round(marBoth,3)
marBoth
```

Alcohol findgs
```{r}
alcohol1 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline1)
summary(alcohol1)

# Test of model fit relative to a Poisson
alcoholPos1 = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline1)
summary(alcoholPos1)
mean(CCPEBaseline1$BINGE530D)
var(CCPEBaseline1$BINGE530D)
dispersiontest(alcoholPos1, alternative = c("greater")) 

alcohol2 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline2)
summary(alcohol2)

# Test of model fit relative to a Poisson
alcoholPos2 = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline2)
summary(alcoholPos2)
mean(CCPEBaseline2$BINGE530D)
var(CCPEBaseline2$BINGE530D)
dispersiontest(alcoholPos2, alternative = c("greater")) 

alcohol3 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline3)
summary(alcohol3)

# Test of model fit relative to a Poisson
alcoholPos3 = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline3)
summary(alcoholPos3)
mean(CCPEBaseline3$BINGE530D)
var(CCPEBaseline3$BINGE530D)
dispersiontest(alcoholPos3, alternative = c("greater")) 

alcohol4 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline4)
summary(alcohol4)

# Test of model fit relative to a Poisson
alcoholPos4 = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline4)
summary(alcoholPos4)
mean(CCPEBaseline4$BINGE530D)
var(CCPEBaseline4$BINGE530D)
dispersiontest(alcoholPos4, alternative = c("greater")) 

alcohol5 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, data = CCPEBaseline5)
summary(alcohol5)

# Test of model fit relative to a Poisson
alcoholPos5 = glm(BINGE530D ~ RSKALC + R_WHITE_N + YOB + EDLEVEL_N + EMPLOY_N + HINCOMEO_N + SEX_PR+ GENDER, family = "poisson", data = CCPEBaseline5)
summary(alcoholPos5)
mean(CCPEBaseline5$BINGE530D)
var(CCPEBaseline5$BINGE530D)
dispersiontest(alcoholPos5, alternative = c("greater")) 

alcohol1 = summary(alcohol1)
alcohol2 = summary(alcohol2)
alcohol3 = summary(alcohol3)
alcohol4 = summary(alcohol4)
alcohol5 = summary(alcohol5)

alcoholPar = t(data.frame(alcohol1$coefficients[,1], alcohol2$coefficients[,1], alcohol3$coefficients[,1], alcohol4$coefficients[,1], alcohol5$coefficients[,1]))
rownames(alcoholPar) = c()

alcoholSE = t(data.frame(alcohol1$coefficients[,2], alcohol2$coefficients[,2], alcohol3$coefficients[,2], alcohol4$coefficients[,2], alcohol5$coefficients[,2]))
rownames(alcoholSE) = c()

alcoholBoth = mi.meld(q = alcoholPar, se = alcoholSE)
alcoholBoth
alcoholAllPars = t(data.frame(alcoholBoth$q.mi))
colnames(alcoholAllPars) = c("ParameterEstimate")
alcoholAllSE = t(data.frame(alcoholBoth$se.mi))
colnames(alcoholAllSE) = c("StandardError")

alcoholBoth = data.frame(alcoholAllPars, alcoholAllSE)
alcoholBoth$Zstat = alcoholBoth$ParameterEstimate/alcoholBoth$StandardError
alcoholBoth$PValue = round(pnorm(-abs(alcoholBoth$Zstat)),3)
alcoholBoth = round(alcoholBoth,3)
alcoholBoth

```


Extra findings

1= Yes sex with woman
0= No sex with woman 


1= Male
2= Female
3= Transgender (Male to female)
4= Transgender (Female to male)
5= Transgender (No further detail)
I want the number 1 so row 2 and the number 2 so column 2

```{r}
# Don't include speak English well ordinal variable, because 95% if 1.
SPEAK_ENGTable = table(gpraAdultBase$SPEAK_ENG)
prop.table(SPEAK_ENGTable)

# No language, because 92% is English
LANGTable = table(gpraAdultBase$LANG)
prop.table(LANGTable)

# Grab the cross tabs to see if there are any women who have sex with women.
WSW = table(gpraAdultBase$SEX_FEMALEEVER, gpraAdultBase$GENDER)
prop.table(WSW)
```

