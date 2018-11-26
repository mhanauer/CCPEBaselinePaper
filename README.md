---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load all releveant packages

```{r}
library(effects)
require(sandwich)
require(foreign)
library(plyr)
library(Amelia)
library(MASS)
library(psych)
library(ggplot2)
library(dplyr)      # for data manipulation
library(tidyr)      # for reshaping data
library(descr)
library(pscl)
library(lmtest)
library(countreg)
library(jtools)
library(descr)
library(prettyR)
```
Just loading the data and getting rid of missing values.  Getting fid of missing data here and calculating the percentage of missing data.
```{r}

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/CCPEPaperData")
CCPEBaseline = read.csv("CCPEBaselineFull.csv", header = TRUE)
dim(CCPEBaseline)
#

CCPEBaselineDemo = CCPEBaseline[c("RSKCIG","CIG30D","MJ30D","RSKMJ", "BINGE530D",	"RSKALC",	"R_WHITE_N",	"REL_IMP", "HINCOMEO_N","SEX_PR", "GENDER",	"YOB", "R_BLACK_N", "R_ASIAIN_N", "E_NONHISPAN")]





CCPEBaselineDemo = CCPEBaselineDemo[1:744,]
write.csv(CCPEBaselineDemo, "CCPEBaselineDemo.csv", row.names = FALSE)
CCPEBaselineDemo = read.csv("CCPEBaselineDemo.csv", header = TRUE, na.strings = c(NA, 98, 99, 77, 97, " "))
CCPEBaselineDemo = na.omit(CCPEBaselineDemo)
dim(CCPEBaselineDemo)

CCPEBaselineDemo$GENDER = ifelse(CCPEBaselineDemo$GENDER > 2, NA, CCPEBaselineDemo$GENDER)
CCPEBaselineDemo$GENDER = ifelse(CCPEBaselineDemo$GENDER == 1, 1, 0)
CCPEBaselineDemo = na.omit(CCPEBaselineDemo)
dim(CCPEBaselineDemo)

describe(CCPEBaselineDemo)

#### Now back to regular data analysis ######################
CCPEBaseline = CCPEBaseline[c("RSKCIG","CIG30D","MJ30D","RSKMJ", "BINGE530D",	"RSKALC",	"R_WHITE_N",	"REL_IMP", "HINCOMEO_N","SEX_PR", "GENDER",	"YOB")]

dim(CCPEBaseline)



CCPEBaseline = CCPEBaseline[1:744,]
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE, na.strings = c(NA, 98, 99, 77, 97, " "))
CCPEBaseline = na.omit(CCPEBaseline)
dim(CCPEBaseline)
#CCPEBaseline = data.frame(na.omit(CCPEBaseline))
#dim(CCPEBaseline)
516-588
0.8776-1
```
Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people.  1 equals male and 2 equals female.  Need to read and write the dataset to get the variables to be factors.  Also, changing gender to be 1 for male and 0 for female.

So I need to change the values greater than 2 to -999 so I can subset those values and figure out which rows I need to delete.  Remember to find the deleted rows, I need to create a new data set and subset which values are -999 so I can find the rows and delete them below
```{r}
CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER > 2, NA, CCPEBaseline$GENDER)
CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER == 1, 1, 0)
CCPEBaseline = na.omit(CCPEBaseline)
dim(CCPEBaseline)
```
Now change AGE to AGE by subtracting 2018 from YOB. 

Ok try subsetting with only age 24.
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
describe.factor(CCPEBaseline$AGE)
CCPEBaseline = subset(CCPEBaseline, AGE < 29)
dim(CCPEBaseline)

## Before getting rid of old people there were 592 then there 571 afterwards.  So need to get this difference from the demoniator to get the demonitaor of actual eligible people for the study.
571/(745-(592-571))
```
Change home income to split on something 30,000 or lower is low income.  We choose $30,000 because it was in the middle of the scale for the GPRA.  Ok so 1 and 2 are 30,000 and below so they are 0 and everything else is 1, because options 3,4,5 and higher than 30,000.

Change sex orientation to straight or non-straight where is straight and zero is non-straight
```{r}
CCPEBaseline$INCOME = ifelse(CCPEBaseline$HINCOMEO_N == 1, 0, ifelse(CCPEBaseline$HINCOMEO_N == 2, 0, 1))
CCPEBaseline$HINCOMEO_N = NULL
CCPEBaseline$SEX_PR = ifelse(CCPEBaseline$SEX_PR ==1, 1, 0)
```



Get descriptives.  Break them down by continous and non-continous.  Continous just get the mean and sd, but for ordinal get the count and percentage.
```{r}
CCPEBaselineCount = data.frame(CIG30D = CCPEBaseline$CIG30D, MJ30D=CCPEBaseline$MJ30D, BINGE530D=CCPEBaseline$BINGE530D, CCPEBaseline$RSKCIG, CCPEBaseline$RSKMJ, CCPEBaseline$RSKALC)
round(apply(CCPEBaselineCount, 2, mean),2)
round(apply(CCPEBaselineCount, 2, sd),2)

## Now create for binary and ordinal
library(prettyR)
describeCounts = data.frame(R_WHITE_N = CCPEBaseline$R_WHITE_N, REL_IMP = CCPEBaseline$REL_IMP, INCOME=CCPEBaseline$INCOME,SEX_PR= CCPEBaseline$SEX_PR,GENDER= CCPEBaseline$GENDER)
describeCounts = apply(describeCounts, 2, function(x){describe.factor(x)})
describeCounts

round(mean(CCPEBaseline$AGE),2)
round(sd(CCPEBaseline$AGE),2)

# Gender, race, income religous importance
compmeans(CCPEBaseline$AGE, CCPEBaseline$GENDER)
compmeans(CCPEBaseline$AGE, CCPEBaseline$R_WHITE_N)
compmeans(CCPEBaseline$AGE, CCPEBaseline$INCOME)
compmeans(CCPEBaseline$AGE, CCPEBaseline$REL_IMP)
```


Now we need to mean center all ordinal and continuous variables, so use the scale function, with scale equals false, because that creates z-scores by dividing by the standard deviation.  Creating a new name for the new variable data set. And adding all of the centered variables to the original data set.

Renaming the variables, because they are now centered so I don't want to confuse them with other variables that are not centered.

Creating interaction variables, because they are easier to include in the code.  See cigarette model below the interaction terms that I created here produce the same results as including the actual interaction term in the model.
```{r}
CCPEBaselineMeanCenter = CCPEBaseline
head(CCPEBaselineMeanCenter)


CCPEBaselineMeanCenter = scale(CCPEBaselineMeanCenter, scale = FALSE)
head(CCPEBaselineMeanCenter)


colnames(CCPEBaselineMeanCenter) = c("CenterRSKCIG", "CenterCIG30D", "CenterMJ30D", "CenterRSKMJ", "CenterBINGE530D", "CenterRSKALC", "CenterR_WHITE_N", "CenterREL_IMP", "CenterSEX_PR", "CenterGENDER", "CenterYOB", "CenterAGE", "CenterINCOME")

CCPEBaseline = data.frame(CCPEBaselineMeanCenter, CCPEBaseline)
head(CCPEBaseline)


```
Final models
```{r}
cigNeg1 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg1)
summary(cigNeg1$fitted.values)
describe.factor(cigNeg1$fitted.values)

marNeg2 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg2)
summary(marNeg2$fitted.values)

bingeNeg = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg)
summary(bingeNeg$fitted.values)

```
Get the interaction term for cigarette smoking
```{r}

marNeg2Plot = hurdle(MJ30D ~   RSKMJ + R_WHITE_N + RSKMJ*AGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg2Plot)

summary(marNeg2)

interact_plot(marNeg2Plot, pred= "RSKMJ", modx = "AGE", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked")

interact_plot(marNeg2, pred= "CenterRSKMJ", modx = "CenterAGE", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked")

```
Ok so starting out with just main effects for cigarette model.  So negative binomal is the better fitting model
```{r}
cigNeg = hurdle(CIG30D ~   CenterRSKCIG +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg)
summary(cigNeg$fitted.values)


cigP = hurdle(CIG30D ~   CenterRSKCIG +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(cigP)
summary(cigP$fitted.values)
describe.factor(cigP$fitted.values)

lrtest(cigNeg, cigP)
AIC(cigNeg)
AIC(cigP)
BIC(cigNeg)
BIC(cigP)
countreg::rootogram(cigP)
countreg::rootogram(cigNeg)
```
So let us just try dropping sexual orientation
```{r}
cigNeg1 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg1)
summary(cigNeg1$fitted.values)
```

So now let us try dropping sexual orientation and adding one interaction term 
White not significant and makes the risk variable not significant
```{r}
cigNeg2 = hurdle(CIG30D ~ CenterRSKCIG + CenterRSKCIG*R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg2)
summary(cigNeg2$fitted.values)
```
Age interaction, no the risk variable is no long signficant
```{r}
cigNeg3 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterRSKCIG*CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg3)
summary(cigNeg1$fitted.values)
```
Religiuos interaction religious not sig, but risk still is
```{r}
cigNeg4 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterRSKCIG*CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg4)
summary(cigNeg4$fitted.values)
```
Try income, no makes risk insignificant
```{r}
cigNeg5 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterREL_IMP + CenterRSKCIG*INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg5)
summary(cigNeg5$fitted.values)
```
Now try gender, risk still good, but significant for gender
```{r}
cigNeg6 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + CenterRSKCIG*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg6)
summary(cigNeg6$fitted.values)
```
Mar: First compare the poisson with the negative binomal model without any interaction terms
```{r}
marNeg = hurdle(MJ30D ~   CenterRSKMJ +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg)
summary(marNeg$fitted.values)


marP = hurdle(MJ30D ~   CenterRSKMJ +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(marP)
summary(marP$fitted.values)
describe.factor(marP$fitted.values)

lrtest(marNeg, marP)
AIC(marNeg)
AIC(marP)
BIC(marNeg)
BIC(marP)
countreg::rootogram(marP)
countreg::rootogram(marNeg)
```
White interaction, not significant and makes risk not significant
```{r}
marNeg1 = hurdle(MJ30D ~   CenterRSKMJ +CenterRSKMJ*R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg1)
```
Age interaction, age is significant and risk is still in the right direction.
```{r}
marNeg2 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg2)
summary(marNeg2$fitted.values)
```
Age + Religion, religion not significant
```{r}
marNeg3 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + INCOME+ GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg3)
summary(marNeg3$fitted.values)
```
Age + income, income not significant
```{r}
marNeg4 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterREL_IMP + CenterRSKMJ*INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg4)
summary(marNeg4$fitted.values)
```
Age + gender, gender not significant 
```{r}
marNeg5 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterREL_IMP + INCOME + CenterRSKMJ*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg5)
summary(marNeg5$fitted.values)
```
Binge: First compare the poisson with the negative binomal model without any interaction terms
```{r}
bingeNeg = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg)
summary(bingeNeg$fitted.values)


bingeP = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(bingeP)
summary(bingeP$fitted.values)
describe.factor(bingeP$fitted.values)

lrtest(bingeNeg, bingeP)
AIC(bingeNeg)
AIC(bingeP)
BIC(bingeNeg)
BIC(bingeP)
countreg::rootogram(bingeP)
countreg::rootogram(bingeNeg)
```
White, not significant
```{r}
bingeNeg1 = hurdle(BINGE530D ~   CenterRSKALC +CenterRSKALC*R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg1)
summary(bingeNeg1$fitted.values)
```
Age, not significant
```{r}
bingeNeg2 = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterRSKALC*CenterAGE + CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg2)
summary(bingeNeg2$fitted.values)
```
reli, not significant
```{r}
bingeNeg3 = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterRSKALC*CenterREL_IMP + INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg3)
summary(bingeNeg3$fitted.values)
```
Income, not significant
```{r}
bingeNeg4 = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + CenterRSKALC*INCOME + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg4)
summary(bingeNeg4$fitted.values)
```
Gender, not significant
```{r}
bingeNeg5 = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + INCOME + CenterRSKALC*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg5)
summary(bingeNeg5$fitted.values)
```
Now get the comparison to the national statistics.  So change CIG30D, CIG30D, and BINGE30 to 1 and 0's with any value above you being 1 and then get the mean and compare to national statistics.

Have the summary function to check for anything negative which would be a missing value.
```{r}
nationStats = data.frame(CCPEBaseline$CIG30D,CCPEBaseline$MJ30D, CCPEBaseline$BINGE530D) 
summary(nationStats)
colnames(nationStats) = c("CIG30D", "MJ30D", "BINGE530D")
nationStats = na.omit(nationStats)
head(nationStats)
nationStats = data.frame(apply(nationStats, 2, function(x)(ifelse(x >0, 1, 0))))
head(nationStats)
apply(nationStats, 2, mean)
apply(nationStats, 2, sum)
# test

```



