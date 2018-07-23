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

CCPEBaseline = CCPEBaseline[c("RSKCIG","CIG30D","MJ30D","RSKMJ", "BINGE530D",	"RSKALC",	"R_WHITE_N",	"REL_IMP", "HINCOMEO_N","SEX_PR", "GENDER",	"YOB")]

dim(CCPEBaseline)

CCPEBaseline = CCPEBaseline[1:744,]
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE, na.strings = c(NA, 98, 99, 77, 97, " "))
CCPEBaseline = na.omit(CCPEBaseline)

dim(CCPEBaseline)
#CCPEBaseline = data.frame(na.omit(CCPEBaseline))
#dim(CCPEBaseline)

592/745
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
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
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
Cig model looking for interactions just running all of them at once.
Theta has to do with the percentage of predicted zeros.  

waldtest(cigP, cigNeg) poisson is nested in negative binomoal so not sure why it isn't working

Lower values of the Log likelihood indicate better model fit: https://www.jmp.com/support/help/14/likelihood-aicc-and-bic.shtml


Here is an example with graph where the graph bar above zero indicates overfitting and vice versa.  It also has : https://data.library.virginia.edu/getting-started-with-negative-binomial-regression-modeling/

So the model fit statistics are saying that the negative binomal is a better fit; however, the predicted values don't make any sense, so I am not sure how the model can be a better fit.

Each of the log theta's is not statistically significant and it seems like that is the dispersion parameter, so I would like a poisson would be fine; however, the other fit statistics seem to indicate the negative binomal model is a better fit.
```{r}

# Age + Race + Religon + Income + Sex orien + Gender
cigP = hurdle(CIG30D ~   CenterRSKCIG*R_WHITE_N + CenterRSKCIG*CenterAGE + CenterRSKCIG*CenterREL_IMP + CenterRSKCIG*INCOME + CenterRSKCIG*SEX_PR+ CenterRSKCIG*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(cigP)

predHurdleCigP = predict(cigP, type = "response")
range(predHurdleCigP)


interact_plot(cigP, pred= "CenterRSKCIG", modx = "R_WHITE_N", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked")

# Even though the means are different, it could be that at different levels of risk 
# Also it gets werid at the low ends of risk, because everyone thinks it is risky 8 people who asnwered 1
describe.factor(CCPEBaseline$RSKCIG)
compmeans(CCPEBaseline$CIG30D, CCPEBaseline$R_WHITE_N)


interact_plot(cigP, pred= "CenterRSKCIG", modx = "CenterREL_IMP", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked")

  
interact_plot(cigP, pred= "CenterRSKCIG", modx = "GENDER", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked")


# Testing showing the conflicting results
cigNeg = hurdle(CIG30D ~   CenterRSKCIG*R_WHITE_N + CenterRSKCIG*CenterAGE + CenterRSKCIG*CenterREL_IMP + CenterRSKCIG*INCOME + CenterRSKCIG*SEX_PR+ CenterRSKCIG*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg)
#predHurdleCigNeg = predict(cigNeg, type = "response")
#range(predHurdleCigNeg)
#lrtest(cigNeg, cigP)
#countreg::rootogram(cigP)
#countreg::rootogram(cigNeg)

```
CenterRSKCIG:R_WHITE_N      1.19635    0.31236   3.830 0.000128 ***
So this means that whites are decreasing their risk at a much slower rate than others

CenterRSKCIG:CenterREL_IMP  0.39891    0.12467   3.200 0.001375 ** 
This means that as religion increases they see a smaller decrease relative to those who are less religious

CenterRSKCIG:GENDER        -0.57823    0.19025  -3.039 0.002372 **
This means that males see a larger decreases as risk increases relative to females



Try the original poisson versus negative binomal and see if the fit tests are better for underdispered data, probably with what we have below is that the full model for the negative binomal will not converge.
```{r}
marPReg = glm(MJ30D ~ CenterRSKMJ*R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + CenterRSKMJ*INCOME + CenterRSKMJ*SEX_PR+ CenterRSKMJ*GENDER, family = "poisson", data = CCPEBaseline) 
summary(marPReg)

marNegreg = glm.nb(MJ30D ~ CenterRSKMJ*R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + CenterRSKMJ*INCOME + CenterRSKMJ*SEX_PR+ CenterRSKMJ*GENDER, data = CCPEBaseline)
summary(cigNegreg)


AIC(marPReg)
AIC(marNegreg)

lrtest(marPReg, marNegreg)

cigPReg = glm(CIG30D ~   CenterRSKCIG*R_WHITE_N + CenterRSKCIG*CenterAGE + CenterRSKCIG*CenterREL_IMP + CenterRSKCIG*INCOME + CenterRSKCIG*SEX_PR+ CenterRSKCIG*GENDER , data = CCPEBaseline, family = "poisson")

cigNegReg = glm.nb(CIG30D ~   CenterRSKCIG*R_WHITE_N + CenterRSKCIG*CenterAGE + CenterRSKCIG*CenterREL_IMP + CenterRSKCIG*INCOME + CenterRSKCIG*SEX_PR+ CenterRSKCIG*GENDER , data = CCPEBaseline)
summary(cigNegReg)
```
Mar model same process as with cigarettes.  So werid the negative binomal is actually a better fit and the predicted values make much more sense here.
```{r}
describe.factor(CCPEBaseline$MJ30D)

MJP = hurdle(MJ30D ~ CenterRSKMJ*R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + CenterRSKMJ*INCOME + CenterRSKMJ*SEX_PR+ CenterRSKMJ*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(MJP)

#predHurdleMJP = predict(MJP, type = "response")
#range(predHurdleMJP)

MJNeg = hurdle(MJ30D ~   CenterRSKMJ*R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + CenterRSKMJ*INCOME + CenterRSKMJ*SEX_PR+ CenterRSKMJ*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(MJNeg)

predHurdleMJNeg = predict(MJNeg, type = "response")


range(predHurdleMJNeg)
lrtest(MJNeg, MJP)
countreg::rootogram(MJP)
countreg::rootogram(MJNeg)

exp(MJNeg$theta)
```
Now binge model
```{r}
describe.factor(CCPEBaseline$BINGE530D)

bingeP = hurdle(BINGE530D ~ CenterRSKALC*R_WHITE_N + CenterRSKALC*CenterAGE + CenterRSKALC*CenterREL_IMP + CenterRSKALC*INCOME + CenterRSKALC*SEX_PR+ CenterRSKALC*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(bingeP)
bingePPred = predict(bingeP, type = "response")
range(bingePPred)

bingeNeg = hurdle(BINGE530D ~ CenterRSKALC*R_WHITE_N + CenterRSKALC*CenterAGE + CenterRSKALC*CenterREL_IMP + CenterRSKALC*INCOME + CenterRSKALC*SEX_PR+ CenterRSKALC*GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg)

exp(bingeNeg$theta)

bingeNegPred = predict(bingeNeg, type = "response")
summary(bingeNegPred)

AIC(bingeP)
AIC(bingeNeg)

BIC(bingeP)
BIC(bingeNeg)

lrtest(bingeP, bingeNeg)

countreg::rootogram(MJP)
countreg::rootogram(MJNeg)

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



