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



Instead try a cross tab of gender and substance misuse
I want the average number of use for each of the categories.
Use the compmeans function to the means and numbers for each category
Do this for every categorical variable and then split age on the mean and do it for that as well.

Need to repeat this process for each substance.

Then figure out how to create a table from this.
```{r}
attach(CCPEBaseline)
R_WHITE_Marj = round(compmeans(MJ30D, R_WHITE_N), 2)
REL_IMPMarji = round(compmeans(MJ30D, REL_IMP),2)
INCOMEMarj = round(compmeans(MJ30D, INCOME),2)
SEX_PRMarj = round(compmeans(MJ30D, SEX_PR),2)
genderMarj = round(compmeans(MJ30D, GENDER),2)

## Change age to split in the mean
ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageMarj = round(compmeans(MJ30D, AGECross),2)

#### Cig  #### #### #### #### #### #### #### 

R_WHITE_CIG = round(compmeans(CIG30D, R_WHITE_N),2)
REL_IMPCIG = round(compmeans(CIG30D, REL_IMP),2)
INCOMECIG = round(compmeans(CIG30D, INCOME),2)
SEX_PRCIG = round(compmeans(CIG30D, SEX_PR),2)
genderCIG = round(compmeans(CIG30D, GENDER),2)

ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageCIG = round(compmeans(CIG30D, AGECross),2)




#### Binge #### #### #### #### #### #### #### 
R_WHITE_BINGE = round(compmeans(BINGE530D, R_WHITE_N),2)
REL_IMPBINGE = round(compmeans(BINGE530D, REL_IMP),2)
INCOMEBINGE = round(compmeans(BINGE530D, INCOME),2)
SEX_PRBINGE = round(compmeans(BINGE530D, SEX_PR),2)
genderBINGE = round(compmeans(BINGE530D, GENDER),2)

## Change age to split in the mean
ageMean = mean(AGE)
ageMean
AGECross = ifelse(AGE > ageMean, 1, 0)
ageBINGE = round(compmeans(BINGE530D, AGECross),2)



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

# Maybe I don't need this. I think I can just add in the previous data frame
CCPEBaseline = data.frame(CCPEBaselineMeanCenter, CCPEBaseline)
head(CCPEBaseline)


```
Cig model looking for interactions.  I looked for interactions one at time, because the model ran out of degrees of freedom or wouldn't run (not entirly sure, but it would run) with all the interaction terms included so looked at them one at a time.  

Theta has to do with the percentage of predicted zeros.
```{r}
# Age + Race + Religon + Income + Sex orien + Gender
cig = hurdle(CIG30D ~   CenterRSKCIG*R_WHITE_N + CenterRSKCIG*CenterAGE + CenterRSKCIG*CenterREL_IMP + CenterRSKCIG*INCOME + CenterRSKCIG*SEX_PR+ CenterRSKCIG*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(cig)

```
Mar model same process as with cigarettes.
```{r}
mar = hurdle(MJ30D ~ CenterRSKMJ*R_WHITE_N + CenterRSKMJ*CenterAGE + CenterRSKMJ*CenterREL_IMP + CenterRSKMJ*INCOME + CenterRSKMJ*SEX_PR+ CenterRSKMJ*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(mar)
```
Now binge model alc
```{r}
binge = hurdle(BINGE530D ~ CenterRSKALC*R_WHITE_N + CenterRSKALC*CenterAGE + CenterRSKALC*CenterREL_IMP + CenterRSKALC*INCOME + CenterRSKALC*SEX_PR+ CenterRSKALC*GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(binge)
describe.factor(CCPEBaseline$SEX_PR)
```




Try jtools.  First getting the data, then having the final two models mar and cigTest.
Then using the interaction plots to plot the predicted values of the number of days someone smokes based on some moderator by the sd+1, mean, and sd-1 for the risk variable.

Then we are using the sim_slopes, which looks at the slope of each risk variable either at one sd above, mean, and one sd below or for each factor.
```{r}
library(jtools)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
colnames(CCPEBaseline)[1] = "Risk_Cigarette" 
colnames(CCPEBaseline)[8] = "Religious_Importance"

#CCPEBaselineTest$INCOME = as.factor(CCPEBaselineTest$INCOME)
#CCPEBaselineTest$CenterRSKMJ = as.factor(CCPEBaselineTest$CenterRSKMJ)


summary(marCenter)

mar = glm(MJ30D ~ R_WHITE_N + CenterRSKMJ*INCOME + CenterRSKMJ*GENDER  +  AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline, family = "poisson")
cigTest = glm(CIG30D ~  R_WHITE_N + Risk_Cigarette*Religious_Importance + AGE  + INCOME + SEX_PR+ GENDER , data = CCPEBaseline, family = "poisson")
summary(cigTest)
summary(mar)

mean(CCPEBaseline$MJ30D)
var(CCPEBaseline$MJ30D)

marGender =  sim_slopes(mar, pred= "CenterRSKMJ", modx = "GENDER", johnson_neyman = TRUE, control.fdr = TRUE, robust = TRUE)
marGender
interact_plot(mar, pred= "CenterRSKMJ", modx = "GENDER", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked")


marIncome = sim_slopes(mar, pred= "CenterRSKMJ", modx = "INCOME", johnson_neyman = TRUE, control.fdr = TRUE, robust = TRUE)
marIncome

interact_plot(mar,  pred= "CenterRSKMJ", modx = "INCOME", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked", outcome.scale = "link")


cigRel = sim_slopes(cigTest, pred = "Risk_Cigarette", modx = "Religious_Importance", johnson_neyman = TRUE, control.fdr = TRUE, robust = TRUE, outcome.scale = "link", data = CCPEBaseline)
cigRel

interact_plot(cigTest, pred = "Risk_Cigarette", modx = "Religious_Importance", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked", outcome.scale = "link", data = CCPEBaseline)

interact_plot(cigTest, pred = "Risk_Cigarette", modx = "Religious_Importance", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked", outcome.scale = "response", data = CCPEBaseline)

```
Now try the code on this website: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
```{r}
#CCPEBaseline$Religious_Importance = as.factor(CCPEBaseline$Religious_Importance)
library(jtools)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
colnames(CCPEBaseline)[1] = "Risk_Cigarette" 
colnames(CCPEBaseline)[8] = "Religious_Importance"
CCPEBaseline$Risk_Cigarette
CCPEBaseline$Religious_Importance
CCPEBaseline$CIG30D
cigTest = glm.nb(CIG30D ~  R_WHITE_N + Risk_Cigarette*Religious_Importance + AGE  + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cigTest)

cigTest = glm.nb(CIG30D ~  R_WHITE_N + RSKCIG*CCPEBaseline$REL_IMP+ AGE  + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)



# When we try response we don't get negative values.  Seems like they exp() the link function, which is exactaly what response gives us, so seems like type = "response is what we want"
newDataResponse = predict(cigTest, CCPEBaseline, type = "response",se.fit = TRUE)
range(newDataResponse$fit)

hist(CCPEBaseline$CIG30D)

CCPETest = data.frame(newDataResponse$fit, CCPEBaseline$CIG30D); CCPETest

newDataLink = predict(cigTest, CCPEBaseline, type = "link",se.fit = TRUE)
range(newDataLink$fit)
range(exp(newDataLink$fit))

CCPEBaselinePredict = cbind(CCPEBaseline, newDataResponse)

ggplot(CCPEBaselinePredict, aes(Risk_Cigarette, predCount))+
  geom_line(aes(colour = factor(Religious_Importance)))

head(newData)


# Try with gender and see if you can get similar results

```
Hurdle regression

CenterRSKMJ:INCOME -0.214441 
```{r}
cigTestHurdle = hurdle(CIG30D ~  R_WHITE_N + RSKCIG*REL_IMP+ AGE  + INCOME + SEX_PR+ GENDER, data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(cigTestHurdle)



newDataResponse = predict(cigTestHurdle, CCPEBaseline, type = "response",se.fit = TRUE)
newDataResponse
range(newDataResponse)



interact_plot(cigTestHurdle, pred = "RSKCIG", modx = "REL_IMP", x.label = "Perceived risk of harm from cigarette smoking", y.label = "Predicted values for reported cigarettes smoked", outcome.scale = "response", data = CCPEBaseline)


marHurdle = hurdle(MJ30D ~ R_WHITE_N + RSKMJ*INCOME + RSKMJ*GENDER  +  AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline, dist = "poisson", zero.dist = "binomial")
summary(marHurdle)

newDataResponseMar = predict(marHurdle, CCPEBaseline, type = "response",se.fit = TRUE)
range(newDataResponseMar)



interact_plot(marHurdle, pred = "RSKMJ", modx = "INCOME", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked", outcome.scale = "response", data = CCPEBaseline)



interact_plot(marHurdle,  pred= "RSKMJ", modx = "GENDER", x.label = "Perceived risk of harm from marijuana smoking", y.label = "Predicted values for reported marijuana smoked", outcome.scale = "response")


## trying the statistical tests not working
cigRel = sim_slopes(cigTestHurdle, pred = "RSKCIG", modx = "REL_IMP", johnson_neyman = TRUE, control.fdr = TRUE, robust = TRUE, outcome.scale = "response", data = CCPEBaseline)
cigRel
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



