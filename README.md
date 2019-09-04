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
#### Now back to regular data analysis ######################
CCPEBaseline = CCPEBaseline[c("RSKCIG","CIG30D","MJ30D","RSKMJ", "BINGE530D",	"RSKALC",	"R_WHITE_N",	"REL_IMP", "GENDER",	"YOB", "R_BLACK_N", "R_ASIAIN_N")]


dim(CCPEBaseline)



CCPEBaseline = CCPEBaseline[1:744,]
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE, na.strings = c(NA, 98, 99, 77, 97, " "))

CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
describe.factor(CCPEBaseline$AGE)
CCPEBaseline = subset(CCPEBaseline, AGE < 29)
CCPEBaseline$YOB = NULL


dim(CCPEBaseline)

CCPEBaseline = subset(CCPEBaseline, GENDER <= 2)
```
Now evaluate the percentage of missing data
```{r}
CCPEBaseline_Complete = na.omit(CCPEBaseline)
dim(CCPEBaseline_Complete)[1]/dim(CCPEBaseline)[1]
library(MissMech)
TestMCARNormality(CCPEBaseline)

data.frame(apply(CCPEBaseline, 2, function(col)sum(is.na(col))/length(col)))
```


Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people.  1 equals male and 2 equals female.  Need to read and write the dataset to get the variables to be factors.  Also, changing gender to be 1 for male and 0 for female.

So I need to change the values greater than 2 to -999 so I can subset those values and figure out which rows I need to delete.  Remember to find the deleted rows, I need to create a new data set and subset which values are -999 so I can find the rows and delete them below
```{r}
CCPEBaseline = na.omit(CCPEBaseline)
CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER == 2, 1, 0)
dim(CCPEBaseline)
```
Get descriptives and sds for each
```{r}
## Cig
compmeans(CCPEBaseline$RSKCIG, CCPEBaseline$R_WHITE_N)
compmeans(CCPEBaseline$RSKCIG, CCPEBaseline$REL_IMP)
compmeans(CCPEBaseline$RSKCIG, CCPEBaseline$GENDER)

Age_cat = CCPEBaseline$AGE
mean_age = mean(Age_cat)
sd_age = sd(Age_cat)

Age_cat_code = ifelse(Age_cat <= mean_age - sd_age, "-1sd", ifelse(Age_cat <= mean_age + sd_age, "Mean", ifelse(Age_cat >= mean_age + sd_age, "+1sd", 0)))
describe.factor(Age_cat_code)
compmeans(CCPEBaseline$RSKCIG, Age_cat_code)
##Mar
compmeans(CCPEBaseline$RSKMJ, CCPEBaseline$R_WHITE_N)
compmeans(CCPEBaseline$RSKMJ, CCPEBaseline$REL_IMP)
compmeans(CCPEBaseline$RSKMJ, CCPEBaseline$GENDER)
compmeans(CCPEBaseline$RSKMJ, Age_cat_code)

##Alc
compmeans(CCPEBaseline$RSKALC, CCPEBaseline$R_WHITE_N)
compmeans(CCPEBaseline$RSKALC, CCPEBaseline$REL_IMP)
compmeans(CCPEBaseline$RSKALC, CCPEBaseline$GENDER)
compmeans(CCPEBaseline$RSKALC, Age_cat_code)

```


Get descriptives.  Break them down by continous and non-continous.  Continous just get the mean and sd, but for ordinal get the count and percentage.
```{r}
CCPEBaselineCount = data.frame(CIG30D = CCPEBaseline$CIG30D, MJ30D=CCPEBaseline$MJ30D, BINGE530D=CCPEBaseline$BINGE530D, CCPEBaseline$RSKCIG, CCPEBaseline$RSKMJ, CCPEBaseline$RSKALC)
round(apply(CCPEBaselineCount, 2, mean),2)
round(apply(CCPEBaselineCount, 2, sd),2)

## Now create for binary and ordinal
library(prettyR)
describeCounts = data.frame(R_WHITE_N = CCPEBaseline$R_WHITE_N, REL_IMP = CCPEBaseline$REL_IMP,GENDER= CCPEBaseline$GENDER, R_BLACK_N = CCPEBaseline$R_BLACK_N, R_ASIAIN_N = CCPEBaseline$R_ASIAIN_N)
describeCounts = apply(describeCounts, 2, function(x){describe.factor(x)})
describeCounts

round(mean(CCPEBaseline$AGE),2)
round(sd(CCPEBaseline$AGE),2)
range(CCPEBaseline$AGE)
# Gender, race, income religous importance
compmeans(CCPEBaseline$AGE, CCPEBaseline$GENDER)
compmeans(CCPEBaseline$AGE, CCPEBaseline$R_WHITE_N)
compmeans(CCPEBaseline$AGE, CCPEBaseline$REL_IMP)
### Create Gender Table
compmeans(CCPEBaseline$BINGE530D, CCPEBaseline$GENDER)
```
Conduct t-tests for each demographic by each each harm
```{r}
##############Cig
wilcox.test(CCPEBaseline$RSKCIG ~ CCPEBaseline$R_WHITE_N)
#### Religion

religion1_2 = subset(CCPEBaseline,REL_IMP <= 2)
wilcox.test(religion1_2$RSKCIG ~religion1_2$REL_IMP)

religion1_3 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 3)
wilcox.test(religion1_3$RSKCIG ~religion1_3$REL_IMP)

religion1_4 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 4)
wilcox.test(religion1_4$RSKCIG ~religion1_4$REL_IMP)

religion2_3 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 3)
wilcox.test(religion2_3$RSKCIG ~religion2_3$REL_IMP)

religion2_4 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 4)
wilcox.test(religion2_4$RSKCIG ~religion2_4$REL_IMP)

wilcox.test(CCPEBaseline$RSKCIG ~ CCPEBaseline$GENDER)
## Categorize age by -1sd mean +1sd
CCPEBaseline_age = data.frame(RSKCIG = CCPEBaseline$RSKCIG, Age_cat_code)
CCPEBaseline_age_1_mean = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "Mean")
wilcox.test(CCPEBaseline_age_1_mean$RSKCIG ~ CCPEBaseline_age_1_mean$Age_cat_code)
CCPEBaseline_age_1_1 = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_1_1$RSKCIG ~ CCPEBaseline_age_1_1$Age_cat_code)
CCPEBaseline_age_Mean_1 = subset(CCPEBaseline_age, Age_cat_code == "Mean" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_Mean_1$RSKCIG ~ CCPEBaseline_age_Mean_1$Age_cat_code)

###############M Mar
wilcox.test(CCPEBaseline$RSKMJ ~ CCPEBaseline$R_WHITE_N)
#### Religion

religion1_2 = subset(CCPEBaseline,REL_IMP <= 2)
wilcox.test(religion1_2$RSKMJ ~religion1_2$REL_IMP)

religion1_3 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 3)
wilcox.test(religion1_3$RSKMJ ~religion1_3$REL_IMP)

religion1_4 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 4)
wilcox.test(religion1_4$RSKMJ ~religion1_4$REL_IMP)

religion2_3 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 3)
wilcox.test(religion2_3$RSKMJ ~religion2_3$REL_IMP)

religion2_4 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 4)
wilcox.test(religion2_4$RSKMJ ~religion2_4$REL_IMP)

wilcox.test(CCPEBaseline$RSKMJ ~ CCPEBaseline$GENDER)
## Categorize age by -1sd mean +1sd
CCPEBaseline_age = data.frame(RSKMJ  = CCPEBaseline$RSKMJ , Age_cat_code)
CCPEBaseline_age_1_mean = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "Mean")
wilcox.test(CCPEBaseline_age_1_mean$RSKMJ  ~ CCPEBaseline_age_1_mean$Age_cat_code)
CCPEBaseline_age_1_1 = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_1_1$RSKMJ  ~ CCPEBaseline_age_1_1$Age_cat_code)
CCPEBaseline_age_Mean_1 = subset(CCPEBaseline_age, Age_cat_code == "Mean" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_Mean_1$RSKMJ  ~ CCPEBaseline_age_Mean_1$Age_cat_code)


###############M Alc
wilcox.test(CCPEBaseline$RSKALC ~ CCPEBaseline$R_WHITE_N)
#### Religion

religion1_2 = subset(CCPEBaseline,REL_IMP <= 2)
wilcox.test(religion1_2$RSKALC ~religion1_2$REL_IMP)

religion1_3 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 3)
wilcox.test(religion1_3$RSKALC ~religion1_3$REL_IMP)

religion1_4 = subset(CCPEBaseline,REL_IMP == 1 | REL_IMP == 4)
wilcox.test(religion1_4$RSKALC ~religion1_4$REL_IMP)

religion2_3 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 3)
wilcox.test(religion2_3$RSKALC ~religion2_3$REL_IMP)

religion2_4 = subset(CCPEBaseline,REL_IMP == 2 | REL_IMP == 4)
wilcox.test(religion2_4$RSKALC ~religion2_4$REL_IMP)

wilcox.test(CCPEBaseline$RSKALC ~ CCPEBaseline$GENDER)
## Categorize age by -1sd mean +1sd
CCPEBaseline_age = data.frame(RSKALC = CCPEBaseline$RSKALC, Age_cat_code)
CCPEBaseline_age_1_mean = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "Mean")
wilcox.test(CCPEBaseline_age_1_mean$RSKALC ~ CCPEBaseline_age_1_mean$Age_cat_code)
CCPEBaseline_age_1_1 = subset(CCPEBaseline_age, Age_cat_code == "-1sd" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_1_1$RSKALC ~ CCPEBaseline_age_1_1$Age_cat_code)
CCPEBaseline_age_Mean_1 = subset(CCPEBaseline_age, Age_cat_code == "Mean" | Age_cat_code == "+1sd")
wilcox.test(CCPEBaseline_age_Mean_1$RSKALC ~ CCPEBaseline_age_Mean_1$Age_cat_code)

```



Now we need to mean center all ordinal and continuous variables, so use the scale function, with scale equals false, because that creates z-scores by dividing by the standard deviation.  Creating a new name for the new variable data set. And adding all of the centered variables to the original data set.

Renaming the variables, because they are now centered so I don't want to confuse them with other variables that are not centered.

Creating interaction variables, because they are easier to include in the code.  See cigarette model below the interaction terms that I created here produce the same results as including the actual interaction term in the model.
```{r}
CCPEBaselineMeanCenter = CCPEBaseline
CCPEBaselineMeanCenter$R_BLACK_N = NULL
CCPEBaselineMeanCenter$R_ASIAIN_N = NULL
head(CCPEBaselineMeanCenter)


CCPEBaselineMeanCenter = scale(CCPEBaselineMeanCenter, scale = FALSE)
head(CCPEBaselineMeanCenter)


colnames(CCPEBaselineMeanCenter) = c("CenterRSKCIG", "CenterCIG30D", "CenterMJ30D", "CenterRSKMJ", "CenterBINGE530D", "CenterRSKALC", "CenterR_WHITE_N", "CenterREL_IMP", "CenterGENDER", "CenterAGE")

CCPEBaseline = data.frame(CCPEBaselineMeanCenter, CCPEBaseline)
head(CCPEBaseline)


```
Final models
```{r}
cigNeg1 = hurdle(CIG30D ~ CenterRSKCIG  +R_WHITE_N + CenterAGE + CenterREL_IMP  + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(cigNeg1)
summary(cigNeg1$fitted.values)
describe.factor(cigNeg1$fitted.values)

marNeg2 = hurdle(MJ30D ~   CenterRSKMJ + R_WHITE_N + CenterRSKMJ*CenterAGE + CenterREL_IMP + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(marNeg2)
summary(marNeg2$fitted.values)

bingeNeg = hurdle(BINGE530D ~   CenterRSKALC +R_WHITE_N + CenterAGE + CenterREL_IMP + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
summary(bingeNeg)
summary(bingeNeg$fitted.values)

```
Get the interaction term for cigarette smoking
```{r}

marNeg2Plot = hurdle(MJ30D ~   RSKMJ + R_WHITE_N + RSKMJ*AGE + CenterREL_IMP  + GENDER , data = CCPEBaseline, dist = "negbin", zero.dist = "binomial")
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
