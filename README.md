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

Need to drop employment variable.  Need to include religion

Best I can do for age is 2018 - AGE
```{r}
CCPEBaseline = data.frame(apply(gpraAdultBase, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, 0,x))))))


CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$R_WHITE_N, CCPEBaseline$REL_IMP, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR, CCPEBaseline$GENDER, CCPEBaseline$YOB)
#TestMCARNormality(data = CCPEBaseline)


# Here I am getting the number of people before imputation 
dim(CCPEBaseline)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)

colnames(CCPEBaseline) = c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "REL_IMP", "INCOME", "SEX_PR", "GENDER", "YOB")

CCPEBaselineFreq = describe(CCPEBaseline)
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE)
## Get correlations
#CCPEBaselineCor= data.frame(cor(CCPEBaseline))
#CCPEBaselineCor = round(CCPEBaselineCor, 3)
#write.csv(CCPEBaselineCor, "CCPEBaselineCor.csv")
```
Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people
```{r}
CCPEBaseline =subset(CCPEBaseline, GENDER == 1 | GENDER == 2)
dim(CCPEBaseline)

CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER == 1,1,0)

```
Now change AGE to AGE by subtracting 2018
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
```
Change home income to split on something $30,000 or lower is low income.
Change sex orientation to straight or non-straight
```{r}
count(CCPEBaseline$INCOME)
CCPEBaseline$INCOME = ifelse(CCPEBaseline$INCOME == 1, 1, ifelse(CCPEBaseline$INCOME == 2, 1, 0))
CCPEBaseline$INCOME

CCPEBaseline$SEX_PR = ifelse(CCPEBaseline$SEX_PR ==1, 1, 0)
CCPEBaseline$SEX_PR
```

Now we need to mean center all ordinal and continuous variables
#
```{r}
CCPEBaselineMeanCenter = CCPEBaseline
head(CCPEBaselineMeanCenter)
describe(CCPEBaseline)

CCPEBaselineMeanCenter = scale(CCPEBaselineMeanCenter, scale = FALSE)
head(CCPEBaselineMeanCenter)
colnames(CCPEBaselineMeanCenter) = c("CenterRSKCIG", "CenterCIG30D", "CenterMJ30D", "CenterRSKMJ", "CenterBINGE530D", "CenterRSKALC", "CenterR_WHITE_N", "CenterREL_IMP", "CenterINCOME", "CenterSEX_PR", "CenterGENDER", "CenterYOB", "CenterAGE")

# Maybe I don't need this. I think I can just add in the previous data frame
CCPEBaseline = data.frame(CCPEBaselineMeanCenter, CCPEBaseline)
head(CCPEBaseline)

# Now create interaction terms
# CenterRSKCIG
CenterRSKCIG_CenterR_WHITE_N = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterR_WHITE_N
CenterRSKCIG_CenterREL_IMP = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterREL_IMP
CenterRSKCIG_CenterINCOME = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterINCOME
CenterRSKCIG_CenterSEX_PR = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterSEX_PR
CenterRSKCIG_CenterGENDER = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterGENDER
CenterRSKCIG_CenterAGE = CCPEBaseline$CenterRSKCIG*CCPEBaseline$CenterAGE

# Now mar interaction 
CenterRSKMJ_CenterR_WHITE_N = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterR_WHITE_N
CenterRSKMJ_CenterREL_IMP = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterREL_IMP
CenterRSKMJ_CenterINCOME = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterINCOME
CenterRSKMJ_CenterSEX_PR = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterSEX_PR
CenterRSKMJ_CenterGENDER = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterGENDER
CenterRSKMJ_CenterAGE = CCPEBaseline$CenterRSKMJ*CCPEBaseline$CenterAGE

# Now alcohol interaction
CenterRSKALC_CenterR_WHITE_N = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterR_WHITE_N
CenterRSKALC_CenterREL_IMP = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterREL_IMP
CenterRSKALC_CenterINCOME = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterINCOME
CenterRSKALC_CenterSEX_PR = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterSEX_PR
CenterRSKALC_CenterGENDER = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterGENDER
CenterRSKALC_CenterAGE = CCPEBaseline$CenterRSKALC*CCPEBaseline$CenterAGE


CCPEBaseline = cbind(CCPEBaseline, CenterRSKCIG_CenterR_WHITE_N, CenterRSKCIG_CenterREL_IMP, CenterRSKCIG_CenterINCOME, CenterRSKCIG_CenterSEX_PR, CenterRSKCIG_CenterGENDER, CenterRSKCIG_CenterAGE, CenterRSKMJ_CenterR_WHITE_N, CenterRSKMJ_CenterREL_IMP, CenterRSKMJ_CenterINCOME, CenterRSKMJ_CenterSEX_PR, CenterRSKMJ_CenterGENDER, CenterRSKMJ_CenterAGE, CenterRSKALC_CenterR_WHITE_N, CenterRSKALC_CenterREL_IMP, CenterRSKALC_CenterINCOME, CenterRSKALC_CenterSEX_PR, CenterRSKALC_CenterGENDER, CenterRSKALC_CenterAGE)

head(CCPEBaseline)
write.csv(CCPEBaseline, "CCPEBaseline", row.names = FALSE)
```
Demographic model only
```{r}
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~  R_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

marPos = glm.nb(MJ30D ~  R_WHITE_N + AGE + REL_IMP + INCOME + SEX_PR+ GENDER, data = CCPEBaseline)
summary(marPos)

alcohol = glm.nb(BINGE530D ~  R_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER, data = CCPEBaseline)
summary(alcohol)

```
Cig model looking for interactions
```{r}
# Race
CCPEBaseline = data.frame(na.omit(CCPEBaseline))

dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG +  CenterRSKCIG_CenterR_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Age
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterAGE + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Religon is sig
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Income
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + CenterRSKCIG_CenterINCOME + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Sex orien
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + CenterRSKCIG_CenterSEX_PR + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Gender
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + CenterRSKCIG_CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Final model no impute
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)
dim(CCPEBaseline)

```

Mar model
```{r}
# Race
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterR_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Age
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterAGE + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Religon 
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Income is sig
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N  + CenterRSKMJ_CenterINCOME + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Sex orien
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ_CenterSEX_PR + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Gender is sig 
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ_CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

# Final model no impute
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ_CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

```
Getting the graphs for RiskCig and Religon.  So risk by religion for all four options across x-axis of number of times there is substance use.

If we did the means, we need the means for low and high income for Risk and behavior.  Then I need a grouping variable that is income so I can group them.  

```{r}
library(ggplot2)
# What variables do I need?  Risk, Cig outcome, and religion
MarRSKMJGraphLow = data.frame(CCPEBaseline$RSKMJ, CCPEBaseline$MJ30D, CCPEBaseline$INCOME)
colnames(MarRSKMJGraphLow) = c("RSKMJ", "MJ30D", "INCOME")
MarRSKMJGraphLow = subset(MarRSKMJGraphLow, INCOME == 1)
MarRSKMJGraphLow = c(mean(MarRSKMJGraphLow$RSKMJ)+sd(MarRSKMJGraphLow$RSKMJ), mean(MarRSKMJGraphLow$RSKMJ), mean(MarRSKMJGraphLow$RSKMJ)-sd(MarRSKMJGraphLow$RSKMJ))

MarRSKMJGraphHigh = data.frame(CCPEBaseline$RSKMJ, CCPEBaseline$MJ30D, CCPEBaseline$INCOME)
colnames(MarRSKMJGraphHigh) = c("RSKMJ", "MJ30D", "INCOME")
MarRSKMJGraphHigh = subset(MarRSKMJGraphHigh, INCOME == 0)
MarRSKMJGraphHigh = c(mean(MarRSKMJGraphHigh$RSKMJ), mean(MarRSKMJGraphHigh$RSKMJ)+sd(MarRSKMJGraphHigh$RSKMJ), mean(MarRSKMJGraphHigh$RSKMJ)-sd(MarRSKMJGraphHigh$RSKMJ))

MarRSKMJGraph = data.frame(MarRSKMJGraphLow, MarRSKMJGraphHigh)
MarRSKMJGraph = stack(MarRSKMJGraph)
MarRSKMJGraph = MarRSKMJGraph$values
MarRSKMJGraph

MarCountGraphLow = data.frame(CCPEBaseline$RSKMJ, CCPEBaseline$MJ30D, CCPEBaseline$INCOME)
colnames(MarCountGraphLow) = c("RSKMJ", "MJ30D", "INCOME")
MarCountGraphLow = subset(MarCountGraphLow, INCOME == 1)
MarCountGraphLow = c(mean(MarCountGraphLow$MJ30D)+sd(MarCountGraphLow$MJ30D), mean(MarCountGraphLow$MJ30D), mean(MarCountGraphLow$MJ30D)-sd(MarCountGraphLow$MJ30D))

MarCountGraphHigh = data.frame(CCPEBaseline$RSKMJ, CCPEBaseline$MJ30D, CCPEBaseline$INCOME)
colnames(MarCountGraphHigh) = c("RSKMJ", "MJ30D", "INCOME")
MarCountGraphHigh = subset(MarCountGraphHigh, INCOME == 0)
MarCountGraphHigh = c(mean(MarCountGraphHigh$MJ30D)+sd(MarCountGraphHigh$MJ30D), mean(MarCountGraphHigh$MJ30D), mean(MarCountGraphHigh$MJ30D)-sd(MarCountGraphHigh$MJ30D))
MarCountGraphHigh

MarCountGraph = data.frame(MarCountGraphLow, MarCountGraphHigh)
MarCountGraph = stack(MarCountGraph)
MarCountGraph = MarCountGraph$values
MarCountGraph

INCOME = c(rep(1,3),rep(0, 3),rep(1,3),rep(0,3))



MarGraphAll = data.frame(MarRSKMJGraph, MarCountGraph, INCOME)

MarGraphAll$INCOME = factor(MarGraphAll$INCOME)

theme_set(theme_grey(base_size = 13))
p = ggplot(data = MarGraphAll, aes(x = MarCountGraph, y = MarRSKMJGraph, group = INCOME, colour = INCOME)) + 
  geom_line(aes(group = INCOME)) +
  geom_point(aes(group = INCOME)) +
  xlab("Mean and SD +- 1 count of marijuana use") +
  ylab("Mean and SD +- 1 perception of risk of marijuana use")
p = p+ggtitle("Interaction between RSKMJ and INCOME");p



```
Conducting subgroup analysis modeling for income and marijuana risk and income
```{r}
CCPEBaselineLowIncome = subset(CCPEBaseline, INCOME == 1)
CCPEBaselineLowIncome = data.frame(na.omit(CCPEBaselineLowIncome))
marLow = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N  + CenterRSKMJ_CenterGENDER + AGE  + REL_IMP + SEX_PR+ GENDER , data = CCPEBaselineLowIncome)
summary(marLow)

CCPEBaselineHighIncome = subset(CCPEBaseline, INCOME == 0)
CCPEBaselineHighIncome = data.frame(na.omit(CCPEBaselineHighIncome))
marHigh = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N  + CenterRSKMJ_CenterGENDER + AGE  + REL_IMP + SEX_PR+ GENDER , data = CCPEBaselineHighIncome)
summary(marHigh)

```
risk and gender for mar

Male Risk Parameter Estimate = -0.890495
Female Risk Parameter Estimate =  
```{r}
CCPEBaselineMale = subset(CCPEBaseline, GENDER == 1)
CCPEBaselineMale = data.frame(na.omit(CCPEBaselineMale))
marMale = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N  + CenterRSKMJ_CenterINCOME + AGE  + REL_IMP + INCOME + SEX_PR , data = CCPEBaselineMale)
summary(marMale)

CCPEBaselineFemale = subset(CCPEBaseline, GENDER == 0)
CCPEBaselineFemale = data.frame(na.omit(CCPEBaselineFemale))
marFemale = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N  + CenterRSKMJ_CenterINCOME + AGE  + REL_IMP + INCOME + SEX_PR , data = CCPEBaselineFemale)
summary(marFemale)
```


Binge drinking interaction testing
```{r}
# Race
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + CenterRSKALC_CenterR_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Age
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + CenterRSKALC_CenterAGE + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Religon 
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + CenterRSKALC_CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Income
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + CenterRSKALC_CenterINCOME + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Sex orien
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + CenterRSKALC_CenterSEX_PR + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Gender
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + CenterRSKALC_CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

# Final model no impute
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

```
Need to impute to the extra ~200 people
```{r}
CCPEBaseline = data.frame(apply(gpraAdultBase, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, 0,x))))))


CCPEBaseline = data.frame(CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$R_WHITE_N, CCPEBaseline$REL_IMP, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR, CCPEBaseline$GENDER, CCPEBaseline$YOB)


colnames(CCPEBaseline) = c("RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "REL_IMP", "INCOME", "SEX_PR", "GENDER", "YOB")

CCPEBaselineFreq = describe(CCPEBaseline)
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE)


m=10
a.out = amelia(x = CCPEBaseline, m=m, ords = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "REL_IMP", "YOB", "CIG30D", "MJ30D", "BINGE530D"), noms = c("GENDER", "R_WHITE_N", "SEX_PR"))
head(a.out$imputations$imp1)
head(a.out$imputations$imp2)
head(a.out$imputations$imp4)
head(a.out$imputations$imp5)
head(a.out$imputations$imp9)
CCPEBaseline1 = data.frame(na.omit(a.out$imputations$imp1))
CCPEBaseline2 = data.frame(na.omit(a.out$imputations$imp2))
CCPEBaseline3 = data.frame(na.omit(a.out$imputations$imp4))
CCPEBaseline4 = data.frame(na.omit(a.out$imputations$imp5))
CCPEBaseline5 = data.frame(na.omit(a.out$imputations$imp9))
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

CCPEBaseline5$GENDER = NULL
CCPEBaseline5$SEX_PR = NULL
CCPEBaseline5$GENDER = GENDER$GENDER
CCPEBaseline5$SEX_PR = SEX_PR$SEX_PR
head(CCPEBaseline5)

GENDER = data.frame(CCPEBaseline5$GENDER)
colnames(GENDER) = c("GENDER")
GENDER = data.frame(apply(GENDER, 2, function(x)(ifelse(x ==1, 1, 0))))

SEX_PR = data.frame(CCPEBaseline5$SEX_PR)
colnames(SEX_PR) = c("SEX_PR")
SEX_PR = data.frame(apply(SEX_PR, 2, function(x)(ifelse(x ==1, 1, 0))))

```
Now change YOB to age
```{r}
CCPEBaseline1$AGE = 2018-CCPEBaseline1$YOB
CCPEBaseline2$AGE = 2018-CCPEBaseline2$YOB
CCPEBaseline3$AGE = 2018-CCPEBaseline3$YOB
CCPEBaseline4$AGE = 2018-CCPEBaseline4$YOB
CCPEBaseline5$AGE = 2018-CCPEBaseline5$YOB

```

Change home income to split on something $30,000 or lower is low income.
Change sex orientation to straight or non-straight
```{r}
CCPEBaseline1$INCOME = ifelse(CCPEBaseline1$INCOME == 1, 1, ifelse(CCPEBaseline1$INCOME == 2, 1, 0))

CCPEBaseline1$SEX_PR = ifelse(CCPEBaseline1$SEX_PR ==1, 1, 0)
CCPEBaseline1$SEX_PR

CCPEBaseline2$INCOME = ifelse(CCPEBaseline2$INCOME == 1, 1, ifelse(CCPEBaseline2$INCOME == 2, 1, 0))

CCPEBaseline2$SEX_PR = ifelse(CCPEBaseline2$SEX_PR ==1, 1, 0)
CCPEBaseline2$SEX_PR

CCPEBaseline3$INCOME = ifelse(CCPEBaseline3$INCOME == 1, 1, ifelse(CCPEBaseline3$INCOME == 2, 1, 0))

CCPEBaseline3$SEX_PR = ifelse(CCPEBaseline3$SEX_PR ==1, 1, 0)
CCPEBaseline3$SEX_PR

CCPEBaseline4$INCOME = ifelse(CCPEBaseline4$INCOME == 1, 1, ifelse(CCPEBaseline4$INCOME == 2, 1, 0))

CCPEBaseline4$SEX_PR = ifelse(CCPEBaseline4$SEX_PR ==1, 1, 0)
CCPEBaseline4$SEX_PR

CCPEBaseline5$INCOME = ifelse(CCPEBaseline5$INCOME == 1, 1, ifelse(CCPEBaseline5$INCOME == 2, 1, 0))

CCPEBaseline5$SEX_PR = ifelse(CCPEBaseline5$SEX_PR ==1, 1, 0)
CCPEBaseline5$SEX_PR
```
Now we need to mean center all ordinal and continuous variables
#
```{r}
CCPEBaseline1MeanCenter1 = data.frame(CCPEBaseline1$RSKCIG, CCPEBaseline1$RSKMJ, CCPEBaseline1$RSKALC, CCPEBaseline1$INCOME, CCPEBaseline1$AGE, CCPEBaseline1$REL_IMP)
CCPEBaseline1MeanCenter1 = scale(CCPEBaseline1MeanCenter1, scale = FALSE)
CCPEBaseline1 = data.frame(CCPEBaseline1MeanCenter1, CCPEBaseline1$CIG30D, CCPEBaseline1$MJ30D, CCPEBaseline1$BINGE530D, CCPEBaseline1$R_WHITE_N, CCPEBaseline1$SEX_PR, CCPEBaseline1$GENDER)
CCPEBaseline1
colnames(CCPEBaseline1) = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "AGE", "REL_IMP", "CIG30D", "MJ30D","BINGE530D", "R_WHITE_N", "SEX_PR", "GENDER")


CCPEBaseline2MeanCenter1 = data.frame(CCPEBaseline2$RSKCIG, CCPEBaseline2$RSKMJ, CCPEBaseline2$RSKALC, CCPEBaseline2$INCOME, CCPEBaseline2$AGE, CCPEBaseline2$REL_IMP)
CCPEBaseline2MeanCenter1 = scale(CCPEBaseline2MeanCenter1, scale = FALSE)
CCPEBaseline2 = data.frame(CCPEBaseline2MeanCenter1, CCPEBaseline2$CIG30D, CCPEBaseline2$MJ30D, CCPEBaseline2$BINGE530D, CCPEBaseline2$R_WHITE_N, CCPEBaseline2$SEX_PR, CCPEBaseline2$GENDER)
CCPEBaseline2
colnames(CCPEBaseline2) = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "AGE", "REL_IMP", "CIG30D", "MJ30D","BINGE530D", "R_WHITE_N", "SEX_PR", "GENDER")

CCPEBaseline3MeanCenter1 = data.frame(CCPEBaseline3$RSKCIG, CCPEBaseline3$RSKMJ, CCPEBaseline3$RSKALC, CCPEBaseline3$INCOME, CCPEBaseline3$AGE, CCPEBaseline3$REL_IMP)
CCPEBaseline3MeanCenter1 = scale(CCPEBaseline3MeanCenter1, scale = FALSE)
CCPEBaseline3 = data.frame(CCPEBaseline3MeanCenter1, CCPEBaseline3$CIG30D, CCPEBaseline3$MJ30D, CCPEBaseline3$BINGE530D, CCPEBaseline3$R_WHITE_N, CCPEBaseline3$SEX_PR, CCPEBaseline3$GENDER)
CCPEBaseline3
colnames(CCPEBaseline3) = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "AGE", "REL_IMP", "CIG30D", "MJ30D","BINGE530D", "R_WHITE_N", "SEX_PR", "GENDER")

CCPEBaseline4MeanCenter1 = data.frame(CCPEBaseline4$RSKCIG, CCPEBaseline4$RSKMJ, CCPEBaseline4$RSKALC, CCPEBaseline4$INCOME, CCPEBaseline4$AGE, CCPEBaseline4$REL_IMP)
CCPEBaseline4MeanCenter1 = scale(CCPEBaseline4MeanCenter1, scale = FALSE)
CCPEBaseline4 = data.frame(CCPEBaseline4MeanCenter1, CCPEBaseline4$CIG30D, CCPEBaseline4$MJ30D, CCPEBaseline4$BINGE530D, CCPEBaseline4$R_WHITE_N, CCPEBaseline4$SEX_PR, CCPEBaseline4$GENDER)
CCPEBaseline4
colnames(CCPEBaseline4) = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "AGE", "REL_IMP", "CIG30D", "MJ30D","BINGE530D", "R_WHITE_N", "SEX_PR", "GENDER")

CCPEBaseline5MeanCenter1 = data.frame(CCPEBaseline5$RSKCIG, CCPEBaseline5$RSKMJ, CCPEBaseline5$RSKALC, CCPEBaseline5$INCOME, CCPEBaseline5$AGE, CCPEBaseline5$REL_IMP)
CCPEBaseline5MeanCenter1 = scale(CCPEBaseline5MeanCenter1, scale = FALSE)
CCPEBaseline5 = data.frame(CCPEBaseline5MeanCenter1, CCPEBaseline5$CIG30D, CCPEBaseline5$MJ30D, CCPEBaseline5$BINGE530D, CCPEBaseline5$R_WHITE_N, CCPEBaseline5$SEX_PR, CCPEBaseline5$GENDER)
CCPEBaseline5
colnames(CCPEBaseline5) = c("RSKCIG", "RSKMJ", "RSKALC", "INCOME", "AGE", "REL_IMP", "CIG30D", "MJ30D","BINGE530D", "R_WHITE_N", "SEX_PR", "GENDER")

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


Cig Models Comparison with final with listwise deletion
```{r}
#cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

cig1 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline1)
summary(cig1)

cig2 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline2)
summary(cig2)

cig3 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline3)
summary(cig3)

cig4 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline4)
summary(cig4)

cig5 = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline5)
summary(cig5)

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
 
Mar Models Comparison with final with listwise deletion
```{r}
#mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

mar1 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterINCOME + CenterRSKMJ*CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline1)
summary(mar1)

mar2 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterINCOME + CenterRSKMJ*CenterGENDER+ AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline2)
summary(mar2)

mar3 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterINCOME + CenterRSKMJ*CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline3)
summary(mar3)

mar4 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterINCOME + CenterRSKMJ*CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline4)
summary(mar4)

mar5 = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ*CenterINCOME + CenterRSKMJ*CenterGENDER + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline5)
summary(mar5)

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
summary(mar)
```
Alcohol Models Comparison with final with listwise deletion
```{r}
#alcohol = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N + RSKALC*REL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(alcohol)

alcohol1 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline1)
summary(alcohol1)

alcohol2 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline2)
summary(alcohol2)

alcohol3 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline3)
summary(alcohol3)

alcohol4 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline4)
summary(alcohol4)

alcohol5 = glm.nb(BINGE530D ~ RSKALC + R_WHITE_N  + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline5)
summary(alcohol5)

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
summary(alcohol)
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

