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

```
Just loading the data and getting rid of missing values.  Getting fid of missing data here and calculating the percentage of missing data.
```{r}

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/CCPEPaperData")
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE)

dim(CCPEBaseline)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)


592/745
```
Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people.  1 equals male and 2 equals female.  Need to read and write the dataset to get the variables to be factors.  Also, changing gender to be 1 for male and 0 for female.
```{r}
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE)

CCPEBaseline =subset(CCPEBaseline, GENDER == 1 | GENDER == 2)
dim(CCPEBaseline)

CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER == 1,1,0)

```
Now change AGE to AGE by subtracting 2018 from YOB.  
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
```
Change home income to split on something 30,000 or lower is low income.  We choose $30,000 because it was in the middle of the scale for the GPRA.  Ok so 1 and 2 are 30,000 and below so they are 0 and everything else is 1, because options 3,4,5 and higher than 30,000.

Change sex orientation to straight or non-straight where is straight and zero is non-straight
```{r}

CCPEBaseline$INCOME = ifelse(CCPEBaseline$INCOME == 1, 0, ifelse(CCPEBaseline$INCOME == 2, 0, 1))
CCPEBaseline
CCPEBaseline$SEX_PR = ifelse(CCPEBaseline$SEX_PR ==1, 1, 0)

```

Get the percentage of females and males smoking marjiauana.  This was check that the previous graphs were incorrect and that more men smoked marijuana than women in the sample.
Create two seperate data sets one for males 1 and females 0.  Turn the variable into a factor so you can subset.  Then get the mean for males and females of the number of times they smoked marijuana in the last 30 days.
```{r}
percentGender = data.frame(GENDER=CCPEBaseline$GENDER, MJ30D=CCPEBaseline$MJ30D)
percentGender$GENDER = factor(percentGender$GENDER)
percentGenderMale= subset(percentGender, GENDER == 1)
averageMarMale = mean(percentGenderMale$MJ30D)
averageMarMale

percentGenderFemale = subset(percentGender, GENDER == 0)
averageMarFemale = mean(percentGenderFemale$MJ30D)
averageMarFemale
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

summary(CCPEBaseline)
dim(CCPEBaseline)[1]


```
Cig model looking for interactions.  I looked for interactions one at time, because the model ran out of degrees of freedom or wouldn't run (not entirly sure, but it would run) with all the interaction terms included so looked at them one at a time.  
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
Testing to make sure that when I create the interaction effect by combining variables and then including them they are not different from just measuring as an interaction effect.
```{r}
# Final model no impute
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ RSKCIG + R_WHITE_N + CenterRSKCIG_CenterREL_IMP + AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

cigTest = glm.nb(CIG30D ~  R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cigTest)
```
Graph model for riskcig and religion.  Need to have the centered values and then get rid of regular rel and risk, because the centered version will automatically be included, but ok, because the results are the same.

Use this website as model: http://ademos.people.uic.edu/Chapter13.html  

Need to grab the standard deviation for risk and religion, because we want to plot the interaction at sd-1, mean, sd+1 for both variables for the predicited values of cigarettes smoked.

Then plug them into the effect function with the levels for each variable, which are the sd-1, mean, sd+1 for each variable.  The effect you are interested in the interaction effect.  You plug in the model to predict cigarettes smoked at these levels of each variable.

Then turn the sd's into factors so when you plot them they show up as the words not the numbers.
```{r}
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
cig = glm.nb(CIG30D ~ R_WHITE_N + CenterRSKCIG*CenterREL_IMP + AGE  + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(cig)

# Now graph it  first grab the means and +- 1 sds for CenterRSKCIG and CenterREL_IMP
CenterRSKCIG.SD <- c(mean(CCPEBaseline$CenterRSKCIG)-sd(CCPEBaseline$CenterRSKCIG),
                     mean(CCPEBaseline$CenterRSKCIG),
                     mean(CCPEBaseline$CenterRSKCIG)+sd(CCPEBaseline$CenterRSKCIG))
CenterRSKCIG.SD <- round(CenterRSKCIG.SD, 3)
CenterRSKCIG.SD

CenterREL_IMP.SD <- c(mean(CCPEBaseline$CenterREL_IMP)-sd(CCPEBaseline$CenterREL_IMP),
                     mean(CCPEBaseline$CenterREL_IMP),
                     mean(CCPEBaseline$CenterREL_IMP)+sd(CCPEBaseline$CenterREL_IMP))
CenterREL_IMP.SD <- round(CenterREL_IMP.SD, 3)
CenterREL_IMP.SD

Cig.SD <- effect(c("CenterRSKCIG*CenterREL_IMP"), cig,
                     xlevels=list(CenterRSKCIG=c(-0.5,  0.0,  0.5),
                                  CenterREL_IMP=c(-1.054, 0.000, 1.054))) 
# put data in data frame 
Cig.SD <- as.data.frame(Cig.SD)
Cig.SD

Cig.SD$CenterRSKCIG<-factor(Cig.SD$CenterRSKCIG,
                      levels=c(-0.5,  0.0,  0.5),
                      labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

Cig.SD$CenterREL_IMP<-factor(Cig.SD$CenterREL_IMP,
              levels=c(-1.054, 0.000, 1.054),
              labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))

Plot.SD<-ggplot(data=Cig.SD, aes(x=CenterRSKCIG, y=fit, group=CenterREL_IMP))+
  geom_line(size=2, aes(color=CenterREL_IMP))+ #Can adjust the thickness of your lines
  ylim(0,8)+ #Puts a limit on the y-axis
  ylab("Predicted days smoked cigarettes")+ #Adds a label to the y-axis
  xlab("Centered perceived susceptibility of smoking cigarettes")+ #Adds a label to the x-axis
  scale_colour_discrete(name = "Religious Importance")+
  ggtitle("Interaction between perceived susceptibility of smoking cigarettes and religion")+ #Title
  theme_bw()+ #Removes the gray background 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.key = element_blank())+ #Removes the lines 
  scale_fill_grey()
Plot.SD

```

Mar model same process as with cigarettes.
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
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ_CenterGENDER +  AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

```
Interaction graph for Mar model 

Don't label the values before any regressions, because then it does always code what is code as the effect in the interaction effect.

When you are labeling the SD's, you need to look at the effect model and variable you want to code.  In the model it has the 0's first.  Since you are not specifcying which number (i.e. 1 or 0) get's which label it is taking the number, in this case 0 and labeling it the first name, which was high.  This is incorrect, so you need to put the first label as low, because the first number in the income column in the effect model is 0 and that is low.  This is the same concept for gender.
```{r}
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
CCPEBaselineMarin = CCPEBaseline

CCPEBaselineMarin$INCOME = factor(CCPEBaselineMarin$INCOME)
mar = glm.nb(MJ30D ~ R_WHITE_N + CenterRSKMJ*INCOME + CenterRSKMJ_CenterGENDER +  AGE  + REL_IMP + SEX_PR+ GENDER , data = CCPEBaselineMarin)

summary(mar)

# Now graph it  first grab the means and +- 1 sds for CenterRSKMJ and INCOME
CenterRSKMJ.SD <- c(mean(CCPEBaselineMarin$CenterRSKMJ)-sd(CCPEBaselineMarin$CenterRSKMJ),
                    mean(CCPEBaselineMarin$CenterRSKMJ),
                    mean(CCPEBaselineMarin$CenterRSKMJ)+sd(CCPEBaselineMarin$CenterRSKMJ))
CenterRSKMJ.SD <- round(CenterRSKMJ.SD, 3)
CenterRSKMJ.SD


Mar.SD <- effect(c("CenterRSKMJ*INCOME"), mar, xlevels=list(CenterRSKMJ=c(-0.864, 0.000,  0.864)), se = TRUE,   confidence.level=.95, typical = mean) 

# put data in data frame 
Mar.SD <- as.data.frame(Mar.SD)
Mar.SD

Mar.SD$CenterRSKMJ<-factor(Mar.SD$CenterRSKMJ,
                           levels=c(-0.864, 0.000,  0.864),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


Mar.SD$INCOME<-factor(Mar.SD$INCOME,labels=c("Low", "High"))

Plot.MarIncome<-ggplot(data=Mar.SD, aes(x=CenterRSKMJ, y=fit, group=INCOME))+
  coord_cartesian(ylim = c(0,8))+  
  #For ylim, specify the range of your DV
  geom_line(size=2, aes(color=INCOME))+
  ylab("Predicted days smoked marijuana")+
  xlab("Centered perceived susceptibility of smoking marijuana")+
  ggtitle("Interaction between perceived susceptibility of smoking marijuana and income")+
  scale_colour_discrete(name = "Income")+
  theme_bw()+ 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_fill_grey()
Plot.MarIncome


```

Graph for interaction of gender and mar
```{r}
CCPEBaselineMarin = CCPEBaseline
CCPEBaselineMarin$GENDER = factor(CCPEBaselineMarin$GENDER)
mar = glm.nb(MJ30D ~  R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ*GENDER +  AGE  + REL_IMP + INCOME + SEX_PR, data = CCPEBaselineMarin)

summary(mar)


# Now graph it  first grab the means and +- 1 sds for CenterRSKMJ and GENDER
CenterRSKMJ.SD <- c(mean(CCPEBaselineMarin$CenterRSKMJ)-sd(CCPEBaselineMarin$CenterRSKMJ),
                    mean(CCPEBaselineMarin$CenterRSKMJ),
                    mean(CCPEBaselineMarin$CenterRSKMJ)+sd(CCPEBaselineMarin$CenterRSKMJ))
CenterRSKMJ.SD <- round(CenterRSKMJ.SD, 3)
CenterRSKMJ.SD


MarGender.SD <- effect(c("CenterRSKMJ*GENDER"), mar, xlevels=list(CenterRSKMJ=c(-0.864, 0.000,  0.864)), se = TRUE,   confidence.level=.95, typical = mean) 

# put data in data frame 
MarGender.SD <- as.data.frame(MarGender.SD )
MarGender.SD 

MarGender.SD $CenterRSKMJ<-factor(MarGender.SD$CenterRSKMJ,
                           levels=c(-0.864, 0.000,  0.864),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


MarGender.SD$GENDER<-factor(MarGender.SD$GENDER,labels=c("Female", "Male"))

Plot.MarIncome<-ggplot(data=MarGender.SD , aes(x=CenterRSKMJ, y=fit, group=GENDER))+
  coord_cartesian(ylim = c(0,8))+  
  geom_line(size=2, aes(color=GENDER))+
 ylab("Predicted days smoked marijuana")+
  xlab("Centered perceived susceptibility of smoking marijuana")+
  ggtitle("Interaction between perceived susceptibility of smoking marijuana and gender")+
  scale_colour_discrete(name = "Gender")+
  theme_bw()+ 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_fill_grey()
Plot.MarIncome

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

```



