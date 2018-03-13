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

setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/CCPEPaperData")
gpraAdultBase = read.csv("CCPEBaseline.csv", header = TRUE)



#CCPEBaseline = data.frame(apply(gpraAdultBase, 2, function(x)(ifelse(x == 97, NA, ifelse(x == 98, NA, ifelse(x == 99, 0,x))))))


#CCPEBaseline = data.frame(CCPEBaseline$INTERVENTION_A, CCPEBaseline$RSKCIG, CCPEBaseline$CIG30D, CCPEBaseline$MJ30D, CCPEBaseline$RSKMJ, CCPEBaseline$BINGE530D, CCPEBaseline$RSKALC, CCPEBaseline$R_WHITE_N, CCPEBaseline$REL_IMP, CCPEBaseline$HINCOMEO_N, CCPEBaseline$SEX_PR, CCPEBaseline$GENDER, CCPEBaseline$YOB)
#TestMCARNormality(data = CCPEBaseline)

# Here I am getting the number of people before imputation 
dim(CCPEBaseline)
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)


#colnames(CCPEBaseline) = c("INTERVENTION_A", "RSKCIG", "CIG30D", "MJ30D", "RSKMJ", "BINGE530D", "RSKALC", "R_WHITE_N", "REL_IMP", "INCOME", "SEX_PR", "GENDER", "YOB")

## Get correlations
#CCPEBaselineCor= data.frame(cor(CCPEBaseline))
#CCPEBaselineCor = round(CCPEBaselineCor, 3)
#write.csv(CCPEBaselineCor, "CCPEBaselineCor.csv")

592/745
```
Drop anyone who is not male or female.  So subset the data where gender equals 1 or 2
Lose 3 total people
```{r}
write.csv(CCPEBaseline, "CCPEBaseline.csv", row.names = FALSE)
CCPEBaseline = read.csv("CCPEBaseline.csv", header = TRUE)
CCPEBaseline =subset(CCPEBaseline, GENDER == 1 | GENDER == 2)
dim(CCPEBaseline)

CCPEBaseline$GENDER = ifelse(CCPEBaseline$GENDER == 1,1,0)


```
Get percentage in progrma 62 means SIS, 1 means CTR, and 51 is respect
```{r}
propIntervention = CCPEBaseline %>%
  count(INTERVENTION_A) %>%
  mutate(prop = prop.table(n))
propIntervention = round(propIntervention, 3)
propIntervention
CCPEBaseline$INTERVENTION_A = NULL
0.032	+.184
```


Now change AGE to AGE by subtracting 2018
```{r}
CCPEBaseline$AGE = 2018-CCPEBaseline$YOB
```
Change home income to split on something $30,000 or lower is low income.
Change sex orientation to straight or non-straight
```{r}

CCPEBaseline$INCOME = ifelse(CCPEBaseline$INCOME == 1, 0, ifelse(CCPEBaseline$INCOME == 2, 0, 1))
CCPEBaseline$SEX_PR = ifelse(CCPEBaseline$SEX_PR ==1, 1, 0)


```

Now we need to mean center all ordinal and continuous variables
#
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
Graph model for riskcig and religion.  Need to have the centered values and then get rid of regular rel and risk, because the centered version will automatically be included, but ok, because this is just for plot.
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
mar = glm.nb(MJ30D ~ RSKMJ + R_WHITE_N + CenterRSKMJ_CenterINCOME + CenterRSKMJ_CenterGENDER +  AGE  + REL_IMP + INCOME + SEX_PR+ GENDER , data = CCPEBaseline)
summary(mar)

```
Interaction graph for Mar model 

```{r}
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
CCPEBaselineMarin = CCPEBaseline

CCPEBaselineMarin$INCOME = factor(CCPEBaselineMarin$INCOME, level =c(1,0), labels = c("High", "Low"))
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


Mar.SD$INCOME<-factor(Mar.SD$INCOME,labels=c("High", "Low"))

Plot.MarIncome<-ggplot(data=Mar.SD, aes(x=CenterRSKMJ, y=fit, group=INCOME))+
  coord_cartesian(ylim = c(0,8))+  
  #For ylim, specify the range of your DV (in our case, 0-4)
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
CCPEBaseline = data.frame(na.omit(CCPEBaseline))
dim(CCPEBaseline)
CCPEBaselineMarin = CCPEBaseline

CCPEBaselineMarin$GENDER = factor(CCPEBaselineMarin$GENDER, level =c(1,0), labels = c("Male", "Female"))
mar = glm.nb(MJ30D ~ R_WHITE_N + CenterRSKMJ*GENDER + CenterRSKMJ_CenterGENDER +  AGE  + REL_IMP + SEX_PR+ GENDER , data = CCPEBaselineMarin)
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

MarGender.SD $CenterRSKMJ<-factor(MarGender.SD $CenterRSKMJ,
                           levels=c(-0.864, 0.000,  0.864),
                           labels=c("1 SD Below Mean", "Mean", "1 SD Above Mean"))


MarGender.SD $GENDER<-factor(MarGender.SD $GENDER,labels=c("Male", "Female"))

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
Now get the comparison to the national statistics.  So change RISKCIG, RISKMJ, and BINGE30 to 1 and 0's with any value above you being 1 and then get the mean and compare to national statistics.
```{r}
nationStats = data.frame(CCPEBaseline$CIG30D,CCPEBaseline$MJ30D, CCPEBaseline$BINGE530D) 
colnames(nationStats) = c("RSKCIG", "RSKMJ", "BINGE530D")
nationStats = na.omit(nationStats)
head(nationStats)
nationStats = data.frame(apply(nationStats, 2, function(x)(ifelse(x >0, 1, 0))))
head(nationStats)
apply(nationStats, 2, mean)

```



