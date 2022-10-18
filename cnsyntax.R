################################################################################
####clear everything.
rm(list=ls())

#### package check
pkg <- c("car", "psych", "ggplot2", "ggridges", "estimatr", "summarytools", 
         "tidyverse", "sandwich", "interactions", "patchwork", "flextable",  
         "dotwhisker", "hrbrthemes", "huxtable", "haven", "lmtest")
new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg)) install.packages(new.pkg)

####packages.
library(car)
library(psych)
library(ggplot2)
library(ggridges)
library(estimatr)
library(expss)
library(summarytools)
library(tidyverse)
library(sandwich)
library(interactions)
library(patchwork)
library(dotwhisker)
library(hrbrthemes)
library(huxtable)
library(haven)
library(lmtest)

################################################################################
###### functions 

# 0-1 recoding function
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}
rsumm<-function(x){
  descr(x, stats='common', transpose=T)
}
tab<-function(x){
  freq(as_factor(x))
}

################################################################################

##### load data from spss
data<-read_sav("cndata.sav")

##### load Rdata if coded and saved previously
load("elec2020.RData")

################################################################################
######## coding

# age
data$age<-as.numeric(data$Q16.0)
data$age<-ifelse(data$age<150, data$age, NA)

# male gender
data$male<-ifelse(data$Q20==1, 1, 0)

# recoded income
data$rinc<-std01(data$Q17)

# education, ordinal
data$reduc<-std01(data$Q25)

# education, college degree
data$college<-factor(ifelse(data$Q25>4, 1, 0))

# race indicators
data$race<-NA
data$race<-replace(data$race, data$Q18_1==1, 1)
data$race<-replace(data$race, data$Q18_2==1, 2)
data$race<-replace(data$race, data$Q18_3==1, 3)
data$race<-replace(data$race, data$Q18_4==1, 4)
data$race<-replace(data$race, (data$Q18_5==1 | data$Q18_6==1), 6)
data$race<-replace(data$race, data$Q19==1, 5)
var_lab(data$race) <- "Recoded Race"
val_lab(data$race) <- num_lab("1 Black
                               2 Asian Am
                               3 Native Am
                               4 White
                               5 Hispanic
                               6 Mixed/Other")

data$white<-ifelse(data$race==4, 1, 0)
data$black<-ifelse(data$race==1, 1, 0)
data$hisp<-ifelse(data$race==5, 1, 0)

# ideology
data$rideo<-std01(data$Q6)

# pid
data$rpid<-std01(data$Q5)

# 3-category pid
data$pid3 <- car::recode(data$Q5, "1:3=1; 4=2; 5:7=3")
var_lab(data$pid3) <- "Recoded PID (3 pt)"
val_lab(data$pid3) <- num_lab("1 D/Lean D
                                2 Ind
                                3 R/Lean R")
data$pid3<-factor(data$pid3)
freq(data$pid3)

# pid indicators
data$dem<-factor(ifelse(data$pid3=="D/Lean D", 1, 0))
data$rep<-factor(ifelse(data$pid3=="R/Lean R", 1, 0))


# CN
data$rcn1<-(data$CN_Satisfaction_1-1)/6
data$rcn2<-(data$CN_Satisfaction_2-1)/6
data$rcn3<-(data$CN_Satisfaction_3-1)/6
data$rcn4<-(data$CN_Satisfaction_4-1)/6
data$rcn5<-(data$CN_Satisfaction_5-1)/6
psych::alpha(with(data, cbind(rcn1, rcn2, rcn3, rcn4, rcn5)))
data$rcnsc<-rowMeans(with(data, cbind(rcn1, rcn2, rcn3, rcn4, rcn5)))

# IS
data$ris1<-(data$CN_Satisfaction_6-1)/6
data$ris2<-(data$CN_Satisfaction_7-1)/6
data$ris3<-(data$CN_Satisfaction_8-1)/6
data$ris4<-(data$CN_Satisfaction_9-1)/6
psych::alpha(with(data, cbind(ris1, ris2, ris3, ris4)))
data$rissc<-rowMeans(with(data, cbind(ris1, ris2, ris3, ris4)))

# CoBRAS
data$rcob1<-std01(data$Instructions_1)
data$rcob2<-1-std01(data$Instructions_2)
data$rcob3<-std01(data$Instructions_3)
data$rcob4<-1-std01(data$Instructions_4)
data$rcob5<-1-std01(data$Instructions_5)
data$rcob6<-1-std01(data$Instructions_6)
data$rcob7<-std01(data$Instructions_7)
data$rcob8<-1-std01(data$Instructions_8)
data$rcob9<-std01(data$Instructions_9)
data$rcob10<-std01(data$Instructions_10)
data$rcob11<-1-std01(data$Instructions_11)
data$rcob12<-1-std01(data$Instructions_12)
data$rcob13<-std01(data$Instructions_13)
data$rcob14<-std01(data$Instructions_14)
data$rcob15<-1-std01(data$Instructions_15)
data$rcob16<-std01(data$Instructions_16)
data$rcob17<-1-std01(data$Instructions_17)
data$rcob18<-std01(data$Instructions_18)
data$rcob19<-std01(data$Instructions_19)
data$rcob20<-1-std01(data$Instructions_20)

psych::alpha(with(data, cbind(rcob1, rcob2, rcob3, rcob4, rcob5, rcob6, rcob7,
                              rcob8, rcob9, rcob10, rcob11, rcob12, rcob13, 
                              rcob14, rcob15, rcob16, rcob17, rcob18, rcob19,
                              rcob20)))
psych::alpha(with(data, cbind(rcob1, rcob2, rcob6, rcob8, rcob12, 
                              rcob15, rcob20)))
psych::alpha(with(data, cbind(rcob3, rcob4, rcob9, rcob13, rcob14, 
                              rcob16, rcob18)))
psych::alpha(with(data, cbind(rcob5, rcob7, rcob10, rcob11, rcob17, 
                              rcob19)))

data$rcobt<-rowMeans(with(data, cbind(rcob1, rcob2, rcob3, rcob4, rcob5, rcob6, 
                                      rcob7, rcob8, rcob9, rcob10, rcob11, 
                                      rcob12, rcob13, rcob14, rcob15, rcob16, 
                                      rcob17, rcob18, rcob19, rcob20)))
data$rupr<-rowMeans(with(data, cbind(rcob1, rcob2, rcob6, rcob8, rcob12, 
                                     rcob15, rcob20)))
data$ruid<-rowMeans(with(data, cbind(rcob3, rcob4, rcob9, rcob13, rcob14, 
                                     rcob16, rcob18)))
data$rbri<-rowMeans(with(data, cbind(rcob5, rcob7, rcob10, rcob11, rcob17, 
                                     rcob19)))

# group differences in racial fairness (high = whites worse off)
data$rf1<-(data$Q10-1)/2
data$rf2<-(data$Q11-1)/2
data$rf3<-(data$Q12-1)/2
data$rf4<-(data$Q13-1)/2
data$rf5<-(data$Q14-1)/2
data$rf6<-(data$Q15-1)/2
data$rf7<-(data$Q16-1)/2
psych::alpha(with(data, cbind(rf1, rf2, rf3, rf4, rf5, rf6, rf7)))
data$rfsc<-rowMeans(with(data, cbind(rf1, rf2, rf3, rf4, rf5, rf6, rf7)))

# bias awareness (high = low awareness)
# item 4 -- poorly correlated with protrait items
data$rba1<-1-std01(data$Q28_1)
data$rba2<-1-std01(data$Q28_2)
data$rba3<-1-std01(data$Q28_3)
data$rba4<-std01(data$Q28_4)
psych::alpha(with(data, cbind(rba1, rba2, rba3, rba4)))
corr.test(with(data, cbind(rba1, rba2, rba3, rba4)))
data$rbasc1<-rowMeans(with(data, cbind(rba1, rba2, rba3, rba4)))
data$rbasc2<-rowMeans(with(data, cbind(rba1, rba2, rba3)))

# opposition to diversity education
# 1st item on CRT not well correlated with others (confusing wording?)
data$rde1<-(5-data$Q17_1)/4
data$rde2<-(data$Q17_2-1)/4
data$rde3<-(data$Q17_3-1)/4
data$rde4<-(data$Q17_4-1)/4
psych::alpha(with(data, cbind(rde1, rde2, rde3, rde4)))
data$rdesc1<-rowMeans(with(data, cbind(rde1, rde2, rde3, rde4)))
data$rdesc2<-rowMeans(with(data, cbind(rde2, rde3, rde4)))

# American exceptionalism
# item 8 poorly correlated with protrait items
data$rae1<-std01(data$Q29_1)
data$rae2<-std01(data$Q29_2)
data$rae3<-std01(data$Q29_3)
data$rae4<-std01(data$Q29_4)
data$rae5<-std01(data$Q29_5)
data$rae6<-std01(data$Q29_6)
data$rae7<-std01(data$Q29_7)
data$rae8<-1-std01(data$Q29_8)
data$rae9<-std01(data$Q29_9)
data$rae10<-std01(data$Q29_10)
psych::alpha(with(data, cbind(rae1, rae2, rae3, rae4, rae5, rae6, rae7,
                              rae8, rae9, rae10)))
corr.test(with(data, cbind(rae1, rae2, rae3, rae4, rae5, rae6, rae7,
                              rae8, rae9, rae10)))
data$raesc1<-rowMeans(with(data, cbind(rae1, rae2, rae3, rae4, rae5, rae6, rae7,
                              rae8, rae9, rae10)))
data$raesc2<-rowMeans(with(data, cbind(rae1, rae2, rae3, rae4, rae5, rae6, rae7,
                                       rae9, rae10)))

# replacement theory
data$rrpl1<-(data$Q34-1)/4
data$rrpl2<-(data$Q35-1)/4
data$rrpl3<-(data$Q36-1)/4
data$rrpl4<-(data$Q37-1)/4
psych::alpha(with(data, cbind(rrpl1, rrpl2, rrpl3, rrpl4)))
corr.test(with(data, cbind(rrpl1, rrpl2, rrpl3, rrpl4)))
data$rrplsc<-rowMeans(with(data, cbind(rrpl1, rrpl2, rrpl3, rrpl4)))

# white christian nationalism
data$rwcn1<-(data$Q39-1)/4
data$rwcn2<-(data$Q40-1)/4
psych::alpha(with(data, cbind(rwcn1, rwcn2)))
corr.test(with(data, cbind(rwcn1, rwcn2)))
data$rwcnsc<-rowMeans(with(data, cbind(rwcn1, rwcn2)))

# characteristics of a 'true' American
# need to factor analyze, possibly ipsatize
data$abrnus<-(4-data$Q42_1)/3
data$axtian<-(4-data$Q42_2)/3
data$aeurop<-(4-data$Q42_3)/3
data$awhite<-(4-data$Q42_4)/3
data$ainstt<-(4-data$Q42_5)/3
data$athink<-(4-data$Q42_6)/3
data$afeela<-(4-data$Q42_7)/3
data$acitiz<-(4-data$Q42_8)/3

# factor analysis
items<-data[c("abrnus", "axtian", "aeurop", "awhite", "ainstt", "athink",
              "afeela", "acitiz")]

# KMO (must be over 0.50) and Bartlett sphericity (must be p<0.05)
items<-na.omit(items)
r<- cor(items)
KMO(r)
cortest.bartlett(r, n = 979)

# parallel analysis with PCA (two factors)
fa.parallel(items, fa="pc", n.iter=50)
fact <- fa(items, nfactors=2,  fm="pa", rotate = "oblimin")
summary(fact)
print(fact)

# cross-loads --> abrnus
# exclusive --> axtian, aeurop, awhite    
# civic --> ainstt, athink, afeela, acitiz

# scales:
psych::alpha(with(data, cbind(axtian, aeurop, awhite)))
psych::alpha(with(data, cbind(ainstt, athink, afeela, acitiz)))
data$eid<-rowMeans(with(data, cbind(axtian, aeurop, awhite)))
data$cid<-rowMeans(with(data, cbind(ainstt, athink, afeela, acitiz)))

##### retain check-passed cases only
df <- data %>% dplyr::filter(Q64==1 & data$Q66_1==1 & data$Q66_2==1) 

# alphas for check-passed cases only
psych::alpha(with(df, cbind(rcn1, rcn2, rcn3, rcn4, rcn5)))
psych::alpha(with(df, cbind(ris1, ris2, ris3, ris4)))
psych::alpha(with(df, cbind(rcob1, rcob2, rcob3, rcob4, rcob5, rcob6, rcob7,
                            rcob8, rcob9, rcob10, rcob11, rcob12, rcob13, 
                            rcob14, rcob15, rcob16, rcob17, rcob18, rcob19,
                            rcob20)))
psych::alpha(with(df, cbind(rcob1, rcob2, rcob6, rcob8, rcob12, 
                            rcob15, rcob20)))
psych::alpha(with(df, cbind(rcob3, rcob4, rcob9, rcob13, rcob14, 
                            rcob16, rcob18)))
psych::alpha(with(df, cbind(rcob5, rcob7, rcob10, rcob11, rcob17, 
                            rcob19)))
psych::alpha(with(df, cbind(rf1, rf2, rf3, rf4, rf5, rf6, rf7)))
psych::alpha(with(df, cbind(rba1, rba2, rba3, rba4)))
psych::alpha(with(df, cbind(rde1, rde2, rde3, rde4)))
psych::alpha(with(df, cbind(rae1, rae2, rae3, rae4, rae5, rae6, rae7,
                            rae8, rae9, rae10)))
psych::alpha(with(df, cbind(rrpl1, rrpl2, rrpl3, rrpl4)))
psych::alpha(with(df, cbind(rwcn1, rwcn2)))
psych::alpha(with(df, cbind(axtian, aeurop, awhite)))
psych::alpha(with(df, cbind(ainstt, athink, afeela, acitiz)))

# factor analysis for identity items is the same for checked cases
items2<-df[c("abrnus", "axtian", "aeurop", "awhite", "ainstt", "athink",
              "afeela", "acitiz")]
fa.parallel(items2, fa="pc", n.iter=50)
fact2 <- fa(items2, nfactors=2,  fm="pa", rotate = "oblimin")
summary(fact2)
print(fact2)

##### save dataframes as Rdata
save(data, file="cndata1.Rdata")
save(df, file="cndata2.Rdata")

################################################################################
######### analysis: full data

###### racial attitudes
corr.test(with(data, cbind(rcnsc, rissc, rideo, rcobt, rupr, ruid, rbri, rfsc, 
                           rbasc1, rbasc2, rdesc1, rdesc2)))

### cobras: full (as expected)
r1<-lm_robust(rcobt ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=data, se_type="HC3")
summary(r1)

### cobras: subscales (NS for privilege; expected for others)
r1a<-lm_robust(rupr ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r1a)
r1b<-lm_robust(ruid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r1b)
r1c<-lm_robust(rbri ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r1c)

### comparative racial fairness (as expected)
r3<-lm_robust(rfsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=data, se_type="HC3")
summary(r3)

### bias awareness: interesting, high CN is associated with more BA
r4a<-lm_robust(rbasc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r4a)
r4b<-lm_robust(rbasc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r4b)

### opposition to diversity education (as expected)
r5a<-lm_robust(rdesc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r5a)
r5b<-lm_robust(rdesc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r5b)

###### american exceptionalism
r6a<-lm_robust(raesc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r6a)
r6b<-lm_robust(raesc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r6b)

###### exclusive identity

### great replacement
r7<-lm_robust(rrplsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=data, se_type="HC3")
summary(r7)

### white christian nationalism
r8<-lm_robust(rwcnsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=data, se_type="HC3")
summary(r8)

### exclusive american identity
r9<-lm_robust(eid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=data, se_type="HC3")
summary(r9)

### civic american identity
r10<-lm_robust(cid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=data, se_type="HC3")
summary(r10)

################################################################################
######### analysis: full data -- checked

###### racial attitudes
corr.test(with(df, cbind(rcnsc, rissc, rideo, rcobt, rupr, ruid, rbri, rfsc, 
                         rbasc1, rbasc2, rdesc1, rdesc2)))

### cobras: full (as expected)
r1<-lm_robust(rcobt ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=df, se_type="HC3")
summary(r1)

### cobras: subscales (reverse for privilege; expected for others)
r1a<-lm_robust(rupr ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r1a)
r1b<-lm_robust(ruid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r1b)
r1c<-lm_robust(rbri ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r1c)

### comparative racial fairness (as expected)
r3<-lm_robust(rfsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=df, se_type="HC3")
summary(r3)

### bias awareness: interesting, high CN is associated with more BA
r4a<-lm_robust(rbasc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r4a)
r4b<-lm_robust(rbasc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r4b)

### opposition to diversity education (as expected)
r5a<-lm_robust(rdesc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r5a)
r5b<-lm_robust(rdesc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r5b)

###### american exceptionalism
r6a<-lm_robust(raesc1 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r6a)
r6b<-lm_robust(raesc2 ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r6b)

###### exclusive identity

### great replacement
r7<-lm_robust(rrplsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=df, se_type="HC3")
summary(r7)

### white christian nationalism
r8<-lm_robust(rwcnsc ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=df, se_type="HC3")
summary(r8)

### exclusive american identity
r9<-lm_robust(eid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                rcnsc+rissc, data=df, se_type="HC3")
summary(r9)

### civic american identity
r10<-lm_robust(cid ~ male+age+rinc+reduc+white+black+rideo+dem+rep+
                 rcnsc+rissc, data=df, se_type="HC3")
summary(r10)

################################################################################
######### white respondents.

###### all 

### cobras: full (as expected)
r1<-lm_robust(rcobt ~ male+age+rinc+reduc+rideo+dem+rep+
                rcnsc+rissc, data=subset(data, white==1), se_type="HC3")
summary(r1)

### cobras: subscales (reverse for privilege; expected for others)
r1a<-lm_robust(rupr ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(data, white==1), se_type="HC3")
summary(r1a)
r1b<-lm_robust(ruid ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(data, white==1), se_type="HC3")
summary(r1b)
r1c<-lm_robust(rbri ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(data, white==1), se_type="HC3")
summary(r1c)

###### checked only

### cobras: full (as expected)
r1<-lm_robust(rcobt ~ male+age+rinc+reduc+rideo+dem+rep+
                rcnsc+rissc, data=subset(df, white==1), se_type="HC3")
summary(r1)

### cobras: subscales (reverse for privilege; expected for others)
r1a<-lm_robust(rupr ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(df, white==1), se_type="HC3")
summary(r1a)
r1b<-lm_robust(ruid ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(df, white==1), se_type="HC3")
summary(r1b)
r1c<-lm_robust(rbri ~ male+age+rinc+reduc+rideo+dem+rep+
                 rcnsc+rissc, data=subset(df, white==1), se_type="HC3")
summary(r1c)

################################################################################


## run to use hrbrthemes, follow instructions to install --
## sometimes need a restart of R after this to get fonts:
import_roboto_condensed()
