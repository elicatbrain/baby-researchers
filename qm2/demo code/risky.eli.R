

#Step 1: get frequencies of SYS to get the low and high cutoffs. Calculate 1sd 
# below the mean for low score (14.66 and 25.34). 
#Step 2: center SYS at the score 1sd below the mean, after checking that it’s 
# really in the dataset range, which it is. SYS_1sdlow 
#Step 3: calculate new interaction term for the low cutoff score of the 
# moderator and the other IV predicting the DV. 
#Step 4: run a regression model with DV = zsertot = predictors DERS_total + 
#  SYS_1sdlow + ders_lowsys and a 
#  second regression model with DV = zsertot = predictors DERS_total + 
#  SYS_1sdhigh + ders_highsys
#Step 5: check if the hypotheses were correct 

#Hypotheses:
  #those with high emotional reactivity will show a strong relation 
  #between DERS and risky sex (zsertot). 
  #and
  #those with low emotional reactivity will show a weak (non-significant) 
  #relation between DERS and zsertot 


setwd("C:/Users/Elizabeth Bell/OneDrive - Southern Methodist University/Desktop/baby-researchers/qm2/datasets")

library(foreign)
library(readr)
library(haven)

risk <- read_csv("Ale.risky.sex.csv")


risk.eli <- Ale_risky_sex 

summary(risk$SYS)
risk$SYS <- as.numeric(risk$SYS)
sd(SYS, na.rm=TRUE)

summary(risk.eli$DERS_total)

risk.eli$SYSlow <- risk.eli$SYS -14.66
risk.eli$SYhigh <- risk.eli$SYS -25.34



risk.eli$low_ders <- risk.eli$SYSlow*risk.eli$DERS_total
risk.eli$high_ders <- risk.eli$SYShigh*risk.eli$DERS_total



model1 <- lm(risk.eli$zsertot ~ risk.eli$DERS_total + 
               risk.eli$SYSlow +  risk.eli$low_ders, na.rm=TRUE)

model2 <- lm(risk.eli$zsertot ~ risk.eli$DERS_total + 
               risk.eli$SYShigh +  risk.eli$high_ders,  na.rm=TRUE)



