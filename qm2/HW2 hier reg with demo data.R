
#In the attached file, we want to know how much of the total variance in 
  #smoking (cigsday) is predicted by mood over and above 
  #demographics (age and gender). 
#Mood is represented by 3 variable: positive mood [PANAS_pa], 
  #negative mood [PANAS_na], and anxious arousal [masq_aa]. 
#Do a hierarchical regression to figure out how much variance is 
  #accounted for by the 3 mood variables together over and above gender and age. 
#Also, in the final regression, how much variance in cigsday is explained 
  #by each mood variable over and above all the other variables? 
#How much variance in cigsday is explained by the overlap of the 3 
  #mood variables?





library(foreign)
library(readr)

write_csv(adaa, "ADAA_moderated_mediation.sav", na="NA")

install.packages("nlme")
library(nlme)

#STEP 0 
#impute data 
#count missing values per variable 
library(dplyr)
sum(is.na(ADAA_moderated_mediation$gender))
sum(is.na(ADAA_moderated_mediation$age))
sum(is.na(ADAA_moderated_mediation$panas_pa))
sum(is.na(ADAA_moderated_mediation$panas_na))
sum(is.na(ADAA_moderated_mediation$masq_aa))


  #mean and quartiles, and NAs 
summary(ADAA_moderated_mediation$panas_na, na.rm=TRUE)
summary(ADAA_moderated_mediation$panas_pa, na.rm=TRUE)
summary(ADAA_moderated_mediation$masq_aa, na.rm=TRUE)

#new data frame so  the imputed vars aren't replacing the raw data 
adaa_full <- ADAA_moderated_mediation

adaa_full$panas_na[is.na(ADAA_moderated_mediation$panas_na)] <- mean(
  ADAA_moderated_mediation$panas_na, na.rm=TRUE)

adaa_full$panas_pa[is.na(ADAA_moderated_mediation$panas_pa)] <- mean(
  ADAA_moderated_mediation$panas_pa, na.rm=TRUE)

adaa_full$masq_aa[is.na(ADAA_moderated_mediation$masq_aa)] <- mean(
  ADAA_moderated_mediation$masq_aa, na.rm=TRUE)


sum(is.na(adaa_full$panas_na))
sum(is.na(adaa_full$panas_pa))
sum(is.na(adaa_full$masq_aa))


summary(adaa_full$panas_na)
summary(adaa_full$panas_pa)
summary(adaa_full$masq_aa)





# Step 1: Run the first level of the hierarchy
model1 <- lm(adaa_full$cigsday ~ adaa_full$gender 
             + adaa_full$age, na.action = "na.omit")
summary(model1)

# Step 2: Run the second level of the hierarchy
model2 <- lm(adaa_full$cigsday ~ adaa_full$gender 
             + adaa_full$age + adaa_full$panas_pa
             + adaa_full$panas_na 
             + adaa_full$masq_aa,  na.action="na.omit")
summary(model2)




# Step 3: Compare the two models using an ANOVA test
anova(model1, model2)


# Step 4: Get the R-squared for each model
rsq1 <- summary(model1)$r.squared
rsq2 <- summary(model2)$r.squared
rsq1
rsq2

# Step 5: Calculate the change in R-squared
rsq_change <- rsq2 - rsq1
rsq_change

# Step 6: Print the change in R-squared
print(rsq_change)

# Step 7: Get the zero-order correlations between y and the predictor variables
class(adaa_full$gender)
adaa_full$gender <- as.factor(adaa_full$gender)

hr_matrix1 <- select(adaa_full, cigsday, gender, age, panas_na, panas_pa, masq_aa)
hr_matrix1
(hr_matrix1)

cor1 <- round(cor(hr_matrix1, use = "complete.obs"), 4)
cor1



# Step 8: Get the partial correlations between y and the predictor variables
# controlling for the variables in the first level of the hierarchy
model1_residuals <- residuals(model1)
cor_y_x3_partial <- cor(model1_residuals, mydata$x3)
cor_y_x4_partial <- cor(model1_residuals, mydata$x4)

# Step 9: Get the part correlations between y and the predictor variables
# controlling for the variables in both levels of the hierarchy
model2_residuals <- residuals(model2)
cor_y_x3_part <- cor(model2_residuals, mydata$x3)
cor_y_x4_part <- cor(model2_residuals, mydata$x4)




