# qm 2 hw 5 - categorical dummy coded vars, one-way anova, and regression model
#use race to predict third grade math achievement score 
#

#import from baby-researchers 



# Create dummy variables, 
  # including reference groups solely to have consistent var names

Race3Math$race_white <- ifelse(Race3Math$race == "1", 1, 0)
Race3Math$race_black <- ifelse(Race3Math$race == "2", 1, 0)
Race3Math$race_hispS <- ifelse(Race3Math$race == "3", 1, 0)
Race3Math$race_hispN <- ifelse(Race3Math$race == "4", 1, 0)



