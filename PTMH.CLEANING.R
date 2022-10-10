#cleaning data 

#STEP 1 - set working directory

#read in data - create data frame from entire dataset
library(readr)

PTMH.00 <- read.csv("PTMH_data00.csv")
view(PTMH.00)

#STEP 2 - apply rhe count function from dplyr

install.packages("dplyr")
library(dplyr)

  #count function without pipes
count(PTMH.00, ID)
count(PTMH.00, SCOPI_01)


#STEP 3 - removing invalid cases + deidentifying the dataset
#selecting and removing rows in PTMH.00

library(ggplot2)
str(PTMH.00)    #tells me it has 708 observations of 636 variables
nrow(PTMH.00)   #confirm number of ROWS


#remove the Label/question line from Qualtrics, row 2
PTMH.707 <- PTMH.00[3:708,]
                #this created a new dataframe with selected [rows, columns]
                #default is to include all x or y if not specified
         

#to delete columns, create a new index/dataframe WithOut certain rows...
  # namenew <- nameoriginal[-c(rows to remove),-c(columns to remove)]

    #PTMH.707deID <- PTMH.00[-c(2), -c(RecipientEmail RecipientFirstName 
                                      #RecipientLastName ExternalReference)]
#Deidentify data
PTMH.707deID <- subset(PTMH.707, select = -c(RecipientLastName, ExternalReference, RecipientFirstName, RecipientEmail))
PTMH.707deID

#TO CREATE A SUBSET BASED ON SOME CONDITION
#incomplete cases 
PTMH.complete <- subset(PTMH.707deID, Finished>=1)
PTMH.complete

nrow(PTMH.complete)
PTMH.599 <- PTMH.complete

#PTMH.anonymous <- subset(PTMH.706, DistributionChannel=anonymous)
PTMH.anonymous #failed 
nrow(PTMH.anonymous) #failed, n=706 still

PTMH.consent <- subset(PTMH.599, Consent>=1)
PTMH.595 <- PTMH.consent

PTMH.blankcases <- PTMH.595[-c(19,54),] #rows 19 and 54 didnt fill out measures
nrow(PTMH.blankcases) 
PTMH.593 <- PTMH.blankcases

PTMHblank <- subset(PTMH.consent, Status==0)
nrow(PTMHblank)

PTMHblank2 <- subset(PTMHblank, SCOPI_01>=1)
nrow(PTMHblank2)


#delete test cases  
count(PTMH_01, ID)
PTMH.workers <- subset(PTMH_01, ...1>=10)
PTMH.workers
PTMH.rando <- subset(PTMH.workers, ID!=15314)
PTMH.rando
PTMH.588 <- PTMH.rando

#save this new deidentified, complete-case-only subset as a csv file :)
write.csv(PTMH.588, 'PTMH.01.csv')

#STEP 4 - VALIDITY CHECKS 
#create variables to score validity items 
#count how many passed 
#create a subset to delete cases that failed more than 2 out of 5
#check subset accuracy 

#PTMH_00
library(tidyverse)
view(PTMH_01)
select(PTMH_01, VALID_1, Valid_2, Valid_5, Valid_4, Valid_3)

library(dplyr)


PTMH.01 <- read.csv("PTMH_01.csv")
view(PTMH.01)
view(PTMH.01)

PTMH.01$Valid_1 <- PTMH.01$VALID_1

PTMH.01[]

#demo nested ifelse function
PTMH.01$test <- with(PTMH.01, ifelse(
  Valid_1 %in% c(1, 2), 2, ifelse(
    Valid_1 %in% c(3, 4), 1, 0
  )
))  
table(PTMH.01$test)
PTMH.01 <- subset(PTMH.01, select=-c(test)) 
#removed test from dataset


#validity check 1
PTMH.01$Valid_1[]
table(PTMH.01$Valid_1)

class(PTMH.01$Valid_1)
PTMH.01$Valid_1 <- as.numeric(PTMH.01$Valid_1)

#new var for passing validity item 1
PTMH.01$v1.pass <- with(PTMH.01, ifelse(
  Valid_1 %in% c(1, 2), 1, 0
))
table(PTMH.01$v1.pass)




#validity check 2
PTMH.01$Valid_2[]
table(PTMH.01$Valid_2)

# new var v2.pass 
PTMH.01$v2.pass <- with (PTMH.01, ifelse(Valid_2 %in% c(1), 1, 0))
table(PTMH.01$v2.pass)


#validity check 3
PTMH.01$Valid_3[]
table(PTMH.01$Valid_3)

#new var v3.pass
PTMH.01$v3.pass <- with(PTMH.01, ifelse(Valid_3 %in% c(5, 4), 1, 0))
table(PTMH.01$v3.pass)



#validity check 4
PTMH.01$Valid_4[]
table(PTMH.01$Valid_4)

#new var v4.pass
PTMH.01$v4.pass <- with(PTMH.01, ifelse(Valid_4 %in% c(1), 1, 0))
table(PTMH.01$v4.pass)



#validity check 5
PTMH.01$Valid_5[]
table(PTMH.01$Valid_5)

PTMH.01$v5.pass <- with(PTMH.01, Valid_5 %in% c(2), 1, 0)
table(PTMH.01$v5.pass)

PTMH.01$v5.pass <- as.numeric(PTMH.01$v5.pass)



#COMPOSITE validity 
#0 if fail and 1 if pass -> add all five
PTMH.01$v.score <- PTMH.01$v1.pass + PTMH.01$v2.pass + PTMH.01$v3.pass + PTMH.01$v4.pass +PTMH.01$v5.pass
table(PTMH.01$v.score)

#preregistered cutoff is 60%...need v.score of c(3,4,5) 
#delete cases where v.score is c(0,1,2)

PTMH.01valid <- subset(PTMH.01, v.score > 3) #YAY


#YAY


PTMH.01valid[]

#STEP 5 - seconds per item 
#create variables to score seconds per item per measure 
  #one second per item required minimum
#create subset for cases that did not fail these requirements

#create items for Page Submit time / number of items on the page (inc. validity items)
#scopi 48 
#idas 100
#mss 39
#pid 25
#bfi 45
#dps 34
#pcq 45
#spi = seconds per item
PTMH.01valid$SCOPI_time_Page.Submit / 48 -> PTMH.01valid$SCOPI.spi
PTMH.01valid$IDAS_time_Page.Submit / 100 -> PTMH.01valid$IDAS.spi
PTMH.01valid$MSS_time_Page.Submit / 39 -> PTMH.01valid$MSS.spi
PTMH.01valid$PID_time_Page.Submit / 25-> PTMH.01valid$PID.spi
PTMH.01valid$BFI_time_Page.Submit / 45 -> PTMH.01valid$BFI.spi
PTMH.01valid$DPS_time_Page.Submit / 34 -> PTMH.01valid$DPS.spi
PTMH.01valid$PCQ_time_Page.Submit / 45 -> PTMH.01valid$PCQ.spi

table(PTMH.01valid$SCOPI.spi) #lets me check the pass item works properly

PTMH.01valid$SCOPI.pass <- ifelse(PTMH.01valid$SCOPI.spi<1, 0, 1)
PTMH.01valid$IDAS.pass <- ifelse(PTMH.01valid$IDAS.spi<1, 0, 1)
PTMH.01valid$MSS.pass <- ifelse(PTMH.01valid$MSS.spi<1, 0, 1)
PTMH.01valid$PID.pass <- ifelse(PTMH.01valid$PID.spi<1, 0, 1)
PTMH.01valid$BFI.pass <- ifelse(PTMH.01valid$BFI.spi<1, 0, 1)
PTMH.01valid$DPS.pass <- ifelse(PTMH.01valid$DPS.spi<1, 0, 1)
PTMH.01valid$PCQ.pass <- ifelse(PTMH.01valid$PCQ.spi<1, 0, 1)

table(PTMH.01valid$SCOPI.pass)
table(PTMH.01valid$IDAS.pass)
table(PTMH.01valid$MSS.pass)
table(PTMH.01valid$PID.pass) 
table(PTMH.01valid$BFI.pass)
table(PTMH.01valid$DPS.pass)


PTMH.01valid2$pass.7SR <- (PTMH.01valid$SCOPI.pass +PTMH.01valid$IDAS.pass + 
                            PTMH.01valid$MSS.pass +PTMH.01valid$PID.pass + 
                            PTMH.01valid$BFI.pass + PTMH.01valid$DPS.pass + 
                            PTMH.01valid$PCQ.pass)

PTMH.01valid2 <- subset(PTMH.01valid, pass.7SR > 6)
table(PTMH.01valid2$pass.7SR)


PTMH.01valid2$PTCQ_timeA_Page.Submit / 56 -> PTMH.01valid2$PTCQ.a.spi
PTMH.01valid2$PTCQ_timeB_Page.Submit / 56 -> PTMH.01valid2$PTCQ.b.spi
PTMH.01valid2$PTCQ_timeC_Page.Submit / 56 -> PTMH.01valid2$PTCQ.c.spi

PTMH.01valid2$PTCQ.a.pass <- ifelse(PTMH.01valid2$PTCQ.a.spi<1, 0, 1)
PTMH.01valid2$PTCQ.b.pass <- ifelse(PTMH.01valid2$PTCQ.b.spi<1, 0, 1)
PTMH.01valid2$PTCQ.c.pass <- ifelse(PTMH.01valid2$PTCQ.c.spi<1, 0, 1)

table(PTMH.01valid2$PTCQ.a.pass)
table(PTMH.01valid2$PTCQ.b.pass)
table(PTMH.01valid2$PTCQ.c.pass)


#RECREATE THESE FOR FUTURE PTCQ AN ANALYSIS AFTER DATA CODING AND IMPUTATION,
#or just copy and paste the rest of the code but tbh that's way more work
PTMH.02.PTa <- subset(PTMH.01valid2, PTCQ.a.pass = 1)
PTMH.02.PTab <- subset(PTMH.02.PTa, PTCQ.b.pass = 1)
PTMH.02.PTabc <- subset(PTMH.02.PTab, PTCQ.c.pass = 1)


PTMH.02 <- PTMH.01valid2
write.csv(PTMH.02, 'PTMH.02.csv')

#NEW DATASET = CLEANED OF INVALID CASES
  #SUMMARY: incomplete responses, test cases, cases that failed validity checks 
  #data was not scrubbed of incomplete PTCQ scores. this will be done later


#STEP 5 - recode reversed items, incorrect scoring, and create subscales

#ITEMS THAT ARE REVERSE CODED: 
#IDAS_27 IDAS_64 
#MSS_4 MSS_10 MSS_25 MSS_37
#BFI : 2  6 8 9 12  18  21  23  24  27  31  34  35  37  41  43

library(car)
PTMH.02$IDAS_27r = recode(PTMH.02$IDAS_27, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$IDAS_64r = recode(PTMH.02$IDAS_64, '1=5; 2=4;3=3; 4=2; 5=1')

PTMH.02$MSS_04r = recode(PTMH.02$MSS_04, '1=0; 0=1')
PTMH.02$MSS_10r = recode(PTMH.02$MSS_10, '1=0; 0=1')
PTMH.02$MSS_25r = recode(PTMH.02$MSS_25, '1=0; 0=1')
PTMH.02$MSS_37r = recode(PTMH.02$MSS_37, '1=0; 0=1')

PTMH.02$BFI_02r = recode(PTMH.02$BFI_02, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_06r = recode(PTMH.02$BFI_06, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_08r = recode(PTMH.02$BFI_08, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_09r = recode(PTMH.02$BFI_09, '1=5; 2=4;3=3; 4=2; 5=1')

PTMH.02$BFI_12 <- PTMH.02$X12 
class(PTMH.02$BFI_12)
PTMH.02$BFI_12 <- as.numeric(PTMH.02$BFI_12)
PTMH.02$BFI_12r = recode(PTMH.02$BFI_12, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_18r = recode(PTMH.02$BFI_18, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_21r = recode(PTMH.02$BFI_21, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_23r = recode(PTMH.02$BFI_23, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_24r = recode(PTMH.02$BFI_24, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_27r = recode(PTMH.02$BFI_27, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_31r = recode(PTMH.02$BFI_31, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_34r = recode(PTMH.02$BFI_34, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_35r = recode(PTMH.02$BFI_35, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_37r = recode(PTMH.02$BFI_37, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_41r = recode(PTMH.02$BFI_41, '1=5; 2=4;3=3; 4=2; 5=1')
PTMH.02$BFI_43r = recode(PTMH.02$BFI_43, '1=5; 2=4;3=3; 4=2; 5=1')

#done with reverse-coding
#521 obs of 687 variables 


#SCALES THAT NEED TO BE RECODED 
#PID should be 0 1 2 3
#PCQ should be 1 2 3 4 5 6 

PTMH.02$PID_01 = recode(PTMH.02$PID_01, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_02 = recode(PTMH.02$PID_02, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_03 = recode(PTMH.02$PID_03, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_04 = recode(PTMH.02$PID_04, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_05 = recode(PTMH.02$PID_05, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_06 = recode(PTMH.02$PID_06, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_07 = recode(PTMH.02$PID_07, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_08 = recode(PTMH.02$PID_08, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_09 = recode(PTMH.02$PID_09, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_10 = recode(PTMH.02$PID_10, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_11 = recode(PTMH.02$PID_11, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_12 = recode(PTMH.02$PID_12, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_13 = recode(PTMH.02$PID_13, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_14 = recode(PTMH.02$PID_14, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_15 = recode(PTMH.02$PID_15, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_16 = recode(PTMH.02$PID_16, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_17 = recode(PTMH.02$PID_17, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_18 = recode(PTMH.02$PID_18, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_19 = recode(PTMH.02$PID_19, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_20 = recode(PTMH.02$PID_20, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_21 = recode(PTMH.02$PID_21, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_22 = recode(PTMH.02$PID_22, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_23 = recode(PTMH.02$PID_23, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_24 = recode(PTMH.02$PID_24, '1=0; 2=1; 3=2; 4=3')
PTMH.02$PID_25 = recode(PTMH.02$PID_25, '1=0; 2=1; 3=2; 4=3')

PTMH.02$PCQ_01 = recode(PTMH.02$PCQ_01, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_02 = recode(PTMH.02$PCQ_02, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_03 = recode(PTMH.02$PCQ_03, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_04 = recode(PTMH.02$PCQ_04, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_05 = recode(PTMH.02$PCQ_05, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_06 = recode(PTMH.02$PCQ_06, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_07 = recode(PTMH.02$PCQ_07, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_08 = recode(PTMH.02$PCQ_08, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_09 = recode(PTMH.02$PCQ_09, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_10 = recode(PTMH.02$PCQ_10, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_11 = recode(PTMH.02$PCQ_11, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_12 = recode(PTMH.02$PCQ_12, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_13 = recode(PTMH.02$PCQ_13, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_14 = recode(PTMH.02$PCQ_14, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_15 = recode(PTMH.02$PCQ_15, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_16 = recode(PTMH.02$PCQ_16, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_17 = recode(PTMH.02$PCQ_17, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_18 = recode(PTMH.02$PCQ_18, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_19 = recode(PTMH.02$PCQ_19, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_20 = recode(PTMH.02$PCQ_20, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_21 = recode(PTMH.02$PCQ_21, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_22 = recode(PTMH.02$PCQ_22, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_23 = recode(PTMH.02$PCQ_23, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_24 = recode(PTMH.02$PCQ_24, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_25 = recode(PTMH.02$PCQ_25, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_26 = recode(PTMH.02$PCQ_26, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_27 = recode(PTMH.02$PCQ_27, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_28 = recode(PTMH.02$PCQ_28, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_29 = recode(PTMH.02$PCQ_29, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_30 = recode(PTMH.02$PCQ_30, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_31 = recode(PTMH.02$PCQ_31, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_32 = recode(PTMH.02$PCQ_32, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_33 = recode(PTMH.02$PCQ_33, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_34 = recode(PTMH.02$PCQ_34, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_35 = recode(PTMH.02$PCQ_35, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_36 = recode(PTMH.02$PCQ_36, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_37 = recode(PTMH.02$PCQ_37, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_38 = recode(PTMH.02$PCQ_38, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_39 = recode(PTMH.02$PCQ_39, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_40 = recode(PTMH.02$PCQ_40, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_41 = recode(PTMH.02$PCQ_41, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_42 = recode(PTMH.02$PCQ_42, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_43 = recode(PTMH.02$PCQ_43, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_44 = recode(PTMH.02$PCQ_44, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')
PTMH.02$PCQ_45 = recode(PTMH.02$PCQ_45, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5')

#done with recoding measures 
#521 obs of 687 variables 

#CREATE SUBSCALES 
#before: 521 cases and 687 variables 
#SCOPI 
#Obsessive Checking 1, 4, 8, 12, 15, 18, 21, 25, 28, 31, 35, 38, 40, 45
#Obsessive Cleanliness 	2, 6, 9, 13, 16, 19, 22, 26, 29, 32, 36, 43
#Compulsive Rituals 	3, 7, 14, 17, 24, 33, 41, 44
#Hoarding 	10, 20, 27, 39, 46
#Pathological Impulses 	5, 11, 23, 30, 34, 37, 42, 47
PTMH.02$SCOPI.Check <- (PTMH.02$SCOPI_01 + PTMH.02$SCOPI_04 + PTMH.02$SCOPI_08
                        + PTMH.02$SCOPI_12 + PTMH.02$SCOPI_15 + PTMH.02$SCOPI_18 
                        + PTMH.02$SCOPI_21 + PTMH.02$SCOPI_25 + PTMH.02$SCOPI_28 
                        + PTMH.02$SCOPI_31 + PTMH.02$SCOPI_35 + PTMH.02$SCOPI_38 
                        + PTMH.02$SCOPI_40 + PTMH.02$SCOPI_45)
PTMH.02$SCOPI.Clean <- (PTMH.02$SCOPI_02 + PTMH.02$SCOPI_06 + PTMH.02$SCOPI_09
                        + PTMH.02$SCOPI_13 + PTMH.02$SCOPI_16 + PTMH.02$SCOPI_19 
                        + PTMH.02$SCOPI_22 + PTMH.02$SCOPI_26 + PTMH.02$SCOPI_29
                        + PTMH.02$SCOPI_32 + PTMH.02$SCOPI_36 + PTMH.02$SCOPI_43)
PTMH.02$SCOPI_Ritual <- (PTMH.02$SCOPI_03 + PTMH.02$SCOPI_07 + PTMH.02$SCOPI_14
                         + PTMH.02$SCOPI_17 + PTMH.02$SCOPI_24 + PTMH.02$SCOPI_33
                         + PTMH.02$SCOPI_41 + PTMH.02$SCOPI_44)
PTMH.02$SCOPI_Hoard <- (PTMH.02$SCOPI_10 + PTMH.02$SCOPI_20 + PTMH.02$SCOPI_27
                        + PTMH.02$SCOPI_39 + PTMH.02$SCOPI_46)
PTMH.02$SCOPI_Impulse <- (PTMH.02$SCOPI_05 + PTMH.02$SCOPI_11 + PTMH.02$SCOPI_23
                          + PTMH.02$SCOPI_30 + PTMH.02$SCOPI_34 + PTMH.02$SCOPI_37
                          + PTMH.02$SCOPI_42 + PTMH.02$SCOPI_47)
#PTMH.02$SCOPI_OCD <- PTMH.02$SCOPI_Check + PTMH.02$SCOPI_Clean + PTMH.02$SCOPI_Ritual
class(PTMH.02$SCOPI_Clean)
PTMH.02$SCOPI.Check <- as.numeric(PTMH.02$SCOPI.Check)
PTMH.02$SCOPI.Clean <- as.numeric(PTMH.02$SCOPI.Clean)
PTMH.02$SCOPI_Ritual <- as.numeric(PTMH.02$SCOPI_Ritual)
PTMH.02$SCOPI_Hoard <- as.numeric(PTMH.02$SCOPI_Hoard)
PTMH.02$SCOPI_Impulse <- as.numeric(PTMH.02$SCOPI_Impulse)

PTMH.02$SCOPI.OCD <- (PTMH.02$SCOPI_01 + PTMH.02$SCOPI_04 + PTMH.02$SCOPI_08
                      + PTMH.02$SCOPI_12 + PTMH.02$SCOPI_15 + PTMH.02$SCOPI_18 
                      + PTMH.02$SCOPI_21 + PTMH.02$SCOPI_25 + PTMH.02$SCOPI_28 
                      + PTMH.02$SCOPI_31 + PTMH.02$SCOPI_35 + PTMH.02$SCOPI_38 
                      + PTMH.02$SCOPI_40 + PTMH.02$SCOPI_45 + 
                        PTMH.02$SCOPI_02 + PTMH.02$SCOPI_06 + PTMH.02$SCOPI_09
                      + PTMH.02$SCOPI_13 + PTMH.02$SCOPI_16 + PTMH.02$SCOPI_19 
                      + PTMH.02$SCOPI_22 + PTMH.02$SCOPI_26 + PTMH.02$SCOPI_29
                      + PTMH.02$SCOPI_32 + PTMH.02$SCOPI_36 + PTMH.02$SCOPI_43 +
                        PTMH.02$SCOPI_03 + PTMH.02$SCOPI_07 + PTMH.02$SCOPI_14
                      + PTMH.02$SCOPI_17 + PTMH.02$SCOPI_24 + PTMH.02$SCOPI_33
                      + PTMH.02$SCOPI_41 + PTMH.02$SCOPI_44)
  #OCD is sum of Check, Clean, and Ritual
#521 obs of 693 variables

library(utils)

#IDAS-2
PTMH.02$GenDep <- (PTMH.02$IDAS_01 + PTMH.02$IDAS_02 + PTMH.02$IDAS_05
                    +  PTMH.02$IDAS_06 + PTMH.02$IDAS_08 + PTMH.02$IDAS_09
                      + PTMH.02$IDAS_11 + PTMH.02$IDAS_13 + PTMH.02$IDAS_21
                     + PTMH.02$IDAS_26 + PTMH.02$IDAS_27r + PTMH.02$IDAS_30
                    + PTMH.02$IDAS_31 + PTMH.02$IDAS_40 + PTMH.02$IDAS_48
                    + PTMH.02$IDAS_51 + PTMH.02$IDAS_52 + PTMH.02$IDAS_57
                     + PTMH.02$IDAS_61 +PTMH.02$IDAS_64r)
PTMH.02$GenDep <- as.numeric(PTMH.02$GenDep)
PTMH.02$i.gendep <- PTMH.02$GenDep / 20
PTMH.02$Dys <- (PTMH.02$IDAS_02 + PTMH.02$IDAS_05 + PTMH.02$IDAS_08 
                  + PTMH.02$IDAS_09 + PTMH.02$IDAS_21 + PTMH.02$IDAS_31
                  + PTMH.02$IDAS_40 + PTMH.02$IDAS_48 + PTMH.02$IDAS_57
                  + PTMH.02$IDAS_61)
PTMH.02$i.dys <- PTMH.02$Dys / 10 
PTMH.02$Lass <- (PTMH.02$IDAS_06 + PTMH.02$IDAS_29 + PTMH.02$IDAS_30
                   + PTMH.02$IDAS_43 + PTMH.02$IDAS_54 + PTMH.02$IDAS_55)
PTMH.02$Lass <- as.numeric(PTMH.02$Lass)
PTMH.02$i.lass <- PTMH.02$Lass / 6
PTMH.02$Insom <- (PTMH.02$IDAS_04 + PTMH.02$IDAS_11 + PTMH.02$IDAS_17
                    + PTMH.02$IDAS_25 + PTMH.02$IDAS_36 + PTMH.02$IDAS_51)
PTMH.02$Insom <- as.numeric(PTMH.02$Insom)
PTMH.02$i.ins <- PTMH.02$Insom / 6
PTMH.02$Suic <- (PTMH.02$IDAS_13 + PTMH.02$IDAS_22 + PTMH.02$IDAS_33
                   + PTMH.02$IDAS_38 + PTMH.02$IDAS_46 + PTMH.02$IDAS_52)
PTMH.02$i.suic <- PTMH.02$Suic / 6
PTMH.02$AppLoss <- (PTMH.02$IDAS_01 + PTMH.02$IDAS_26 + PTMH.02$IDAS_60)
PTMH.02$AppLoss <- as.numeric(PTMH.02$AppLoss)
PTMH.02$i.aplos <- PTMH.02$AppLoss/ 3
PTMH.02$AppGain <- (PTMH.02$IDAS_19 + PTMH.02$IDAS_24 + PTMH.02$IDAS_63)
PTMH.02$i.appgain <- PTMH.02$AppGain / 3
PTMH.02$Well <- (PTMH.02$IDAS_03 + PTMH.02$IDAS_10 + PTMH.02$IDAS_23
                   + PTMH.02$IDAS_27 + PTMH.02$IDAS_50 + PTMH.02$IDAS_53
                   + PTMH.02$IDAS_59 + PTMH.02$IDAS_64)
PTMH.02$i.well <- PTMH.02$Well / 8
PTMH.02$Temper <- (PTMH.02$IDAS_12 + PTMH.02$IDAS_35 + PTMH.02$IDAS_37
                     + PTMH.02$IDAS_44 + PTMH.02$IDAS_62)
PTMH.02$i.temper <- PTMH.02$Temper / 5
PTMH.02$Mania <- (PTMH.02$IDAS_67 + PTMH.02$IDAS_71 + PTMH.02$IDAS_77
                    + PTMH.02$IDAS_83 + PTMH.02$IDAS_87)
PTMH.02$i.mania <- PTMH.02$Mania / 5 
PTMH.02$Euph <- (PTMH.02$IDAS_72 + PTMH.02$IDAS_78 + PTMH.02$IDAS_88
                   + PTMH.02$IDAS_92 + PTMH.02$IDAS_97)
PTMH.02$i.euph <- PTMH.02$Euph / 5
PTMH.02$Panic <- (PTMH.02$IDAS_07 + PTMH.02$IDAS_16 + PTMH.02$IDAS_32
                    + PTMH.02$IDAS_39 + PTMH.02$IDAS_45 + PTMH.02$IDAS_49
                    + PTMH.02$IDAS_56 + PTMH.02$IDAS_58)
PTMH.02$i.panic <- PTMH.02$Panic / 8
PTMH.02$SocAnx <- (PTMH.02$IDAS_15 + PTMH.02$IDAS_18 + PTMH.02$IDAS_20 
                     + PTMH.02$IDAS_41 + PTMH.02$IDAS_47 + PTMH.02$IDAS_99)
PTMH.02$i.sanx <- PTMH.02$SocAnx / 6
PTMH.02$Claus <- (PTMH.02$IDAS_74 + PTMH.02$IDAS_80 + PTMH.02$IDAS_84
                    + PTMH.02$IDAS_90 + PTMH.02$IDAS_94)
PTMH.02$i.claus <- PTMH.02$Claus / 5
PTMH.02$Intru <- (PTMH.02$IDAS_14 + PTMH.02$IDAS_28 + PTMH.02$IDAS_34 
                    + PTMH.02$IDAS_42)
PTMH.02$i.intru <- PTMH.02$Intru / 4
PTMH.02$Avoid <- (PTMH.02$IDAS_73 + PTMH.02$IDAS_79 + PTMH.02$IDAS_89 
                    + PTMH.02$IDAS_93)
PTMH.02$i.avoid <- PTMH.02$Avoid / 4 
PTMH.02$Check <- (PTMH.02$IDAS_68 + PTMH.02$IDAS_75 + PTMH.02$IDAS_81)
PTMH.02$i.check <- PTMH.02$Check / 3
PTMH.02$Order <- (PTMH.02$IDAS_65 + PTMH.02$IDAS_69 + PTMH.02$IDAS_82
                    + PTMH.02$IDAS_85 + PTMH.02$IDAS_95)
PTMH.02$i.order <- PTMH.02$Order / 5 
PTMH.02$Clean <- (PTMH.02$IDAS_66 + PTMH.02$IDAS_70 + PTMH.02$IDAS_76 
                    + PTMH.02$IDAS_86 + PTMH.02$IDAS_91 + PTMH.02$IDAS_96
                    + PTMH.02$IDAS_98)
PTMH.02$i.clean <- PTMH.02$Clean / 7
#521 obs of 731 variables
#IDAS VAR NAMES:
#PTMH.02$i.Gendep PTMH.02$i.dys PTMH.02$i.lass PTMH.02$i.ins PTMH.02$i.suic
#PTMH.02$i.aplos PTMH.02$i.appgain PTMH.02$i.well PTMH.02$i.temper PTMH.02$i.mania
#PTMH.02$i.euph PTMH.02$i.panic PTMH.02$i.sanx PTMH.02$i.claus PTMH.02$i.intru
#PTMH.02$i.avoid PTMH.02$i.check PTMH.02$i.order PTMH.02$i.clean
  # i.gendep  i.dys i.lass  i.ins i.suic  i.aplos i.appgaon i.well  i.temper
  # i.mania i.euph  i.panic i.panic i.sanx  i.claus i.intru i.avoid i.check
  # i.order i.clean 


#MSS-B
#Positive schizotypy – 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38 
#Negative schizotypy – 1, 4*, 7, 10*, 13, 16, 19, 22, 25*, 28, 31, 34, 37* 
#Disorganized Schizotypy – 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36 
#SUM scores, no total score 
PTMH.02$mss.pos <- (PTMH.02$MSS_02 + PTMH.02$MSS_05 + PTMH.02$MSS_08 + 
                   PTMH.02$MSS_11 + PTMH.02$MSS_14 + PTMH.02$MSS_17 + 
                   PTMH.02$MSS_20 + PTMH.02$MSS_23 + PTMH.02$MSS_26 +
                   PTMH.02$MSS_29 + PTMH.02$MSS_32 + PTMH.02$MSS_35 + 
                   PTMH.02$MSS_38)
PTMH.02$mss.neg <- (PTMH.02$MSS_01 + PTMH.02$MSS_04r + PTMH.02$MSS_07 + 
                    PTMH.02$MSS_10r + PTMH.02$MSS_13 + PTMH.02$MSS_16 + 
                      PTMH.02$MSS_19 + PTMH.02$MSS_22 + PTMH.02$MSS_25r + 
                      PTMH.02$MSS_28 + PTMH.02$MSS_31 + PTMH.02$MSS_34 + 
                      PTMH.02$MSS_37r)
PTMH.02$mss.dis <- (PTMH.02$MSS_03 + PTMH.02$MSS_06 + PTMH.02$MSS_09 + 
                      PTMH.02$MSS_12 + PTMH.02$MSS_15 + PTMH.02$MSS_18 + 
                      PTMH.02$MSS_21 + PTMH.02$MSS_24 + PTMH.02$MSS_27 + 
                      PTMH.02$MSS_30 + PTMH.02$MSS_33 + PTMH.02$MSS_36)
#MSS done - 521 obs of 734 variables


#PID-5-BF
#Negative Affect - 8 9 10 11 15
#Detachment - 4 13 14 16 18
#Antagonism - 17 19 20 22 25
#Disinhibition - 1 2 3 5 6 
#Psychoticism - 7 12 21 23 24
PTMH.02$pid.neg <- (PTMH.02$PID_08 + PTMH.02$PID_09 + PTMH.02$PID_10 + 
                      PTMH.02$PID_11 + PTMH.02$PID_15)
PTMH.02$pid.det <- (PTMH.02$PID_04 + PTMH.02$PID_13 + PTMH.02$PID_14 + 
                         PTMH.02$PID_16 + PTMH.02$PID_18)
PTMH.02$pid.ant <- (PTMH.02$PID_17 + PTMH.02$PID_19 + PTMH.02$PID_20 + 
                      PTMH.02$PID_22 + PTMH.02$PID_25)
PTMH.02$pid.dis <- (PTMH.02$PID_01 + PTMH.02$PID_02 + PTMH.02$PID_03 + 
                      PTMH.02$PID_05 + PTMH.02$PID_06)
PTMH.02$pid.psy <- (PTMH.02$PID_07 + PTMH.02$PID_12 + PTMH.02$PID_21 + 
                      PTMH.02$PID_23 + PTMH.02$PID_24)
#PID 5 DONE 521 obs of 739 variables 

#BFI 
#mean scores
PTMH.02$bfi.o.sum <- (PTMH.02$BFI_05 + PTMH.02$BFI_10 + PTMH.02$BFI_15 + 
                    PTMH.02$BFI_20 + PTMH.02$BFI_25 + PTMH.02$BFI_30 + 
                    PTMH.02$BFI_35r + PTMH.02$BFI_40 + PTMH.02$BFI_41r + 
                    PTMH.02$BFI_44)
PTMH.02$bfi.o <- PTMH.02$bfi.o.sum / 10 
PTMH.02$bfi.c.sum <- (PTMH.02$BFI_03 + PTMH.02$BFI_08r + PTMH.02$BFI_13 + 
                        PTMH.02$BFI_18r + PTMH.02$BFI_23r + PTMH.02$BFI_28 + 
                        PTMH.02$BFI_33 + PTMH.02$BFI_38 + PTMH.02$BFI_43r)
PTMH.02$bfi.c <- PTMH.02$bfi.c.sum / 9 
PTMH.02$bfi.e.sum <- (PTMH.02$BFI_01 + PTMH.02$BFI_06r + PTMH.02$BFI_11 + 
                        PTMH.02$BFI_16 + PTMH.02$BFI_21r + PTMH.02$BFI_26 + 
                        PTMH.02$BFI_31r + PTMH.02$BFI_36)
PTMH.02$bfi.e <- PTMH.02$bfi.e.sum / 8 
PTMH.02$bfi.a.sum <- (PTMH.02$BFI_02r + PTMH.02$BFI_07 + PTMH.02$BFI_12r + 
                        PTMH.02$BFI_17 + PTMH.02$BFI_22 + PTMH.02$BFI_27r + 
                        PTMH.02$BFI_32 + PTMH.02$BFI_37r + PTMH.02$BFI_42)
PTMH.02$bfi.a <- PTMH.02$bfi.a.sum / 9 
PTMH.02$bfi.n.sum <- (PTMH.02$BFI_04 + PTMH.02$BFI_09r + PTMH.02$BFI_14 + 
                        PTMH.02$BFI_19 + PTMH.02$BFI_24r + PTMH.02$BFI_29 + 
                        PTMH.02$BFI_34r + PTMH.02$BFI_39)
PTMH.02$bfi.n <- PTMH.02$bfi.n / 8 
#BFI DONE 521 obs of 749 variables 


#DPS
#Obliviousness – items # 2, 3, 7, 8, 10, 16, 18, 19, 20, 23, 24, 26, 28, 32
#Imagination – items # 1, 5, 11, 13, 15, 21, 29
#Detachment -- items # 12, 14, 17, 27, 30, 33
#some items not in a subscale -> composite score needed 
#1 is low/false, 5 is high/true 
PTMH.02$dps.obliv <- (PTMH.02$DPS_02 + PTMH.02$DPS_03 + PTMH.02$DPS_07 + 
                        PTMH.02$DPS_08 + PTMH.02$DPS_10 + PTMH.02$DPS_16 + 
                        PTMH.02$DPS_18 + PTMH.02$DPS_19 + PTMH.02$DPS_20 + 
                        PTMH.02$DPS_23 + PTMH.02$DPS_24 + PTMH.02$DPS_26 + 
                        PTMH.02$DPS_28 + PTMH.02$DPS_32)
PTMH.02$dps.imag <- (PTMH.02$DPS_01 + PTMH.02$DPS_05 + PTMH.02$DPS_11 + 
                       PTMH.02$DPS_13 + PTMH.02$DPS_15 + PTMH.02$DPS_21 + 
                       PTMH.02$DPS_29)
PTMH.02$dps.detach <- (PTMH.02$DPS_12 + PTMH.02$DPS_14 + PTMH.02$DPS_17 + 
                         PTMH.02$DPS_27 + PTMH.02$DPS_30 + PTMH.02$DPS_33)
#DPS DONE 521 obs of 752 variables 

#PCQ
#LC Mean (3, 16, 30, 37, 44) 
#PF Mean (8, 9, 18, 24, 26, 36, 41) 
#EW Mean (17, 21, 27, 43)
#SC Mean (6, 12, 34, 40)
#DP Mean (1, 2, 7, 13, 19, 20, 22, 23, 31, 32, 33, 35, 38, 42) 
#TD Mean (4, 5, 10, 11, 14, 15, 25, 28, 29, 39, 45) 
PTMH.02$PCQ_LCsum <- (PTMH.02$PCQ_03 + PTMH.02$PCQ_16 + PTMH.02$PCQ_30 + 
                      PTMH.02$PCQ_27 + PTMH.02$PCQ_44)
PTMH.02$pcq.lc <- PTMH.02$PCQ_LCsum / 5 
PTMH.02$PCQ_PFsum <- (PTMH.02$PCQ_08 + PTMH.02$PCQ_09 + PTMH.02$PCQ_18 + 
                        PTMH.02$PCQ_24 + PTMH.02$PCQ_26 + PTMH.02$PCQ_36 + 
                        PTMH.02$PCQ_41)
PTMH.02$pcq.pf <- PTMH.02$PCQ_PFsum / 7
PTMH.02$PCQ_EWsum <- (PTMH.02$PCQ_17 + PTMH.02$PCQ_21 + PTMH.02$PCQ_27 + 
                        PTMH.02$PCQ_43)
PTMH.02$pcq.ew <- PTMH.02$PCQ_EWsum / 4
PTMH.02$PCQ_SCsum <- (PTMH.02$PCQ_06 + PTMH.02$PCQ_12 + PTMH.02$PCQ_34 + 
                        PTMH.02$PCQ_40)
PTMH.02$pcq.sc <- PTMH.02$PCQ_SCsum / 4 
PTMH.02$PCQ_DPsum <- (PTMH.02$PCQ_01 + PTMH.02$PCQ_02 + PTMH.02$PCQ_07 + 
                        PTMH.02$PCQ_13 + PTMH.02$PCQ_19 + PTMH.02$PCQ_20 + 
                        PTMH.02$PCQ_22 + PTMH.02$PCQ_23 + PTMH.02$PCQ_31 + 
                        PTMH.02$PCQ_32 + PTMH.02$PCQ_33 + PTMH.02$PCQ_35 + 
                        PTMH.02$PCQ_38 + PTMH.02$PCQ_42)
PTMH.02$pcq.dp <- PTMH.02$PCQ_DPsum / 14 
PTMH.02$PCQ_TDsum <- (PTMH.02$PCQ_04 + PTMH.02$PCQ_05 + PTMH.02$PCQ_10 + 
                        PTMH.02$PCQ_11 + PTMH.02$PCQ_14 + PTMH.02$PCQ_15 + 
                        PTMH.02$PCQ_25 + PTMH.02$PCQ_28 + PTMH.02$PCQ_29 + 
                        PTMH.02$PCQ_39 + PTMH.02$PCQ_45)
PTMH.02$pcq.td <- PTMH.02$PCQ_TDsum / 11 
5+7+4+4+14+11
#PCQ DONE 521 obs of 764 variables 



PTMH.02[]

PTMH.03 <- PTMH.02
write.csv(PTMH.03, 'PTMH.03.csv')
#PTMH.03.csv has 521 obs of 764 variables 

#PTMH.03a <- subset(PTMH.03, select = -c())
#PTMH.03a


class(PTMH.02$i.gendep)
class(PTMH.02$i.dys)
class(PTMH.02$i.lass)
class(PTMH.02$i.ins)
class(PTMH.02$i.suic)
class(PTMH.02$i.aplos)
class(PTMH.02$i.appgain)
class(PTMH.02$i.well)
class(PTMH.02$i.temper)
class(PTMH.02$i.mania)
class(PTMH.02$i.euph)
class(PTMH.02$i.panic)
class(PTMH.02$i.sanx)
class(PTMH.02$i.claus)
class(PTMH.02$i.intru)
class(PTMH.02$i.avoid)
class(PTMH.02$i.check)
class(PTMH.02$i.order)
class(PTMH.02$i.clean)
class(PTMH.02$mss.pos)
class(PTMH.02$mss.neg)
class(PTMH.02$mss.dis)
class(PTMH.02$pid.neg)
class(PTMH.02$pid.det)
class(PTMH.02$pid.ant)
class(PTMH.02$pid.dis)
class(PTMH.02$pid.psy)
class(PTMH.02$bfi.a)
class(PTMH.02$bfi.c)
class(PTMH.02$bfi.e)
class(PTMH.02$bfi.o)
class(PTMH.02$bfi.n)
class(PTMH.02$dps.obliv)
class(PTMH.02$dps.imag)
class(PTMH.02$dps.detach)
class(PTMH.02$pcq.lc)
class(PTMH.02$pcq.pf)
class(PTMH.02$pcq.ew)
class(PTMH.02$pcq.sc)
class(PTMH.02$pcq.dp)
class(PTMH.02$pcq.td)

PTMH.02$mss.pos <-as.numeric(PTMH.02$mss.pos)
PTMH.02$mss.dis <-as.numeric(PTMH.02$mss.dis)

PTMH.02$dps.obliv <-as.numeric(PTMH.02$dps.obliv)
PTMH.02$dps.imag <-as.numeric(PTMH.02$dps.imag)
PTMH.02$dps.detach <-as.numeric(PTMH.02$dps.detach)




#DATA IMPUTATION 
  # mice - multiple imputation by chained equations 
  #step 1 - impute values as placeholders (can be a mean or median)
  #step 2 - utilize placeholders 
  #step 3 - regress feature 
  #step 4 - impute predictions from regression 
  #step 5 - repeat 1-4 for each feature that needs imputing 
  #step 6 - repeat step 5 until convergence or end of cycle (maybe 10) 


library(mice)
library(tidyverse)

PTMH.03 %>% summarise_if(is.numeric, funs(sum(is.na = TRUE)))

library(dplyr)
?mutate
mutate(PTMH.03, id = row_number())

nrow(PTMH.03)
ncol(PTMH.03)

unique(PTMH.03$SCOPI.Check)
unique(PTMH.03$SCOPI.Clean)
unique(PTMH.03$SCOPI_Hoard)
unique(PTMH.03$SCOPI_Impulse)
unique(PTMH.03$SCOPI_Ritual)
unique(PTMH.03$SCOPI.OCD)
unique(PTMH.02$i.gendep)
unique(PTMH.02$i.dys)
unique(PTMH.02$i.lass)
unique(PTMH.02$i.ins)
unique(PTMH.02$i.suic)
unique(PTMH.02$i.aplos)
unique(PTMH.02$i.appgain)
unique(PTMH.02$i.well)
unique(PTMH.02$i.temper)
unique(PTMH.02$i.mania)
unique(PTMH.02$i.euph)
unique(PTMH.02$i.panic)
unique(PTMH.02$i.sanx)
unique(PTMH.02$i.claus)
unique(PTMH.02$i.intru)
unique(PTMH.02$i.avoid)
unique(PTMH.02$i.check)
unique(PTMH.02$i.order)
unique(PTMH.02$i.clean)
unique(PTMH.02$mss.pos)
unique(PTMH.02$mss.neg)
unique(PTMH.02$mss.dis)
unique(PTMH.02$pid.neg)
unique(PTMH.02$pid.det)
unique(PTMH.02$pid.ant)
unique(PTMH.02$pid.dis)
unique(PTMH.02$pid.psy)
unique(PTMH.02$bfi.a)
unique(PTMH.02$bfi.c)
unique(PTMH.02$bfi.e)
unique(PTMH.02$bfi.o)
unique(PTMH.02$bfi.n)
unique(PTMH.02$dps.obliv)
unique(PTMH.02$dps.imag)
unique(PTMH.02$dps.detach)
unique(PTMH.02$pcq.lc)
unique(PTMH.02$pcq.pf)
unique(PTMH.02$pcq.ew)
unique(PTMH.02$pcq.sc)
unique(PTMH.02$pcq.dp)
unique(PTMH.02$pcq.td)

md.pattern(PTMH.03)

methods(mice)
summary(PTMH.03$SCOPI_01)
count_fields(PTMH_03)
summary(PTMH_03)


sapply()
