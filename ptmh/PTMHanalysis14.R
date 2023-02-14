#PTMH DATA ANALYSIS - INCREMENTAL VALIDITY OF pt


View(PTMH.05)



-------------#DESCCRIPTIVES----------------------------------------------------


mean(x = PTMH.04$AGE)
median(x = PTMH.04$AGE)
min(x = PTMH.04$AGE)
max(x = PTMH.04$AGE)
range(x = PTMH.04$AGE)

PTMH.04$Duration.in.minutes <- PTMH.04$Duration.in.seconds/60

mean(x=PTMH.04$Duration.in.minutes)



-------------#DATA ANALYSIS----------------------------------------------------
#use the PTMH.03 and PTMHspss_clean datasets to compare and 
#create a clean non-imputed dataset. 

mean()


library(tidyverse)
library(readr)

PTMH.04 <- read.csv(PTMH.04)

library(readr)
PTMH.04 <- read_delim("ptmh/PTMH.04.csv", 
                      delim = "\t", escape_double = FALSE, 
                      col_names = TRUE, trim_ws = TRUE)
View(PTMH.04)

PTMH.05 <- PTMH.04



-----------#hierarchical regression models--base code--------------------------

DO NOT RUN YET. PREREG HAS NOT BEEN POSTED TO OSF. 


#create step1 models for all DVs 
#create step2 models for all DVs 

step1dys <- lm(i.dys ~ bfi.n + pid.neg, data="PTMH.05")




