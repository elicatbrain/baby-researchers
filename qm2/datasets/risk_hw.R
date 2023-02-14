# SETUP =============================================================================================================================================

# clear global environment

rm(list=ls())

# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# function to load libraries

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("sjPlot"),pkgTest)
lapply(c("readr"),pkgTest)
lapply(c("labelled"),pkgTest)
library(sjPlot)
library(readr)
library(labelled)

setwd("/Users/carolinelee/Desktop/Baby Researchers/qm2/datasets")

# IMPORT DATA =================================================================================

risk <- read_csv("Ale.risky.sex.csv")

risk_sd <- sd(risk$SYS, na.rm=TRUE)

mean(risk$SYS, na.rm=TRUE) + risk_sd # SYS score 1SD above mean = 25.33944

mean(risk$SYS, na.rm=TRUE) - risk_sd # SYS score 1SD below mean = 14.66056

risk$SYS_high_centered <- risk$SYS - 25.33944

risk$SYS_low_centered <- risk$SYS - 14.66056

summary(risk$SYS_high_centered, na.rm=TRUE)  

risk$SYS_HC_levels <- "Low"

risk$SYS_HC_levels[risk$SYS_high_centered > 0] <- "High"

risk$SYS_HC_levels <- to_factor(risk$SYS_HC_levels)

reg1 <- lm(zsertot ~ DERS_total + SYS_high_centered + DERS_total*SYS_high_centered, data = risk)

summary(reg1)

reg1_plot <- lm(zsertot ~ DERS_total + SYS_HC_levels + DERS_total*SYS_HC_levels, data = risk)

plot_model(reg1_plot, type = "pred", terms = c("DERS_total", "SYS_HC_levels"))

summary(risk$SYS_low_centered, na.rm=TRUE)










