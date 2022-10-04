
#######################################################################################.
# SCRIPT: RELIABILITY AND VALIDITY
# USE CASE: CLIENT VALUE (1 - 10) ON FIVE VALUES  - STATISTICAL ANALYSIS
# EXAMPLE:  ARE THESE MEASURES RELIABLE? DO THEY MEASURE "ONE THING"
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#     - EDX / Microsoft. Research Module 4. Lab 1 & 2
#######################################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

#install.packages("Hmisc")
# Load-multiple-packages-at-once
required_packages <- c("psych") # , "ggridges","ggplot2"
lapply(required_packages, library, character.only = TRUE)

#######################################################.
# 1 RELIABILITY    -----------------------------

# * 1.1 LOAD THE DATA - THREE MEASURES OF LOYALTY    ----------

path            <- "Data_Files"       
dat   <- read.csv(paste0(path,"//Module4Lab1_measurement.csv"))

head(dat,5)
summary(dat)

# do these five adjectives reliably form a single index of anything?

# * 1.2 CHECK CORRELATIONS ------------ 
round(cor(dat[-1]),2)

# The fourth adjective doesn't correlate well.

# * 1.3 PARALLEL ANALYSIS - CHECK IF THE FACTORS CLUSTER TOGETHER --------------

fa.parallel(dat, fm = 'minres', fa = 'fa')

# WE ARE MEASURING "ONE THING" (the first factor above 1)

# * 1.4 FACTOR ANALYSIS -----------------

fa(dat, nfactors = 1, fm="minres")$loadings

# * 1.5 RELIABILITY TEST ----

psych::alpha(dat)

# WE SHOULD EXCLUDE THE FACTOR THAT DOESN'T CORRELATE WELL

# * 1.6 HISTOGRAM OF VALUES ----

dat$Sentiment <-  rowSums(data.frame(dat$friendly, dat$inviting, dat$awesome, dat$pleasant))
hist(dat$Sentiment)

#######################################################.
# 2 VALIDITY -----------

# * 2.1 Read data --------
# Four variables: sentiment, word count, product rating, purchase likelihood

datvalidity <- read.csv(paste0(path,"//Module4Lab2_validity.csv"))

str(datvalidity)

# * 2.2 Correlation -----------

round(cor(datvalidity[-1]),4)

# * 2.3 P-values ----------

round(psych::corr.test(datvalidity)$p,4)

# END --------
