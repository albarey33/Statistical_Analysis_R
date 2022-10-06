
###################################################################.
# SCRIPT: FREQUENCY CLAIMS TO ANALYZE TWO OR MORE VARIABLES - CORRELATION
# USE CASE: VISUALIZE THE DISTRIBUTION OF ONE VARIABLE USING HISTOGRAM
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#                          - EDX / Microsoft. Module 3. Lab 2"
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

#install.packages("Hmisc")
# Load-multiple-packages-at-once
required_packages <- c("psych", "stringr","Hmisc","ggplot2", "Rmisc")
lapply(required_packages, library, character.only = TRUE)

# 1 LOAD THE DATA - THREE MEASURES OF LOYALTY    -----------------------------

path            <- "PopulationSamples"       
dat   <- read.csv(paste0(path,"//Module3Lab2_loyaltydata.csv"))

head(dat,5)

# 2 CHECK MAIN PARAMETERS MEAN, MEDIAN, RANGE ------------ 
summary(dat)

# 3 ANALYSIS OF CORRELATION --------
round(cor(dat[,-1]),3)    

# Variables are not highly correlated with each other

# 4 VISUALIZE EACH CORRELATION ----------

ggplot(data=dat, aes(x=loyalty1, y=loyalty2))+
  geom_jitter(alpha=0.3)+
  theme_light()

ggplot(data=dat, aes(x=loyalty1, y=loyalty3))+
  geom_jitter(alpha=0.3)+
  theme_light()

ggplot(data=dat, aes(x=loyalty2, y=loyalty3))+
  geom_jitter(alpha=0.3)+
  theme_light()

# 5 PLOT ALL CORRELATIONS --------------

plot(dat[,-1])

# 6 CORRELATION TEST - P VALUES ---------

psych::corr.test(dat[,-1])

rcorr(as.matrix(dat[,-1]))

# 7 CONFIDENCE INTERVAL ----------

rcorr(as.matrix(dat[,-1]))

# 8 COR.TEST (base package) ----------------

cor.test(dat$loyalty1, dat$loyalty2)

cor1 <- cor.test(dat$loyalty1, dat$loyalty2)

names(cor1)

cor1$conf.int[1]
cor1$conf.int[2]

# CONCLUSION: The correlations are not strong. 

# END -----------