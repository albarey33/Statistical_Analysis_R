
#######################################################################################.
# SCRIPT: FREQUENCY CLAIMS TO ANALYZE TWO GROUPS
# USE CASE: VISUALIZE THE COMPARISON OF TWO GROUPS - STATISTICAL ANALYSIS
# EXPERIMENT TO FIND OUT CAUSE-EFFECT RELATIONSHIPS
# EXAMPLE: ANALYSIS CAUSE-EFFECT IN STRESS-PRODUCTIVITY RELATIONSHIP ON EMPLOYEES.
# EXAMPLE: TWO IDENTICAL GROUPS, EXCEPT OF THE "STRESS REDUCTION TREATMENT" INTERVENTION
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#                     - EDX / Microsoft. Research Module 3. Lab 3 Causal Claims"
#######################################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

#install.packages("Hmisc")
# Load-multiple-packages-at-once
required_packages <- c("effsize", "ggridges","ggplot2")
lapply(required_packages, library, character.only = TRUE)

# 1 LOAD THE DATA - THREE MEASURES OF LOYALTY    -----------------------------

path            <- "Data_Files"       
dat   <- read.csv(paste0(path,"//Module3Lab3_causal.csv"))

head(dat,5)

# 2 COMPARE THE TWO GROUPS BY THEIR MEAN ------------ 
tapply(dat$prod, dat$group, mean, na.rm=T)
tapply(dat$prod, dat$group, sd, na.rm=T)

# 3 CHECK IF THE DIFFERENCE IS STATISTICALLY SIGNIFICANT --------

t.test(dat$prod ~ dat$group)
# t.test(prod ~ group, data=dat)  # alternative script, same results

# Conclusions
# This difference can happen only 0.2% of the time (p-value = 0.001673). 
# The difference is NOT due to chance.

# 3 VISUALIZE THE RESULTS. COMPARE THE TWO GROUPS BOX-PLOTS --------

ggplot(data=dat, aes(x=group, y=prod, fill=group))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(width=0.4)+
  theme_light()

# 4 VISUALIZE THE RESULTS. COMPARE THE TWO GROUPS VIOLIN PLOT -----------

ggplot(data=dat, aes(x=group, y=prod, fill=group))+
  geom_violin(alpha=0.5)+
  geom_jitter(width=0.4)+
  theme_light()

# 5 EVALUATE THE EFFECT SIZE (COHEN's d or COHEN's difference) ---------

effsize::cohen.d(prod ~ group, data=dat)

# CONCLUSION --------------------
# The stress reduction intervention had an effect, although the effect is minimal.

# END -----------