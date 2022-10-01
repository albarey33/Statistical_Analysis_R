
###################################################################.
# SCRIPT: FREQUENCY CLAIMS TO ANALYZE ONE VARIABLE
# USE CASE: VISUALIZE THE DISTRIBUTION OF ONE VARIABLE USING HISTOGRAM
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#                          - EDX / Microsoft. Module 3. Lab 1"
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

# Load-multiple-packages-at-once
required_packages <- c("psych", "ggplot2", "Rmisc")
lapply(required_packages, library, character.only = TRUE)

# 1 LOAD THE DATA ------------------------------------

dat <- data.frame(count=c(1,7,3,3,2,1,2,2,2,2,0,2,1,7,1,2,5,3,1,0,4,1,2,1,5,1,3,1,5,
                   5,1,3,1,4,2,3,1,4,1,5,3,1,6,1,1,2,1,2,3,0,2,1,0,1,6,1,2,3,
                   3,0,4,2,4,3,2,4,5,1,1,1,2,2,2,3,5,2,1,2,1,0,2,5,1,1,2,2,5,3,
                   6,2,4,6,6,2,3,0,3,3,0,3))

# 2 TABLE: TABULATION TABLE --------
table(dat$count)
as.matrix(table(dat$count)) # The same, vertically arranged

# PERCENTAGES
table(dat$count)/sum(!is.na(dat$count))

# ACCUMULATIVE PERCENTAGES
cumsum(table(dat$count)/sum(!is.na(dat$count)))

# BIND RECAP VALUES
tab1 <- cbind(table(dat$count),
              table(dat$count)/sum(!is.na(dat$count)),
              cumsum(table(dat$count)/sum(!is.na(dat$count)))
              )
              
colnames(tab1) <- c("Count", "Perc", "Cum.Perc")
tab1 

# 3 HISTOGRAM -----------

hist(dat$count)     # Base chart. By default gives a wrong bucket distribution

# Using ggplot2 instead, with binwidth=1

ggplot(data=dat, aes(x=count))+
  theme_light()+
  geom_histogram(color="black", fill="brown", binwidth = 1)+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+  # add labels
  scale_x_continuous(name="Cups of Coffee Per Day", breaks=0:10)+   # axis X
  scale_y_continuous(name="Frequency", breaks=seq(0, 50, 5), limits=c(NA, 30)) # axis Y

# 4 CENTRAL TENDENCY - SUMMARY --------

# calculate the Skewness

psych::skew(dat$count)

# 0.7552241 is within the acceptable range to consider this as normally distributed

# SUMMARY: MEAN, MEDIA, RANGE ------
summary(dat$count)

# calculate mean
mean(dat$count)
summary(dat$count)['Mean']

# calculate median
median(dat$count)
summary(dat$count)['Median']

# Calculate Confidence Interval
Rmisc::CI(dat$count)

# END -----------