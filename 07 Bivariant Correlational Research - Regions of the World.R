
#######################################################################################.
# SCRIPT: BIVARIANT CORRELATIONAL RESEARCH
# USE CASE: REGIONS OF THE WORLD - CORRELATION BETWEEN ALL PARAMETERS
# EXAMPLE:  
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#     - EDX / Microsoft. Research Module 5. Lab 1
#######################################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

#install.packages("Hmisc")
# Load-multiple-packages-at-once
required_packages <- c("corrplot","psych","ggplot2") # , "ggridges","ggplot2"
lapply(required_packages, library, character.only = TRUE)

#######################################################.
# 1 LOAD THE DATA - THREE MEASURES OF LOYALTY    ----------

path            <- "Data_Files"       
dat   <- read.csv(paste0(path,"//Module5Lab5_regionalhappy.CSV"))

head(dat,5)

# Resume Names
names(dat) <- c("Happiness", "GDP", "Family", "Life.Expect", 
                "Freedom", "Generosity", "Trust.Gov", "Dystopia")

head(dat)

# 2 CORRELATION TWO VARIABLES  ------------ 
cor(dat$Happiness, dat$Life.Expect)
# 0.7819506 is a large correlation by the next Cohen's Correlation table
  # Correlation 	   Meaning
  # 1. 	0 - 0.1 	  Negligible
  # 2. 	0.1 - 0.3 	Small
  # 3. 	0.3 - 0.5 	Medium
  # 4. 	0.50 + 	    Large

# Scatter Plot 
ggplot(data=dat, aes(x=Happiness, y=Life.Expect))+
  geom_point()+
  theme_light()

# 3 SIGNIFICANCE TEST FOR THIS CORRELATION =====

cor.test(dat$Happiness, dat$Life.Expect)

# If the null is true, this would happen 2.2e-16 percent of the times (p-value)
# We reject the null hypothesis. There is a correlation between these two variables.
# CI  0.7120831 0.8364830 . This range includes the correlation 95% of the time.

# 4 CAVEAT: CHECK NORMALITY - SKEWNESS

hist(dat$Happiness)
hist(dat$Life.Expect)
psych::skew(dat$Happiness)   # Close to zero: Very normal
psych::skew(dat$Life.Expect) # -0.5668279 Acceptable (+-1)

# 5 CORRELATION AMONG MANY VARIABLES ==============

cor(dat[,c("Happiness", "Life.Expect", "GDP", "Generosity")])
# Generosity is seemengly less related

# SIGNIFICANCE TEST MULTIPLE VARIABLES ================
psych::corr.test(dat[,c("Happiness", "Life.Expect", "GDP", "Generosity")])

# 6 CLUSTERING ==================
# Matrix cor all variables
cors <- cor(dat)

# Heatmap (dendrogram: lines above that connect two groups)
heatmap(cors, symm = TRUE)
# Cluster (highly correlated) in Happiness, Life.Expect, GDP, Family

# 7 CORRELATION PLOT ==============
# This graph gives more emphasis on the individuals relationships
corrplot(cors)

# With numbers in one diagonal
corrplot.mixed(cors, order="hclust")

# Add a grid of p-values, excluding non significant.
corrplot.mixed(cors, 
               p.mat=corr.test(dat)$p, sig.level=0.05)

# END --------
