
#######################################################################################.
# SCRIPT: BETWEEN SUBJECTS EXPERIMENTS - A/B TEST
# USE CASE: REGIONS OF THE WORLD - CORRELATION BETWEEN ALL PARAMETERS
# EXAMPLE: CHOOSE AMONG THREE LOGOS TESTED ON A SAMPLE OF 100 CUSTOMERS
# EXAMPLE SOURCE: "Course: Data Science - Research Methods in R
#                  - EDX / Microsoft. Research Module 5. Lab 3 and 4
#######################################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

#install.packages("Hmisc")
# Load-multiple-packages-at-once
required_packages <- c("pwr","psych","effsize","ggplot2") # , "ggridges"
#required_packages <- c("corrplot","psych","ggplot2") # , "ggridges","ggplot2"
lapply(required_packages, library, character.only = TRUE)


# * 0 LOAD THE DATA - THREE MEASURES OF LOYALTY    ----------

path            <- "Data_Files"       
dat   <- read.csv(paste0(path,"//Module5Lab5_logos.CSV"), stringsAsFactors = T)

head(dat,5)
dim(dat)
str(dat)

# 100 customers completed a survey about a randomly selected logo
# These are all positive adjectives: friendly, inviting, interesting, positive, pleasant
# All these answers will be combined into a single positive scale for comparison.

summary(dat)
# There is one NA (missing variables) in the logo variable

#######################################################.
# 1 SCORE THE SCALE   ------------

# * 1.1 Correlation among all variables   ------------ 
round(cor(dat[,2:6]),2)   # Later Principal Component Analysis

# * 1.2 Add variable "Sentiment" as average of variables ====
dat$sentiment <- rowMeans(data.frame(dat$friendly, dat$inviting, 
                    dat$interesting, dat$positive, dat$pleasant))
# Alternative Method
# dat$sentiment <- rowMeans(subset(dat, select=c("friendly", "inviting", 
#                                   "interesting", "positive", "pleasant")))

# * 1.3 Check the new variable "Sentiment" =====
mean(dat$sentiment)
sd(dat$sentiment)
ggplot(data = dat, aes(x=sentiment))+
  geom_histogram(color="blue", fill="light blue", bins = 15)+
  theme_gray()+
  scale_x_continuous(name="Sentiment")+
  ggtitle("Histogram of Sentiment")

# * 1.4 Reliability Analysis - Score the Scale - ALPHA function =====

summary(psych::alpha(
  data.frame(dat$friendly, dat$inviting, dat$interesting, dat$positive, dat$pleasant)
  ))
# raw_alpha is 0.94 reliable

# 2 VISUALIZE THE DATA AND RUN DESCRIPTIVES =========

# In between-subjects designs we compare means across groups

table(dat$logo, useNA = "always")

ggplot(data=dat[!is.na(dat$logo),], aes(x=logo, y=sentiment, fill=logo))+
  geom_boxplot(alpha=0.20, color="black")+
  geom_jitter(alpha=0.5, color="black", fill="grey90", width=0.5)+
  theme_gray()+
  scale_y_continuous(name = "sentiment")+
  scale_x_discrete(name = "logo")+
  scale_fill_discrete(name="Logo")

# Check incomplete cases
dat[!complete.cases(dat),]

# 3 DETAILED STATISTICS - DESCRIPTIVE STATISTICS: MEAN STANDARD DEVIATION ======
table_1 <- rbind(
round(tapply(X = dat$sentiment, INDEX = dat$logo, FUN = mean, na.rm=TRUE),2),
round(tapply(X = dat$sentiment, INDEX = dat$logo, FUN = sd, na.rm=TRUE),2)
)
rownames(table_1) <- c("mean", "sd")
t(table_1)

# 4 INFERENTIAL TETS FOR TWO GROUPS: A and B
dat2 <- dat[(dat$logo=="Logo A" | dat$logo=="Logo B"),]
t.test(dat2$sentiment ~ dat2$logo)
# t.test(sentiment ~ logo, data=dat2) # alternative syntaxis
# p-value = 0.7046 We fail to reject the null hypothesis 

# 5 CHECKING THE EFFECT SIZE WITH EFFSIZE PACKAGE

#   Cohen's Difference table
  #  d Value  	   Meaning
  # 1. 	0 - 0.2 	  Negligible
  # 2. 	0.2 - 0.5 	Small
  # 3. 	0.5 - 0.8 	Medium
  # 4. 	0.80 + 	    Large

effsize::cohen.d(dat2$sentiment ~ dat2$logo)
# Cohen's d estimate: 0.09475291 (negligible)

# 6 FINAL REPORT

ggplot(data=dat2[!is.na(dat2$logo),], aes(x=logo, y=sentiment, fill=logo))+
         geom_boxplot(alpha=0.20, color="black")+
         geom_jitter(alpha=0.5, color="black", fill="grey90", width=0.4)+
         theme_gray()+
         scale_y_continuous(name = "sentiment")+
         scale_x_discrete(name = "logo")+
         scale_fill_discrete(name="Logo")+
         ggtitle("Sentiment: Logo A vs Logo B")
table_1        
         
# 4 ADDENDUM: POWER CONSIDERATIONS ============
# Sample Sizes
table(dat2$logo)

# Feed Information
pwr.t2n.test(n1=33, n2=32, power = 0.80)
# We could ONLY reliably detect effects higher than d = .71 ("moderate-large").

# Let's suppose we required to detect a smaller effect size, say d=0.20. Which
# sample size is required?
pwr.t.test(d=0.20, power = 0.80)
# n = 394 sample size required per logo

# 5 INFERENTIAL TEST FOR MULTIPLE GROUPS - ANOVA ============

# ANOVA computes the F statistic to evaluate significant differences in the 
# variances, to determine if we should reject the Null hypothesis.

# Save the anova model, then run a summary
anovamodel1 <-  aov(dat$sentiment ~ dat$logo)
summary(anovamodel1)

# For the statement "All the means of logo groups are equal", we get a 
# p-value of 6.25e-06. This is highly statistically significant.

# * 5.1 Tukey test to compare the groups one by one ============
TukeyHSD(anovamodel1)
# Logo C shows significant differences with the other two Logos.
# Logo C has reduced sentiment.

#######################################.
# 6 FACTORIAL DESIGNS ================
# Evaluate the effect of the logos by gender

# Open two packages
# lapply(c("ez", "phia"), library, character.only = TRUE)
# library("ez")
library("phia")

# Split the groups of logos by also the gender
ggplot(data=dat[!is.na(dat$logo),], aes(x=logo, y=sentiment, fill=sex))+
  geom_boxplot(alpha=0.20, color="black")+
  geom_jitter(alpha=.5, color="black", fill="grey90", width=0.20)+
  theme_light()+
  scale_y_continuous("sentiment")+
  scale_x_discrete("logo")+
  scale_fill_discrete("sex")

table_2 <- rbind(
  round(tapply(X = dat$sentiment, INDEX = list(dat$logo, dat$sex), FUN = mean, na.rm=TRUE),2),
  round(tapply(X = dat$sentiment, INDEX = list(dat$logo, dat$sex), FUN = sd, na.rm=TRUE),2)
)
#rownames(table_2) <- c("mean", "mean", "mean", "sd", "sd", "sd")
t(table_2)

# * 6.1 ANOVA ====================

anovamodel2 <-  aov(dat$sentiment ~ dat$logo + dat$sex)
summary(anovamodel2)
TukeyHSD(anovamodel2)

#head(dat)
#library("ez")

# generate an id variable
#dat$idvar <- as.factor(1:nrow(dat))

# ezANOVA(data=dat,
#        wid=.(idvar),
#        dv=.(sentiment),
#        between=.(sex, logo),
#        type=3)

#install.packages('devtools',type='source'))

dat$logosex <- paste(dat$logo,dat$sex, sep = "/")
dat33 <- dat[!is.na(dat$logo),]
anovamodel3 <-  aov(dat33$sentiment ~ dat33$logosex)
summary(anovamodel3)
TukeyHSD(anovamodel3)

# testInteractions(mod$aov)

# END --------

