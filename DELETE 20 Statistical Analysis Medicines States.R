
########################################################################.
##################### STATISTICAL ANALYSIS  - SMALL SAMPLES T-TEST #####.
## SCRIPT: t-test T-TEST t.test()
#########################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "ggplot2", "lubridate", "data.table", "tidyverse")
lapply(required_packages, library, character.only = TRUE)

 
# 1 READ THE DOWNLOADED DATA CSV FILES ------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"       

# Read sample files using full path and regex
df_twoyears <- read.csv(list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Medicines_States.*?.csv")))

df_twoyears$Date <- lubridate::mdy(format(df_twoyears$Date, 
                                              format="%m/%d/%Y"))

head(df_twoyears)
str(df_twoyears)

# * 1.1 Calculation of Total sales ----
df_twoyears <- df_twoyears %>% mutate(Total_Sales = Green_Medicine + Orange_Medicine)

# * 1.2 Calculation of Revenue ----
df_twoyears <- df_twoyears %>% mutate(Revenue = Price * Total_Sales)

str(df_twoyears)

# * 1.1 BY YEARS ------
df_twoyears$day <- day(df_twoyears$Date)
df_2016 <- df_twoyears[year(df_twoyears$Date) == "2016",]
df_2015 <- df_twoyears[year(df_twoyears$Date) == "2015",]


#########################################################################################.
# 2 ONE SAMPLE T-TEST - Comparison Sales with previous years --------

# One sample T-Test tests if the given sample of observations 
# could have been generated from a population with a specified mean.
# It is typically implemented on small samples.

# t = (x - mu ) / standard error 
# where standard error ->   standard_deviation / sqrt(sample number: n) 
# degrees of freedom: n - 1

# Historical data 

hist_sales <-data.frame(Total_Sales = rnorm(100,mean=200,sd=50),
                        e = 'historical')

str(hist_sales)
mean(hist_sales$Total_Sales)

# combine to plot both histograms graph 

combo <- rbind(df_2016 %>% mutate(e = 'sample') %>% select(Total_Sales, e),
                  hist_sales)

ggplot(combo, aes(Total_Sales, fill = e)) +
  scale_fill_manual(values=c("blue", "red"))+
  geom_histogram(alpha=0.5,binwidth=0.5,position="identity")

# Density plot

ggplot(combo, aes(Total_Sales, fill = e)) + 
  geom_density(alpha = 0.2) + geom_vline (xintercept = 200, size=0.5, color="red")

# HYPOTHESIS TESTING: ONE SAMPLE t-test ------

# t = (x - mu ) / standard error 
# where standard error ->   standard_deviation / sqrt(sample number: n) 
# degrees of freedom: n - 1

sample_mean <- mean(df_2016$Total_Sales)
sample_mean
n_sample <- length(df_2016$Total_Sales)
n_sample
stdev <- sd(df_2016$Total_Sales)
stdev

t_result <- (sample_mean - 200)/(stdev/sqrt(n_sample-1))
t_result

# T-TEST two.sided --------

# NULL HYPOTHESIS: The sample and the historical sales have the same mean
# ALTERNATE HYPOTHESIS: The sample and the historical sales have different mean

t.test(x=df_2016$Total_Sales, mu=200, alternative = "two.sided")
# p-value = 0.726 probability that I am wrong with the alternate hypothesis
# The null hypothesis can not be rejected.

# T-TEST Lower -------- 196 (sample) vs 200 (historical)

# NULL HYPOTHESIS: The sample is NOT lower than the historical sales
# ALTERNATE HYPOTHESIS: The sample is LOWER than the the historical sales mean

t.test(x=df_2016$Total_Sales, mu=200, alternative = "less")
# The null hypothesis can not be rejected.

t.test(x=df_2016$Total_Sales, mu=211, alternative = "less")
# The null hypothesis is rejected.


# T-TEST Greater --------

# NULL HYPOTHESIS: The sample is NOT greater than the historical sales
# ALTERNATE HYPOTHESIS: The sample is GREATER than the the historical sales mean

# t.test(x=df_2016$Total_Sales, mu=180, alternative = "greater")
# The null hypothesis can not be rejected.


# 
t.test(df_2016$Total_Sales,hist_sales$Total_Sales)

# Other example car mileage ------

# https://www.machinelearningplus.com/statistics/one-sample-t-test/
# For example: A car manufacturer claims that their 
# cars give a highway mileage of 20kmpl on an average. 
# You sample 12 cars, measure their mileage
# Use the T-test to determine if the manufacturer’s claim is true.

x <-  c(21.5, 24.5, 18.5, 17.2, 14.5, 23.2, 22.1, 20.5, 19.4, 18.1, 24.1, 18.5)

# NULL HYPOTHESIS: The averages sample vs standard are ~ 20 kmpl
# Alternate hypothesis: The averages differ / true mean is not equal to 20

t.test(x=x, mu=20, alternative = 'two.sided', conf.level = 0.95)

# The null hypothesis can not be rejected / is accepted

# One sided
# NULL HYPOTHESIS: The sample mileage average  is >= standard 20 kmpl
# Alternate hypothesis: The sample average is less than 20 kmpl

t.test(x=x, mu=20, alternative = 'less', conf.level = 0.95)

# The null hypothesis can not be rejected / is accepted

# 
# # One sided
# # NULL HYPOTHESIS: The sample mileage average  is >= standard 20 kmpl
# # Alternative hypothesis: The sample average is less than 20 kmpl
# 
# t.test(x=x, mu=20, alternative = 'greater', conf.level = 0.95)
# 
# # The null hypothesis can not be rejected / is accepted

#########################################################################################.
# 3 TWO SAMPLE T-TEST - Comparison of two different variables within the same data ------

# NULL HYPOTHESIS: THE MEANS ARE EQUAL
# ALTERNATE HYPOTHESIS: The means differ.

# Question: Were the Green_Medicine sales higher than the the Orange Medicine

# * 3.1 Two Sample Boxplot ------

dfboxplot <- rbind(
df_2016 %>% mutate(e = "Green_Medicine") %>% select(medicine=Green_Medicine,e),
df_2016 %>% mutate(e = "Orange_Medicine") %>% select(medicine=Orange_Medicine,e)
)

ggplot(data = dfboxplot, 
       aes(x = e, y = medicine)) +
  geom_boxplot() +
  xlab("medicine") +
  ylab("sales") +
  labs(title = "Comparison Medicines")


# * 3.2 T-test --------

# NULL hypothesis: Both means are statistically equal
# alternative hypothesis: true difference in means is not equal to 0

t.test(df_2016$Green_Medicine, 
       df_2016$Orange_Medicine, 
       alternative = "two.sided", var.equal=TRUE)

t.test(df_2016$Green_Medicine, 
       df_2016$Orange_Medicine, 
       alternative = "greater", var.equal=TRUE)

t.test(df_2016$Orange_Medicine, 
       df_2016$Green_Medicine,
       alternative = "less", var.equal=TRUE)

# The null hypothesis is rejected.

# * 3.3 SIMILAR RESULTS IN EXCEL DATA ANALYSIS ADD-IN -------
# Green_Medicine	Orange_Medicine
# Mean	116.15625	          80
# Variance	666.8457661	   478
# Observations	32	        32
# Pooled Variance	572.4228831	
# Hypothesized Mean Difference	0	
# df	62	
# t Stat	6.044840954	            # <<<<<<<<< Same t Stat
# P(T<=t) one-tail	4.67378E-08	    <<<<<<<<< p-value = 4.674e-08
# t Critical one-tail	1.669804163	
# P(T<=t) two-tail	9.34756E-08	#   <<<<<<<<< p-value = 9.348e-08
# t Critical two-tail	1.998971517	

#####################################################################.
# 4 PAIRED TWO SAMPLE T-TEST ----------

# * 4.1 Plot Sales in same days of two periods -------
#create plot with two lines

dfTwoLinePlot <- cbind(
  df_2015 %>% select(Day=day,Sales_2015=Total_Sales),
  df_2016 %>% select(Sales_2016=Total_Sales)
)

str(dfTwoLinePlot)

ggplot(dfTwoLinePlot, aes(x = Day)) + 
  geom_line(aes(y = Sales_2016 , color = '2016')) + 
    geom_line(aes(y = Sales_2015 , color = '2015'))  


# * 4.2 Paired Two Sample T-Test -------

# NULL HYPOTHESIS: No statistically significant difference
# Alternative Hypothesis: These is a difference

t.test(df_2016$Total_Sales, 
      df_2015$Total_Sales, paired = TRUE, alternative = "two.sided")
# The null hypothesis is rejected 

# t-Test: Paired Two Sample for Means		
# 
#         2016	              2015
# Mean	196.9354839	        174.7419355
# Variance	2325.929032	   2054.931183
# Observations	31	             31
# Pearson Correlation	0.955668532	
# Hypothesized Mean Difference	0	
# df	30	
# t Stat	8.689344068	
# P(T<=t) one-tail	5.41847E-10	
# t Critical one-tail	1.697260887	
# P(T<=t) two-tail	1.08369E-09	
# t Critical two-tail	2.042272456	

#####################################################################.
# 5 ANALYSIS OF VARIANCE (ANOVA) -------------

Green_VA <- df_2016 %>% filter(Location=="VA") %>% select(Green_Medicine) %>% pull()
Green_NC <- df_2016 %>% filter(Location=="NC") %>% select(Green_Medicine) %>% pull() 
Orange_VA <- df_2016 %>% filter(Location=="VA") %>% select(Orange_Medicine) %>% pull()
Orange_NC <- df_2016 %>% filter(Location=="NC") %>% select(Orange_Medicine) %>% pull() 
length(Green_VA) <- 16
length(Orange_VA) <- 16

df_AOV <- data.frame(cbind(Green_VA= Green_VA, Green_NC=Green_NC, 
                           Orange_VA=Orange_VA, Orange_NC=Orange_NC))
df_AOV
df_2016 <- data.table(df_2016)
df_2016_melted <- melt(df_2016, id.vars="Location", 
                       measure.vars = c("Green_Medicine", "Orange_Medicine"))
df_2016_melted <- df_2016_melted %>% rename(Medicines = variable)
df_2016_melted

# * 5.1 One Way ANOVA test ------
df_aov <- aov(df_2016_melted$value ~ factor(df_2016_melted$Location))
summary(df_aov)

df_aov <- aov(df_2016_melted$value ~ factor(df_2016_melted$Medicines))
summary(df_aov)

# * 5.2 Performing Two Way ANOVA test in R ------
df_aov <- aov(df_2016_melted$value ~ factor(df_2016_melted$Location) * factor(df_2016_melted$Medicines))
summary(df_aov)


# * 5.3 Another example of ANOVA -----

# Variance in mean within group and between group
boxplot(mtcars$disp~factor(mtcars$gear),
        xlab = "gear", ylab = "disp")

# Step 1: Setup Null Hypothesis and Alternate Hypothesis
# H0 = mu = mu01 = mu02(There is no difference
# between average displacement for different gear)
# H1 = Not all means are equal

# Step 2: Calculate test statistics using aov function
mtcars_aov <- aov(mtcars$disp~factor(mtcars$gear))
summary(mtcars_aov)

# Step 3: Calculate F-Critical Value
# For 0.05 Significant value, critical value = alpha = 0.05

# Step 4: Compare test statistics with F-Critical value
# and conclude test p < alpha, Reject Null Hypothesis


#####################################################################.
# 6 LINEAR REGRESSION MULTIPLE VARIABLES ----------------------------

head(df_2016)

lm(formula = Total_Sales ~ Temperature + Ads_Papers + Price, data = df_2016,  )

# Results
# Coefficients:
#   (Intercept)  Temperature   Ads_Papers        Price  
# -18.8297       0.9343       1.6206     -85.8801  

# Results in Excel - Data Analysis tools
# Coefficients
# Intercept	-18.82972
# Temperature	0.934292297
# Ads_Papers	1.620582414
# Price	-85.88007278

# END -----------



