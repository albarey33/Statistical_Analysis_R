
############################################################.
# STATISTICAL ANALYSIS  - TWO PRODUCTS                     
# Example: Two Products Sales, affected by                 
# two numerical variables And one Categorical (Method)
############################################################.

# 0 INSTALL PACKAGES / CALL LIBRARIES -------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "ggplot2", "lubridate", 
                       "data.table", "tidyverse", "reshape2", "psych")
lapply(required_packages, library, character.only = TRUE)

# 1 READ THE DOWNLOADED DATA CSV FILES -------------------------

# Paths # Location of Source files
setwd("D:/DataRepository/StatisticalAnalysis")
getwd()
# Read data file
df_twoprodsBothYears <- read.csv("Two_Products_data.csv")

df_twoprodsBothYears$Date <- as.Date(df_twoprodsBothYears$Date, format = "%m/%d/%Y")

# Only Year 2016
df_twoprods          <- df_twoprodsBothYears %>% filter(year(Date) == 2016)

head(df_twoprods)
str(df_twoprods)

# Convert Date to Date format
df_twoprods$Date <- as.Date(df_twoprods$Date, format = "%m/%d/%Y")

############################################################################.
# 2 DESCRIPTIVE STATISTICS --------------

# Exploratory Data analysis (EDA): Checking all variables and their interaction.

head(df_twoprods)
str(df_twoprods)
summary(df_twoprods)

# * 2.1 Calculate statistics of Numerical Variables ------------------

# Function to calculate statistics
# Using psych::describe to get the statistics kurtosis, skewness
fx_get_statistics <- function(data, numeric_vector){
  results_df <- data.frame()   # Create an empty df to store the results
  for (ii in numeric_vector) {
    desc <- psych::describe(data[, ii])
    rownames(desc) <- ii
    results_df <- rbind(results_df, desc)
  }
 print(t(results_df))
}

# Numeric columns 
str(df_twoprods)

num_vector <- c("Product1", "Product2", "StockPrice", "MarketingSpend")

fx_get_statistics(df_twoprods, num_vector)

# As the skewness and kurtosis all the numerical variables 

# Reshape the data using melt

# * 2.2 Create a line chart for all numerical variables ---------

melted_data <- melt(df_twoprods[,c("Date", num_vector)], 
                  id.vars = "Date") 
head(melted_data)

ggplot(data = melted_data, aes(x = Date, y = value, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Line Chart for Numeric Variables Over Time",
    x = "Date",
    y = "Value"
  ) +
  scale_x_date(date_labels = "%Y/%m/%d", date_breaks = "1 week") +
  scale_color_manual(values = c("Product1" = "lightblue", 
                                "Product2" = "pink", 
                                "StockPrice" = "green", 
                                "MarketingSpend" = "purple"))

# * 2.3 Scatter Plot Sales vs Marketing Spend ------------

ggplot(data = df_twoprods, aes(x = MarketingSpend, 
                               y = Sales, color = Method)) +
  geom_point(size = 3) +  # Use points for the scatter plot
  # Add linear regression line
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth=0.5) +  
  labs(
    title = "Scatter Plot of Sales vs. MarketingSpend",
    x = "Marketing Spend",
    y = "Sales"
  ) +
  #theme_minimal() + 
  expand_limits(x = 70, y = 100)

# * 2.4 Scatter Plot Sales vs Stock Price ------------

ggplot(data = df_twoprods, aes(x = StockPrice, 
                               y = Sales, color = Method)) +
  geom_point(size = 3) +  # Use points for the scatter plot
  # Add linear regression line
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth=0.5) +  
  labs(
    title = "Scatter Plot of Sales vs. StockPrice",
    x = "StockPrice",
    y = "Sales"
  ) +
  #theme_minimal() + 
  expand_limits(x = 70, y = 100)

# * 2.5 Box Plots of Two Products -------------

ggplot(data = melted_data[melted_data$variable %in% 
                            c("Product1", "Product2"),], 
              aes(x = variable, y = value, fill = variable)) +
   geom_boxplot() +
  labs(
    title = "Product1 and Product2 Comparison",
    x = "Product",
    y = "Sales" ) +
    scale_fill_manual(values = c("Product1" = "lightblue", 
                                 "Product2" = "pink")) 
    #theme_minimal()


# * 2.6 Histograms of Two Products -------------

# Combine Histogram and Density Plots

# Create a combined histogram and density plot for Product1 and Product2
ggplot(data = melted_data[melted_data$variable %in% 
                            c("Product1", "Product2"), ], 
       aes(x = value, fill = variable)) +
  scale_fill_manual(values = c("Product1" = "lightblue", "Product2" = "pink")) +
  geom_histogram(alpha = 0.5, binwidth = 10, position = "identity") +
  labs(
    title = "Histogram: Product1, Product2",
    x = "Value",
    y = "Frequency"
  ) 
  #theme_minimal() +
  #scale_x_continuous(limits = c(0, 240), breaks = seq(0, 240, by = 20)) +
  #scale_y_continuous(breaks = seq(0, 20, by = 5))  # limits = c(0, 0.01),

# Create a density plot
ggplot(data = melted_data[melted_data$variable %in% 
                            c("Product1", "Product2"), ], 
       aes(x = value, fill = variable)) +
  scale_fill_manual(values = c("Product1" = "lightblue", "Product2" = "pink")) +
    geom_density(alpha = 0.2, aes(y = after_stat(scaled))) +
    labs(
    title = "Density Plot: Product1, Product2",
    x = "Value",
    y = "Frequency"
  ) 

# * 2.7 Checking the Sales by Method and Week day -----------

df_twoprods %>% group_by(paste(wday(df_twoprods$Date), 
                         df_twoprods$Day, sep = "-"), Method) %>% 
                         summarise(Total_Sales = sum(Sales))

# Summarize the data and create the bar chart
df_twoprods %>%
  group_by(DayOfWeek = paste(wday(Date), Day, sep = "-"), Method) %>%
  summarise(Total_Sales = sum(Sales), .groups = "keep") %>%
  ggplot(aes(x = DayOfWeek, y = Total_Sales, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Sales by Day of the Week and Method",
    x = "Day of the Week",
    y = "Total Sales"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# * 2.8 Bar chart Weekday, method, with detail of Products -------

# Summarize the data for total sales by Daynum, Method, Product1, and Product2

df_twoprods %>%
  mutate(Daynum = paste(wday(Date), Day, sep = "-")) %>%
  pivot_longer(cols = c(Product1, Product2), names_to = "variable", values_to = "Total_Sales") %>%
  group_by(Daynum, Method, variable) %>%
  summarise(Total_Sales = sum(Total_Sales), .groups = "keep") %>%
  ggplot(aes(x = Daynum, y = Total_Sales, fill = Method, color = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Sales by Day of the Week and Method",
    x = "Day of the Week",
    y = "Total Sales"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################################################.
# 3 COMPARATIVE STATISTICS --------
# Comparison of Sales vs Historical Data (Previous Days) 
# If the current data could have been generated from a population 
# with a specified mean.
# T-test: It is typically implemented on small samples.

# t = (x - mu ) / standard error 
# where standard error ->   standard_deviation / sqrt(sample number: n) 
# degrees of freedom: n - 1

# Historical data 
# Sales on Previous 100 periods
# Assuming historical daily sales of 125 units
set.seed(105)
hist_sales <-data.frame(variable = 'historical', 
                 value = rnorm(100,mean=125,sd=50)) 
str(hist_sales)
historical_mean <- mean(hist_sales$value)
historical_mean

# combine to plot both histograms graph 

prod1_hist <- bind_rows(
              bind_cols(variable="Product1", value=df_twoprods$Product1),
              bind_cols(variable="Product2", value=df_twoprods$Product2),
              hist_sales
                        )

ggplot(data = prod1_hist, 
       aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(
    title = "Products vs Historical Sales",
    x = "Comparison Products vs Historical Daily Sales",
    y = "Sales" ) +
  scale_fill_manual(values = c("historical" = "lightgray", 
                               "Product1" = "lightblue", 
                               "Product2" = "pink")) 
  #theme_minimal()

# * 3.1 Hypothesis Testing: ONE SAMPLE t-test ------

# t = (x - mu ) / standard error 
# where standard error ->   standard_deviation / sqrt(sample number: n) 
# degrees of freedom: n - 1

sample_mean <- mean(df_twoprods$Product1)
sample_mean
n_sample <- length(df_twoprods$Product1)
n_sample
stdev <- sd(df_twoprods$Product1)
stdev

t_result <- (sample_mean - historical_mean)/(stdev/sqrt(n_sample-1))
t_result

# T-TEST two.sided

# NULL HYPOTHESIS: The sample and the historical sales have the same mean
# ALTERNATE HYPOTHESIS: The sample and the historical sales have different mean

t.test(x=df_twoprods$Product1, mu=historical_mean, alternative = "two.sided")
# p-value = 0.1109 probability that I am wrong with the alternate hypothesis
# The null hypothesis can not be rejected.

# T-TEST Lower 

# NULL HYPOTHESIS: The sample is NOT lower than the historical sales
# ALTERNATE HYPOTHESIS: The sample is LOWER than the the historical sales mean

t.test(x=df_twoprods$Product1, mu=historical_mean, alternative = "less")
# The null hypothesis can not be rejected.

t.test(x=df_twoprods$Product2, mu=historical_mean, alternative = "less")
# The null hypothesis is rejected.The product2 sales are lower than historical


#########################################################################################.
# * 3.2 Hypothesis Testing: TWO SAMPLE T-TEST --------------- 
# Comparison of two different variables within the same data
# NULL HYPOTHESIS: THE MEANS ARE EQUAL
# ALTERNATE HYPOTHESIS: The means differ.

# Question: Were the Product1 sales higher than the the Product2 sales?

#######################################################.
# NULL hypothesis: Both means are statistically equal
# alternative hypothesis: true difference in means is not equal to 0

t.test(df_twoprods$Product1, 
       df_twoprods$Product2, 
       alternative = "two.sided", var.equal=TRUE)

# The probability to see these results are  p=1.879e-07.
# The null hypothesis is rejected. 

#######################################################.
# NULL hypothesis: Product1 sales are less than Product2 sales (negative difference)
# alternative hypothesis: true difference in means is greater than 0
t.test(df_twoprods$Product1, 
       df_twoprods$Product2, 
       alternative = "greater", var.equal=TRUE)
# The probability to see these results are  p=9.393e-08
# The null hypothesis is rejected.

#######################################################.
# NULL hypothesis: Product1 sales are greater than Product2 sales (positive difference)
# alternative hypothesis: true difference in means is less than 0
t.test(df_twoprods$Product1, 
       df_twoprods$Product2, 
       alternative = "less", var.equal=TRUE)
# The null hypothesis can not be rejected.

# * 3.3 Similar results in Excel Data Analysis Add-In -------

# t-Test: Two-Sample Assuming Equal Variances		
#                Product1	Product2
# Mean	     116.5806452	80.35483871
# Variance   	683.1182796	489.7698925
# Observations    	31	31
# Pooled Variance	586.444086	
# Hypothesized Mean Difference	0	
# df	60	
# t Stat	5.889393952	
# P(T<=t) one-tail	9.39311E-08	
# t Critical one-tail	1.670648865	
# P(T<=t) two-tail	1.87862E-07	
# t Critical two-tail	2.000297822	

#####################################################################.
# 4 ASSOCIATIVE STATISTICS ----------

# * 4.1 paired Two Sample T-Test------
# Paired Comparison of Sales for the same days on years 2015 - 2016
# The only difference is that there were not MarketingSpend on 2015
# NULL HYPOTHESIS: No statistically significant difference
# Alternative Hypothesis: These is a difference
# alternative hypothesis: true mean difference is not equal to 0
# Compare Sales on the same dates, 

# Only Year 2015
df_twoprods_2015  <- df_twoprodsBothYears %>% filter(year(Date) == 2015)

t.test(df_twoprods$Sales, 
       df_twoprods_2015$Sales, paired = TRUE, alternative = "two.sided")
# The probability to see these results are  p= 9.088e-09
# The null hypothesis is rejected. There is a difference among both years

# Results
# data:  df_twoprods$Sales and df_twoprods_2015$Sales
# t = 7.8569, df = 30, p-value = 9.088e-09
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   33.32690 56.73762
# sample estimates:
# mean difference 
# 45.03226 

# t-Test: Paired Two Sample for Means		
# t Stat	7.856926907                 <---- Same result
# P(T<=t) one-tail	4.54376E-09
# t Critical one-tail	1.697260887
# P(T<=t) two-tail	9.08753E-09      <--- Same result
# t Critical two-tail	2.042272456

# * 4.2 Analysis of Variance (ANOVA) -------------

# * * 4.2.1 One Way ANOVA ---------------------

# Analyzing the effect of Method on Sales

summary(aov(Sales ~ Method, data = df_twoprods))

# F-statistic p-value = 0.47, 
# There is no strong evidence to reject the null hypothesis. 
# "Method" does not have a significant effect on "Sales."

# Analyzing the effect of Marketing on Sales

summary(aov(Sales ~ MarketingSpend, data = df_twoprods))

# F-statistic p-value = 1.92e-09, 
# indicates strong evidence against the null hypothesis. 
# It suggests that "MarketingSpend" significantly affects "Sales."

# Analyzing the effect of StockPrice on Sales

summary(aov(Sales ~ StockPrice, data = df_twoprods))

# F-statistic p-value = 0.00814, p-value is less than 0.05
# indicates strong evidence to reject the null hypothesis. 
# It suggests that "StockPrice" significantly affects "Sales"


# * * 4.2.2 Two-Way ANOVA --------

# To analyze the impact of two independent variables: "Method," "StockPrice"

summary(aov(Sales ~ Method + StockPrice, data = df_twoprods))

# "StockPrice" p-value = 0.0117. It has a significant effect on "Sales"
# "Method" does not have a significant effect on "Sales" (p-value  0.4262)

# * * 4.2.3 Multi-way ANOVA -----------

# Perform a multi-way ANOVA involving three or more independent variables,
# independent variables: "Method," "StockPrice," and "MarketingSpend"

# Assuming you have loaded your dataset into df_twoprods
# Perform a multi-way ANOVA

anova_result <- aov(Sales ~ Method + StockPrice + MarketingSpend, data = df_twoprods)

# Print the ANOVA summary
summary(anova_result)

# * * 4.2.4 Combination of Method and Product --------
# To create a recap of sales by the Combination of Method and Product ----
# calculate the sums of sales for each combination. 

recap_df <- data.frame(
  c(df_twoprods[df_twoprods$Method == "M01","Product1"],NA),
  c(df_twoprods[df_twoprods$Method == "M01","Product2"],NA),
  df_twoprods[df_twoprods$Method == "M02","Product1"],
  df_twoprods[df_twoprods$Method == "M02","Product2"]
)

names(recap_df) <- c("M01_Prod1", "M01_Prod2", "M02_Prod1", "M02_Prod2")
str(recap_df)
names(recap_df)

summary(aov(M01_Prod1 ~ ., data = recap_df ))
summary(aov(M01_Prod2 ~ ., data = recap_df ))
summary(aov(M02_Prod1 ~ ., data = recap_df ))
summary(aov(M02_Prod2 ~ ., data = recap_df ))

#####################################################################.
# 5 PREDICTIVE STATISTICS ----------------------------
# LINEAR REGRESSION MULTIPLE VARIABLES

head(df_twoprods)

regression_results <- lm(formula = Sales ~ StockPrice + MarketingSpend + Price, 
   data = df_twoprods)

print(regression_results)

summary(regression_results)

# Results
#Coefficients:
#  (Intercept)      StockPrice  MarketingSpend       Price  
#   -176.106           2.666           1.922        -131.305  

# Results in Excel - Data Analysis tools
# Coefficients
# Intercept	-176.1059757       <-- same result
# StockPrice	2.665833671      <-- same result
# MarketingSpend	1.922497817  <-- same result
# Price	-131.3052427           <-- same result  

# END -----------



