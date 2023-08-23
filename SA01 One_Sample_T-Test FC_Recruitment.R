
###################################################################.
# SCRIPT: ONE SAMPLE T-TEST
# USE CASE: TO BUILD THE MONTHLY ENROLLMENT LIST DATA CSV FILE 
# FOR DATA PROCESSING: Example Care Management Results
# Data Source: Recruitment Notes Log
# FREQUENCY: Execute this script after enrollment ONCE A MONTH
###################################################################.

# It is a type of statistical hypothesis test used to determine if the mean
# of a sample is significantly less than a hypothesized population mean. 
# The test assesses whether there is enough evidence in the sample data 
# to support the claim that the true population mean is less than the 
# hypothesized value.


# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

st <- Sys.time()

#.libPaths()
# .libPaths("C:/R_Libraries")     # Defined as Environmental Variable
# .libPaths() ###  Check the default folder where packages are installed

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "readxl", "data.table", "lubridate")
lapply(required_packages, library, character.only = TRUE)

# READ THE LIST OF EXCEL DATA FILES ------------------
# Read sample files using full path and Regex (known pattern)

setwd("D:/DataRepository")
setwd("./StatisticalAnalysis/SA01")
#path <- "./StatisticalAnalysis/SA01"  # setwd("./SA/SA01")
filenames_list <- list.files(pattern=paste0("^FC.*",".xlsx"), #path= path,
                             full.names=FALSE)
filenames_list

# * Function: Read Excel files with equal structure --------

fx_readfiles <- function(filename){
  print(paste("Merging", filename, sep = " "))
  xlfile <- readxl::read_excel(filename) #RESULT DATA FRAME
  print(paste("Number of records in ",
              filename, " = ", nrow(xlfile),
              " ; columns = ", length(xlfile), sep = ""))
  xlfile[] <- lapply(xlfile, function(x) {
    if (inherits(x, "logical")) as.character(x) else x
  })
  dfXLfile <- data.frame(xlfile)  # Create data frame, not tibble
  dfXLfile <- dfXLfile %>% filter(MotherName != "empty", 
                                  RoomOccupied == "N", 
                                  BirthOutcome == "Alive", 
                                  Eligibility == "Eligible",
                                  ContactedbyFC == "Y" 
                                  ) %>% 
                                  select(c("CaseID", 
                                    "CaseOpen_DD",
                                    "CountyofResidence", 
                                    "FamilyagreedFCvisit"
                                  
                                    ))
  dfXLfile
}

# Apply defined function to list -------
tibbles <- lapply(filenames_list,fx_readfiles)

str(tibbles)            # Resulting in 5 tibbles

df <- do.call(dplyr::bind_rows, tibbles)

dim(df)
head(df,30)
tail(df,30)

df$Index <- paste0('E',stringr::str_pad(1:nrow(df), 4, side="left", pad="0"))
head(df)

# LEFT JOIN USERS CASE_ID 
users <- read.csv("Users_CaseID.csv")
users <- users %>% select(-c("Since"))
str(users)

#users %>% group_by(ID_FCDB) %>% tally()

str(df)
#df %>% left_join(users, by = c("CaseID", "ID_FCDB"))
df <- df %>% 
  left_join(users, by = c("CaseID" = "ID_FCDB"))

str(df)
dim(df)
class(df)
tail(df,20)

######## CONVERT CASEOPEN_DD to Date Time  YEAR WEEK -------------
# In some records de value is registered including seconds 
df$CaseOpen_DDCHECK <- coalesce(
  as.POSIXct(df$CaseOpen, format = "%m/%d/%Y %I:%M:%S %p", tz = "EST"),
  as.POSIXct(df$CaseOpen, format = "%m/%d/%Y %I:%M %p", tz = "EST")
)
df

# Create field year - week
df$yearweek <- paste0(format.Date(df$CaseOpen_DDCHECK, "%Y"), "/", 
                  sprintf("%02d", week(df$CaseOpen_DDCHECK)))
df$yearweek[is.na(df$yearweek)]
as.Date(df$CaseOpen_DDCHECK)
df %>% filter(as.Date(CaseOpen_DDCHECK) == "2023-08-11")
### 

df1 <- df %>% group_by(yearweek, FamilyagreedFCvisit, UserEmail) %>% tally()
class(df1)

### 
df1 <- data.table(df1)
tail(df1,20)

# TOTAL RESULTS
#fx_TotalResults <- function(data){
#  resultCM <- dcast(data, yearweek + UserEmail ~ FamilyagreedFCvisit, value.var = "n", fill = 0)
#  resultCM <- resultCM %>% mutate(Total = agreed + declined + undecided + undetermined  )
#  resultCM <- resultCM %>% mutate(PercentAgreed = agreed / Total )
#  resultCM
#}

fx_TotalResults <- function(data){
  resultCM <- data %>%
    pivot_wider(names_from = FamilyagreedFCvisit, values_from = n, values_fill = 0) %>%
    mutate(Total = agreed + declined + undecided + undetermined,
           PercentAgreed = agreed / Total)
  resultCM
}


dfTotal <- fx_TotalResults(df1)
dfTotal
sum(dfTotal$agreed)
sum(dfTotal$Total)
mu_value <- sum(dfTotal$agreed)/sum(dfTotal$Total)
mu_value

# RESULTS PER RECRUITER -------------
#fx_convertdfCM <- function(data, CM){
#  # Filter CM
#  data <- data %>% filter(UserEmail == CM)
#  resultCM <- dcast(data, yearweek + UserEmail ~ FamilyagreedFCvisit, value.var = "n", fill = 0)
#  resultCM <- resultCM %>% mutate(Total = agreed + declined + undecided + undetermined  )
#  resultCM <- resultCM %>% mutate(PercentAgreed = agreed / Total )
#  resultCM <- resultCM[resultCM$Total>4,] # eliminating outliers
#  resultCM
#}

# Results per Recruiter
fx_convertdfCM <- function(data, CM){
  # Filter CM
  data <- data %>% filter(UserEmail == CM)
  resultCM <- data %>% pivot_wider(names_from = FamilyagreedFCvisit, 
                      values_from = n, values_fill = 0) %>%
    mutate(Total = agreed + declined + undecided + undetermined,
           PercentAgreed = agreed / Total)
  resultCM <- resultCM[resultCM$Total>4,] # eliminating outliers
  resultCM
}

dfCM01 <- fx_convertdfCM(df1, "CM01")
dfCM01 <- dfCM01 %>% select(-c("NA"))
dfCM01 %>% group_by(UserEmail) %>% tally()
dfCM01
sum(dfCM01$agreed)/sum(dfCM01$Total)
dfCM02 <- fx_convertdfCM(df1, "CM02")
dfCM02 %>% group_by(UserEmail) %>% tally()
sum(dfCM02$agreed)/sum(dfCM02$Total)
dfCM03 <- fx_convertdfCM(df1, "CM03")
dfCM03 %>% group_by(UserEmail) %>% tally()
sum(dfCM03$agreed)/sum(dfCM03$Total)

# COMBINE TO PLOT IN HISTOGRAMS  -------

combo <- rbind(dfCM01, dfCM02, dfCM03)

# Combine Histogram and Density Plots for Percentages of Agreement
ggplot(combo, aes(PercentAgreed, fill = UserEmail)) +
  scale_fill_manual(values=c("blue", "darkgreen", "orange")) +
  geom_histogram(alpha=0.7, binwidth=0.01, position="identity") +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0.83166, size = 0.01, color = "red") + 
  ggtitle("Combined Histogram and Density Plots for Percentages of Agreement")

# Print the melted dataframe
#print(melted_df)

# NULL HYPOTHESIS: There is no difference -------------

# NULL HYPOTHESIS: There is no difference between the results of the 
# recruiters individually compared to the total of the whole period.

#t.test(dfCM01$PercentAgreed, mu = mu_value, 
#       alternative = "two.sided", conf.level = 0.95, alpha = 0.05)
#t.test(dfCM02$PercentAgreed, mu = mu_value, 
#       alternative = "two.sided", conf.level = 0.95, alpha = 0.05)
#t.test(dfCM03$PercentAgreed, mu = mu_value, 
#       alternative = "two.sided", conf.level = 0.95, alpha = 0.05)

# NULL HYPOTHESIS: The results are not less than the average -------

t.test(dfCM01$PercentAgreed, mu = mu_value, 
       alternative = "less", conf.level = 0.95, alpha = 0.05)
t.test(dfCM02$PercentAgreed, mu = mu_value, 
       alternative = "less", conf.level = 0.95, alpha = 0.05)
t.test(dfCM03$PercentAgreed, mu = mu_value, 
       alternative = "less", conf.level = 0.95, alpha = 0.05)


# Interpret the results
#if (p_value < 0.05) {
#  print("Reject the null hypothesis. There is evidence of a significant 
#        difference in the proportion of patients who agreed to a 
#        home visit after the designated point compared to the 
#        historical proportion.")
#} else {
#  print("Fail to reject the null hypothesis. There is no significant 
#        evidence of a difference.")
#}

# EXPORT DATA RESULTS  ----------

#names(df)
#str(df)
#dim(df)
#setwd("D:/FC_Scheduling_Data")
#data.table::fwrite(df,"FCRecruitment.csv")

#setwd("D:/FC_Scheduling_Data")
#data.table::fwrite(df,"D:/FC_Scheduling_Data/checkrecap00.csv")
#df1


######################## END END END END END ---------


############# EXAMPLE PROPORTION AGREEMENT --------------
# Example data (replace this with your actual data)
# Let's assume "AgreementDT" indicates whether a patient agreed to a home visit
# Let's also assume you have a data subset: after_data (after a certain point)

# Calculate the proportion of patients who agreed to a home visit after the designated point
#proportion_after <- sum(after_data$AgreementDT == "Agreed") / nrow(after_data)

# Historical proportion (replace this with the historical value you want to compare to)
#historical_proportion <- 0.2  # Example historical proportion

# Perform one-sample t-test
#t_statistic <- (proportion_after - historical_proportion) / sqrt(proportion_after * (1 - proportion_after) / nrow(after_data))
#df <- nrow(after_data) - 1
#p_value <- 2 * pt(-abs(t_statistic), df)

# Print the results
#print(paste("Proportion Agreed (After):", proportion_after))
#print("One-Sample T-Test for Proportions Results:")
#print(paste("T-Statistic:", t_statistic))
#print(paste("P-Value:", p_value))

# Interpret the results
#if (p_value < 0.05) {
#  print("Reject the null hypothesis. There is evidence of a significant difference in the proportion of patients who agreed to a home visit after the designated point compared to the historical proportion.")
#} else {
#  print("Fail to reject the null hypothesis. There is no significant evidence of a difference.")
#}

################# END END =========

