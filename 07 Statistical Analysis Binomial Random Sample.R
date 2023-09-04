
########################################################################.
##################### STATISTICAL ANALYSIS - BINOMIAL RANDOM SAMPLE ####.
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "ggplot2", "lubridate", "data.table", "tidyverse")
lapply(required_packages, library, character.only = TRUE)

 
# 1 GENERATE ONE SAMPLE N = 50 BINOMIAL RANDOM --------------------------------

# Paths # Location of Source files

# Generate from n=50 with a chance to get 1 equal to 0.3 and get 0 equal to 0.7
set.seed(12345)
binomrandsample <- rbinom(n = 50,1,prob = 0.3)  
# table
table(binomrandsample)/sum(table(binomrandsample))

mean(binomrandsample)

# 2 GENERATE 100 SAMPLES N = 50 BINOMIAL RANDOM ---------------------------

# Paths # Location of Source files

# Generate from n=50 with a chance to get 1 equal to 0.3 and get 0 equal to 0.7

results_50 <- NULL
for(i in seq.int(1:100)){
  results_50[i] <- mean(rbinom(n = 50,1,prob = 0.3))
}
results_50

hist_50 <- hist(results_50, breaks=20)
hist_50

# Standard Deviations for Binomial Random = 0.0654

sqrt(0.3*(1-0.3)/(50-1))

# 3 GENERATE 100 SAMPLES N = 800 BINOMIAL RANDOM ----------------------------

# Paths # Location of Source files

# Generate from n=50 with a chance to get 1 equal to 0.3 and get 0 equal to 0.7

results_800 <- NULL
for(i in seq.int(1:100)){
  results_800[i] <- mean(rbinom(n = 800,1,prob = 0.3))
}
results_800

hist_800 <- hist(results_800)
hist_800

# Standard Deviations for Binomial Random = 0.01621199

sqrt(0.3*(1-0.3)/(800-1))

# 4 PLOT BOTH HISTOGRAMS ------------.

results_50; results_800

range(c(hist_50$breaks, hist_800$breaks)) # Get range for x-axis

max(c(hist_50$count, hist_800$count)) # Get range for y-axis

min(c(results_50, results_800)) - 0.001 # Set the minimum for the breakpoints
# e <- max(c(results_50, results_800)) # Set the maximum for the breakpoints

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

#plot(hist_50, col = c1, xlim = c(0, 0.6), ylim = c(0,25))
plot(hist_50, col = c1, 
     xlim = range(c(hist_50$breaks, hist_800$breaks)), 
     ylim = c(0,25))
plot(hist_800, add = TRUE, col = c2)


# END -----------



