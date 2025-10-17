##Assessment 2##


##install packages require for analysis
install.packages("ggplot2")
install.packages("car")
install.packages("lmtest")
install.packages("AICcmodavg")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("EnvStats")
install.packages("bestNormalize")
install.packages("MASS")
install.packages("openxlsx")
install.packages("writexl")
install.packages("GGally") 
install.packages("reshape2")



##run library for each package so they are ready to use
library(car)
library(lmtest)
library(AICcmodavg)
library(tidyr)
library(dplyr)
library(ggpubr)
library(EnvStats)
library(bestNormalize)
library(MASS)
library(openxlsx)
library(ggplot2)
library(writexl)
library(GGally)
library(reshape2)


# Load the dataset directly from the project folder
# The 'openxlsx' package is assumed to be loaded above.
iscc <- read.xlsx("iscc.xlsx")

# Check the first few rows
print("First few rows of the dataset:")
print(head(iscc))


#check the first few rows:
head(iscc)


#Check the structure of the data:
str(iscc)

#Check colmun
print(colnames(iscc))



## Data Preparation for any code for cleaning or preparing my data set.##

# Remove NA values and create a new data frame
# na.omit() is used to remove rows containing NA values.
iscc_complete <- na.omit(iscc)
iscc_complete

# Check the original data for plannedstartmatingdate and lastservice
# head() is used to inspect the original data for both plannedstartmatingdate and lastservice.
head(iscc_complete$plannedstartmatingdate)
head(iscc_complete$lastservice)

# Convert plannedstartmatingdate to date type
# plannedstartmatingdate is converted to a date format using the specified format.
iscc_complete$plannedstartmatingdate <- as.Date(iscc_complete$plannedstartmatingdate, format = "%d-%b-%y")

# Convert lastservice to numeric type
# lastservice is converted to numeric type first.
iscc_complete$lastservice <- as.numeric(iscc_complete$lastservice)

# Convert serial number to date
# The serial values in lastservice are converted to date format using the correct origin.
iscc_complete$lastservice <- as.Date(iscc_complete$lastservice, origin = "1899-12-30")

# Check the results after conversion again to ensure both year/month/date are formatted in correct way.
head(iscc_complete$plannedstartmatingdate)
head(iscc_complete$lastservice)

# Confirm both date formats by printing
# head() is used again to verify the conversion results for both columns.
print(head(iscc_complete$plannedstartmatingdate))
print(head(iscc_complete$lastservice))

# Check the number of NAs for plannedstartmatingdate
na_count <- sum(is.na(iscc_complete$plannedstartmatingdate))
print(na_count)

# Show values ​​that could not be converted for plannedstartmatingdate
na_values <- iscc_complete$lastservice[is.na(iscc_complete$plannedstartmatingdate)]
print(na_values)


# Check the number of NAs for lastservice
na_count <- sum(is.na(iscc_complete$lastservice))
print(na_count)

# Show values ​​that could not be converted for lastservice
na_values <- iscc_complete$lastservice[is.na(iscc_complete$lastservice)]
print(na_values)


# Convert categorical variables
# By converting the data into factors, the characteristics of categorical data are utilized,
# making the model easier to interpret and the analysis results clearer.
# In addition, using factors in methods such as regression analysis and ANOVA in R allows me to properly 
# evaluate the effect of each category.
# By setting categorical variables as factors, I can handle them appropriately when building models.

# Treatment is a binary variable (yes/no) indicating whether or not the patient received Monensin Lumen Capsule treatment.
# This variable is also categorical data, and by converting it to a factor, we can accurately incorporate the effect of treatment into the model.
# Treating it as a factor allows us to properly compare treatment and control groups in the regression analysis.
iscc_complete$treatment <- as.factor(iscc_complete$treatment)
iscc_complete$treatment

# farmid is a variable that identifies the farm to which each cow belongs, and is categorical data.
# By converting it to a factor, I can analyze each farm separately, and dummy variables are automatically generated in the regression model.
# This makes it possible to analyze differences between farms.


# Differences by treatment: A color-coded histogram by treatment allows me to compare the distribution of ISCC 
# between cows that received Monensin ruminal capsules and those that did not.
# For example, if treated cows have a lower ISCC, it is likely that the treatment is effective.
ggplot(iscc_complete, aes(x = averageiscc, fill = treatment)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Average ISCC by Treatment", 
       x = "Average ISCC (x 1,000 cells/ml)", 
       y = "Frequency") +
  theme_minimal()


iscc_complete$farm <- as.factor(iscc_complete$farm)
iscc_complete
# Differences among farms: By observing the color-coded histograms by farm,
# I can understand the difference in the distribution of ISCC among farms.
# If the ISCC is high on a particular farm, it may be due to factors specific to the management or environment of that farm.
ggplot(iscc_complete, aes(x = averageiscc, fill = farm)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Average ISCC by Farm", 
       x = "Average ISCC (x 1,000 cells/ml)", 
       y = "Frequency") +
  theme_minimal()

# ggplot2 automatically handles factor variables, so I use fill aesthetics to draw histograms colored by category.

# Checking after conversion
str(iscc_complete)  # Show structure of data frame

# Interpretation of summary results
# 0: Number of cows not treated with Monensin ruminal capsules (318)
# 1: Number of cows treated with Monensin ruminal capsules (318)
# The results show that treated and untreated cows are evenly distributed, which indicates a balanced data set for analysis.
## Display factor levels:$ treatment: Factor w/ 2 levels "0","1": 2 1 1 1 1 2 1 2 2 2 ...
summary(iscc_complete$treatment)  # Display summary of treatment


# The summary results for farm show that the number of cows belonging to each farm is distributed as follows:
# Interpretation of summary results
# Farm 1: 128 heads
# Farm 2: 268 heads
# Farm 3: 240 heads
#These results show that there is a difference in the number of cows on each farm.
# In particular, Farm 2 has more cows than the other farms.
#Display  farm : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
summary(iscc_complete$farm)  #Display summary of farmid

# Histogram of iscc_complete, then focus on response variable: averageiscc
# I'm creating a histogram of averageiscc to visualize the distribution of the data.
# By adding a mean line, I'm also showing the central tendency of the data.
#This is still skew so not noralised distribution.
hist(iscc_complete$averageiscc, 
     breaks = 30, 
     main = "Histogram of Average Individual Somatic Cell Count", 
     xlab = "Average ISCC (x 1,000 cells/ml)", 
     col = "lightblue",  # Bar Color
     border = "black",    # Bar border color
     xlim = c(0, max(iscc_complete$averageiscc, na.rm = TRUE)),  # x-axis range
     ylim = c(0, 20),     # y-axis range (adjust accordingly)
     las = 1)             # x-axis labels sideways

# Add: Draw an average line
abline(v = mean(iscc_complete$averageiscc, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)


# Log transformation of averageiscc and setting new variable names
# Logarithmic transformation is expected to bring the data distribution closer to normal.
# It is particularly effective when the average number of individual cells is skewed to the right.

# Consider the following variables that should be log10 transformed:

# What to log transform is averageiscc:
# Reason: This is the average individual cell count (ISCC), and since high values ​​indicate mastitis,
# it is usually expected to have a right-skewed distribution.
# By log transforming, it is expected that the distribution will be closer to normal.
# log_averageiscc       : num [1:636] 2.99 1.95 2.19 1.67 2.45 ...

# Other variables
# farm: This is a factor variable, so I do not perform a logarithmic transformation.
# age, lactationno, timetocon, noservices: These are numerical variables,
# but I usually do not need to perform a logarithmic transformation on these variables.
# However, if a particular variable is extremely skewed, I can consider performing a logarithmic transformation.
# treatment: This is a binary variable (0 or 1), so I do not need to perform a logarithmic transformation.
iscc_complete$log_averageiscc <- log10(iscc_complete$averageiscc + 1)  # +1 to avoid division by zero

# Check if the variable was added correctly
head(iscc_complete)
str(iscc_complete)

hist(iscc_complete$log_averageiscc, 
     breaks = 30, 
     main = "Histogram of Log10 Average Individual Somatic Cell Count", 
     xlab = "Log10 Average ISCC (x 1,000 cells/ml)", 
     ylab = "Frequency",  
     col = "lightblue",  
     border = "black",    
     xlim = c(0, max(iscc_complete$log_averageiscc, na.rm = TRUE)),  
     ylim = c(0, max(table(cut(iscc_complete$log_averageiscc, breaks = 30)))),  
     las = 1)             

# Add average line
abline(v = mean(iscc_complete$log_averageiscc, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)



# Checking normality – Shapiro-Wilk test
#W = 0.94391
#p-value = 8.933e-15

#Interpretation of results
#W value:

#The closer the W value is to 1, the more the data follows a normal distribution. A W value of 0.94391 suggests that the data may deviate from the normal distribution.
#p value:

# The null hypothesis (the data follows a normal distribution) is rejected because the p value is very small (8.933e-15) compared to the usual significance level (0.05).
# In other words, we can conclude that the data does not follow a normal distribution
#Checking normality – Shapiro-Wilk test
# Check whether the data follow a normal distribution.
# If the Shapiro-Wilk test results in a p-value < 0.05, reject normality.
# data:  iscc_complete$log_averageiscc
# W = 0.94391, p-value = 8.933e-15
# This is a number called "W". This number indicates how "normally shaped (normally distributed)" the data is.
# The closer the W value is to 1, the closer the data is to a normal shape.
# In this case, the value is 0.94391, which means that the data is slightly deviated from a normal shape.
# This number is called the "p-value." This very small number (8.933e-15) is the result of a special calculation
# and is used to determine whether the data has a normal shape (normal distribution).
# Generally, if the p-value is smaller than 0.05, it is considered that the "data does not have a normal shape." 
#Here, because the p-value is very small, I can see that the data does not have a normal shape (normal distribution).

shapiro_test_result_log_averageiscc <- shapiro.test(iscc_complete$log_averageiscc)
print(shapiro_test_result_log_averageiscc)

# Creating a Q-Q plot
# Visually assess normality with a Q-Q plot.
qqnorm(iscc_complete$log_averageiscc)
qqline(iscc_complete$log_averageiscc, col = "red")

#-------------------------------------------------------------------------------------------------------------

# Calculating the Z-score
# Identify and visualize outliers using Z-scores.
# The data was standardized by calculating the Z-score of log_averageiscc
# using z_scores <- scale(iscc_complete$log_averageiscc).
# Calculating the Z-score
z_scores <- scale(iscc_complete$log_averageiscc)

# Create a scatter plot of the Z scores
plot(iscc_complete$log_averageiscc, 
     z_scores, 
     main = "Z-Scores of Log10 Average ISCC",
     xlab = "Log10 Average ISCC (x 1,000 cells/ml)",
     ylab = "Z-Score",
     col = ifelse(abs(z_scores) > 3, "red", "blue"),  # Outliers in red
     pch = 16)  
abline(h = c(-3, 3), col = "red", lty = 2)  # Z score boundaries

# Get the index of the outlier
outliers_z <- which(abs(z_scores) > 3)
print(outliers_z)  # Show outlier indices

# Detecting outliers using IQR
Q1 <- quantile(iscc_complete$log_averageiscc, 0.25)
Q3 <- quantile(iscc_complete$log_averageiscc, 0.75)
IQR <- Q3 - Q1

# Replace outliers with lower and upper bounds
iscc_complete$log_averageiscc[iscc_complete$log_averageiscc < (Q1 - 1.5 * IQR)] <- (Q1 - 1.5 * IQR)
iscc_complete$log_averageiscc[iscc_complete$log_averageiscc > (Q3 + 1.5 * IQR)] <- (Q3 + 1.5 * IQR)


#----------------------------------------------------------------------------------------------------------------

# Create a histogram for timetocon
hist(iscc_complete$timetocon, 
     breaks = 30, 
     main = "Histogram of Time to Conception", 
     xlab = "Time to Conception (days)", 
     col = "lightgreen",  # Bar Color
     border = "black",     # Bar border color
     xlim = c(0, max(iscc_complete$timetocon, na.rm = TRUE)),  # x-axis range
     ylim = c(0, 20),     # y-axis range (adjust accordingly)
     las = 1)              # x-axis labels sideways

# Add: Draw an average line
abline(v = mean(iscc_complete$timetocon, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)

# Apply log10 transformation to timetocon
iscc_complete$log_timetocon <- log10(iscc_complete$timetocon + 1)  # +1 to avoid log(0)

# Check if the variable was added correctly
head(iscc_complete)
str(iscc_complete)


# Check for Missing Values
#Make sure that there are no missing values in timetocon,
#as this can also cause issues when creating the log-transformed variable. You can check for NAs as follows:
sum(is.na(iscc_complete$timetocon))


# Create a histogram for log-transformed timetocon
hist(iscc_complete$log_timetocon, 
     breaks = 30, 
     main = "Histogram of Log10 Time to Conception", 
     xlab = "Log10 Time to Conception (days)", 
     col = "lightgreen",  # Bar Color
     border = "black",     # Bar border color
     xlim = c(0, max(iscc_complete$log_timetocon, na.rm = TRUE)),  # x-axis range
     ylim = c(0, 20),     # y-axis range (adjust accordingly)
     las = 1)              # x-axis labels sideways

# Add: Draw an average line
abline(v = mean(iscc_complete$log_timetocon, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = "Mean", col = "red", lty = 2, lwd = 2)


shapiro_test_result_log_timetocon <- shapiro.test(iscc_complete$log_timetocon)
print(shapiro_test_result_log_timetocon)

qqnorm(iscc_complete$log_timetocon)
qqline(iscc_complete$log_timetocon, col = "red")


# Calculating the Z-score
z_scores <- scale(iscc_complete$log_timetocon)

# Create a scatter plot of the Z scores
plot(iscc_complete$log_timetocon, 
     z_scores, 
     main = "Z-Scores of Log10 Time to Conception",
     xlab = "Log10 Time to Conception (days)",
     ylab = "Z-Score",
     col = ifelse(abs(z_scores) > 3, "red", "blue"),  # Outliers in red
     pch = 16)  # Point shape
abline(h = c(-3, 3), col = "red", lty = 2)  # Z score boundaries

# Get the index of the outlier
outliers_z <- which(abs(z_scores) > 3)
print(outliers_z)  # Show outlier indices



# Detecting outliers using IQR
Q1 <- quantile(iscc_complete$log_timetocon, 0.25)
Q3 <- quantile(iscc_complete$log_timetocon, 0.75)
IQR <- Q3 - Q1

# Replace outliers with lower and upper bounds
iscc_complete$log_timetocon[iscc_complete$log_timetocon < (Q1 - 1.5 * IQR)] <- (Q1 - 1.5 * IQR)
iscc_complete$log_timetocon[iscc_complete$log_timetocon > (Q3 + 1.5 * IQR)] <- (Q3 + 1.5 * IQR)



#-------------------------------------------------------------------------
# Create a scatter plot after outlier processing
# iscc_cleaned is the dataset after outlier processing.
# In this code, I use the original dataset as iscc_complete and 
# save the result of outlier processing in iscc_cleaned.
# By taking these steps, I can check the quality of my data and
# reduce the impact of outliers before proceeding with my multiple regression analysis.
iscc_cleaned <- iscc_complete 
summary(iscc_cleaned)# Save the dataset after outlier processing


# Plot Settings
par(mfrow = c(2, 2))  # Setting up a 2x2 plot

# Histogram of Log10 Average ISCC
hist(iscc_cleaned$log_averageiscc, 
     main = "Histogram of Log10 Average ISCC", 
     xlab = "Log10 Average ISCC", 
     col = "lightblue")

# Add a median line
abline(v = median(iscc_cleaned$log_averageiscc, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
# Add average line
abline(v = mean(iscc_cleaned$log_averageiscc, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Mean"), col = c("blue", "red"), lty = 2, lwd = 2)

# Histogram of Log10 Time to Conception
hist(iscc_cleaned$log_timetocon, 
     main = "Histogram of Log10 Time to Conception", 
     xlab = "Log10 Time to Conception", 
     col = "lightgreen")

# Add a median line
abline(v = median(iscc_cleaned$log_timetocon, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
# Add average line
abline(v = mean(iscc_cleaned$log_timetocon, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Median", "Mean"), col = c("blue", "red"), lty = 2, lwd = 2)


# Q-Q plot
qqnorm(iscc_cleaned$log_averageiscc)
qqline(iscc_cleaned$log_averageiscc, col = "red")
qqnorm(iscc_cleaned$log_timetocon)
qqline(iscc_cleaned$log_timetocon, col = "red")


# Check column names
print(colnames(iscc_cleaned))

# Check the structure of the data frame
str(iscc_cleaned)


# Check the number of NAs in each column
na_count <- sapply(iscc_cleaned, function(x) sum(is.na(x)))
print(na_count)

# Scatter plot example
plot(iscc_cleaned$log_timetocon, iscc_cleaned$log_averageiscc,
     main = "Log10 Average ISCC vs Log10 Time to Conception",
     xlab = "Log10 Time to Conception",
     ylab = "Log10 Average ISCC",
     col = "blue", pch = 16)

# Create Scatterplot:
ggplot(iscc_cleaned, aes(x = log_timetocon, y = log_averageiscc)) +
  geom_point() +
  labs(x = "Log10 Time to Conception", y = "Log10 Average ISCC") +
  theme_minimal()

# Correlation Coefficient:
cor((iscc_cleaned$log_timetocon), (iscc_cleaned$log_averageiscc))

# Check for NA Values:
# Check for NA values
na_timetocon <- sum(is.na(iscc_cleaned$log_timetocon))  # Count NA in log_timetocon
na_averageiscc <- sum(is.na(iscc_cleaned$log_averageiscc))  # Count NA in log_averageiscc

# Print NA counts
print(paste("NA in log_timetocon:", na_timetocon))
print(paste("NA in log_averageiscc:", na_averageiscc))

# Filter out invalid values (non-positive)
cleaned_data <- iscc_cleaned[iscc_cleaned$log_timetocon > 0 & iscc_cleaned$log_averageiscc > 0, ]

# Calculate the correlation coefficient
cor_result <- cor(cleaned_data$log_timetocon, cleaned_data$log_averageiscc)
print(paste("Correlation Coefficient:", cor_result))


# Fit the linear model between Y: log_averageiscc and log_timetocon
model <- lm(log_averageiscc ~ log_timetocon, data = cleaned_data)
summary(model)


# histogram
par(mfrow = c(2, 1))
hist(iscc_cleaned$log_averageiscc,
     main = "Histogram of Log10 Average ISCC",
     xlab = "Log10 Average ISCC",
     col = "lightblue")

hist(iscc_cleaned$log_timetocon,
     main = "Histogram of Log10 Time to Conception",
     xlab = "Log10 Time to Conception",
     col = "lightgreen")


# Creating a box plot of log_averageiscc
boxplot(iscc_cleaned$log_averageiscc,
        main = "Boxplot of Log10 Average ISCC",
        ylab = "Log10 Average ISCC",
        col = "lightblue",
        border = "darkblue",
        outline = TRUE)

# Calculate statistics
stats <- boxplot.stats(iscc_cleaned$log_averageiscc)

# Add statistical labels to the plot with offsets
text(x = 1.2, y = stats$stats[3] + 0.1, labels = paste("Median:", round(stats$stats[3], 2)), pos = 4)
text(x = 1.2, y = stats$stats[1] + 0.1, labels = paste("Q1:", round(stats$stats[1], 2)), pos = 4)
text(x = 1.2, y = stats$stats[5] - 0.1, labels = paste("Q3:", round(stats$stats[5], 2)), pos = 4)

# Display minimum and maximum values at the ends of the whiskers with offsets
text(x = 1.2, y = min(iscc_cleaned$log_averageiscc) - 0.1, labels = paste("Min:", round(min(iscc_cleaned$log_averageiscc), 2)), pos = 3)
text(x = 1.2, y = max(iscc_cleaned$log_averageiscc) + 0.1, labels = paste("Max:", round(max(iscc_cleaned$log_averageiscc), 2)), pos = 1)

# Display the number of outliers
text(x = 1.2, y = max(stats$out) + 0.1, labels = paste("Outliers:", length(stats$out)), pos = 4)

# Check the data distribution
print("Frequency of values in log_averageiscc:")
print(table(iscc_cleaned$log_averageiscc))

# Calculate quantiles manually
quantiles <- quantile(iscc_cleaned$log_averageiscc, probs = c(0, 0.25, 0.5, 0.75, 1))
print("Quantiles:")
print(quantiles)



# Creating a box plot of log_timetocon
boxplot(iscc_cleaned$log_timetocon,
        main = "Boxplot of Log10 Time to Conception",
        ylab = "Log10 Time to Conception (days)",
        col = "lightblue",
        border = "darkblue",
        outline = TRUE)

# Calculate statistics
stats <- boxplot.stats(iscc_cleaned$log_timetocon)

# Add statistical labels to the plot
text(x = 1.2, y = stats$stats[3], labels = paste("Median:", round(stats$stats[3], 2)), pos = 4)
text(x = 1.2, y = stats$stats[1], labels = paste("Q1:", round(stats$stats[1], 2)), pos = 4)
text(x = 1.2, y = stats$stats[5], labels = paste("Q3:", round(stats$stats[5], 2)), pos = 4)

# Display minimum and maximum values at the ends of the whiskers
text(x = 1.2, y = min(iscc_cleaned$log_timetocon), labels = paste("Min:", round(min(iscc_cleaned$log_timetocon), 2)), pos = 3)
text(x = 1.2, y = max(iscc_cleaned$log_timetocon), labels = paste("Max:", round(max(iscc_cleaned$log_timetocon), 2)), pos = 1)

# Display the number of outliers
text(x = 1.2, y = max(stats$out), labels = paste("Outliers:", length(stats$out)), pos = 4)

# Check the data distribution
print("Frequency of values in log_timetocon:")
print(table(iscc_cleaned$log_timetocon))

# Calculate quantiles manually
quantiles <- quantile(iscc_cleaned$log_timetocon, probs = c(0, 0.25, 0.5, 0.75, 1))
print("Quantiles:")
print(quantiles)


# Checking after conversion
# Summarizing the data
summary(iscc_cleaned)

# Create a scatter plot matrix
# Variables used: We've selected other explanatory variables, including log_averageiscc and log_timetocon.
#Colors: We've created plots with different colors (blue and red) based on the treatment column.
#Plot title: We've changed the title of the scatterplot matrix to highlight the log-transformed variables.
pairs(iscc_cleaned[, c("log_averageiscc", "age", "farm", "lactationno", "log_timetocon", "treatment", "noservices")],
      main = "Scatterplot Matrix with Log-Transformed Variables",
      pch = 19, col = ifelse(iscc_cleaned$treatment == "1", "blue", "red"))


# Assuming 'iscc_complete' is my dataset but after data cleaning, iscc_cleaned was used as the main data set, so log10, data transformed date and time,
# outline treatment, null value was removed from original database.

# Create Scatterplot Matrix
selected_data <- iscc_cleaned[, c("log_averageiscc", "age", "farm", "lactationno", "log_timetocon", "treatment", "noservices")]

scatterplot_matrix <- ggpairs(selected_data, 
                              aes(color = treatment, alpha = 0.5)) +
  labs(title = "Scatterplot Matrix of Variables")
print(scatterplot_matrix)

# Save Scatter plot Matrix
ggsave("scatterplot_matrix.png", plot = scatterplot_matrix, width = 10, height = 10)


iscc_cleaned <- na.omit(iscc_cleaned)

# # Multiple Linear Regression
# Building a multiple regression model (Model 1)
lm1 <- lm(log_averageiscc ~ age + farm + lactationno + log_timetocon + treatment + noservices, data = iscc_cleaned)

# View a summary of the model
summary(lm1)

# Check the level of the farm variable
levels(iscc_cleaned$farm)

# Check the farm level
table(iscc_cleaned$farm)

# Exclude farm3
iscc_cleaned <- iscc_cleaned[iscc_cleaned$farm != 3, ]


# Check the farm level again after removing farm 3.
table(iscc_cleaned$farm)


# Create a data frame excluding farm3
iscc_cleaned <- iscc_cleaned[iscc_cleaned$farm != 'farm3', ]
print(iscc_cleaned) 

# Building Model 2
lm2 <- lm(log_averageiscc ~ age + farm + log_timetocon + treatment + noservices, data = iscc_cleaned)

# View a summary of the model
summary(lm2)

nrow(lm1$model)  # lm1 data size
nrow(lm2$model)  # lm2 data size


# Compare Model 1 and Model 2
lrtest(lm1, lm2)


# Build a model with lactation removed
lm3 <- lm(log_averageiscc ~ age + farm + log_timetocon + treatment, data = iscc_cleaned)

# View a summary of the model 3
summary(lm3)

nrow(lm2$model)  # lm2 data size
nrow(lm3$model)  # lm3 data size

# Compare Model 2 and Model 3
lrtest(lm2, lm3)

# Build a model with noservices removed
lm4 <- lm(log_averageiscc ~ age + farm + log_timetocon, data = iscc_cleaned)

# View a summary of the model 4
summary(lm4)

nrow(lm3$model)  # lm3 data size
nrow(lm4$model)  # lm4 data size

# Compare Model 3 and Model 4
lrtest(lm3, lm4)

# Build a model 5 that removes treatment1
#lm5 <- lm(log_averageiscc ~ age + farm, data = iscc_cleaned)

# View a summary of the model 5
#summary(lm5)

# Compare Model 4 and Model 5
#lrtest(lm4, lm5)

#Testing residuals for modelfit
par(mfrow=c(2,2))
plot(lm4)
par(mfrow=c(1,1))

#Test VIF#
car :: vif(lm4)


# Using AIC to confirm best model
# Model checking using AIC: 
# Use aictab to compare the AIC of five manually constructed models (lm1 to lm5) and assess the best model.
models <-list(lm1, lm2, lm3, lm4)
aictab(cand.set = models)


model_summary <- summary(lm4)  # lm5 is applying
results_table <- data.frame(
  Coefficient = model_summary$coefficients[, 1],
  SE = model_summary$coefficients[, 2],
  P_value = model_summary$coefficients[, 4]
)
results_table <- round(results_table, 8)

print(results_table)


# Stepwise model selection
# final_model <- stepAIC(lm1, direction = "both") select the best optimised model
final_model <- stepAIC(lm1, direction = "both")

# View a summary of the final model
cat("Final Model Summary:\n")
summary(final_model)

# View a summary of each model
# I use the cat function to format the results nicely,
# so that I can easily compare how the summaries for each model differ.
cat("\nModel 1 Summary:\n")
summary(lm1)

cat("\nModel 2 Summary:\n")
summary(lm2)

cat("\nModel 3 Summary:\n")
summary(lm3)

cat("\nModel 4 Summary:\n")
summary(lm4)

#cat("\nModel 5 Summary:\n")
#summary(lm5)

# Compare models
# lrtest_result <- lrtest(lm1, final_model) compares the fit of the initial model (lm1) 
# with the final model.
lrtest_result <- lrtest(lm1, final_model)
cat("\nLR Test Result (lm1 vs final_model):\n")
print(lrtest_result)


# Evaluating the residuals
# Visually assess the residuals of the final model with plot(final_model:lm5).
par(mfrow=c(2,2))
plot(final_model)
par(mfrow=c(1,1))

# Residual plots
par(mfrow = c(2, 2))
plot(final_model)

# Calculate VIF
library(car)
vif_values <- vif(final_model)
print(vif_values)



getwd()  # Show current working directory
setwd("C:/Users/takam.000/OneDrive - Lincoln University/QMTE608/Assigment2")
write.csv(results_table, "results_table.csv", row.names = FALSE)








