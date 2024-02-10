#4.1. Comprehensive descriptive statistical analysis
#Load dataset  
world_data <- read.csv("P_Data_Extract_From_World_Development_Indicators (new).csv", check.names = FALSE)
world_data


 
#Data inspection
head(world_data)
summary(world_data)
str(world_data)


library(tidyverse)
install.packages("e1071")
library(e1071)

# List of columns to analyze based on the research objective
columns_to_analyze <- c(
  "Current health expenditure (% of GDP)",
  "Current health expenditure per capita (current US$)",
  "Domestic general government health expenditure (% of GDP)",
  "Domestic general government health expenditure (% of general government expenditure)",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Physicians (per 1,000 people)",
  "Life expectancy at birth, total (years)",
  "Mortality caused by road traffic injury (per 100,000 population)",
  "Life expectancy at birth, female (years)",
  "Life expectancy at birth, male (years)",
  "Population ages 0-14 (% of total population)",
  "Population ages 15-64 (% of total population)",
  "Population ages 80 and above, female (% of female population)",
  "Population ages 80 and above, male (% of male population)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)")

# Function to calculate comprehensive statistics
calculate_stats <- function(columns_to_analyze) {
  data <- world_data[[columns_to_analyze]]
  stats <- c(
    Mean = mean(data, na.rm = TRUE),
    Median = median(data, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(data), decreasing = TRUE))[1]), # Basic mode calculation
    StandardDeviation = sd(data, na.rm = TRUE),
    Skewness = skewness(data, na.rm = TRUE),  
    Kurtosis = kurtosis(data, na.rm = TRUE),  
    Range = diff(range(data, na.rm = TRUE)),
    IQR = IQR(data, na.rm = TRUE),
    Variance = var(data, na.rm = TRUE),
    Quantiles = quantile(data, na.rm = TRUE)
  )
  return(stats)
}

# Apply the function to each column and print results
for (column in columns_to_analyze) {
  cat("\nDescriptive Statistics for:", column, "\n")
  print(calculate_stats(column))
} 
# Apply the function to each column and store results
comprehensive_stats <- lapply(columns_to_analyze, calculate_stats)

# Viewing the results
print(comprehensive_stats)

#Missing values
# Calculate the percentage of missing data in each column
missing_percentages <- sapply(world_data, function(x) mean(is.na(x)) * 100)
missing_percentages


# Display the percentages for columns with missing data
missing_percentages[missing_percentages > 0]

# Frequency of categorical variables
sapply(world_data[sapply(world_data, is.factor)], table)

#============================================
#Median Imputation for columns with low missing data percentage

# List of columns to fill missing values with the median
columns_to_fill <- c(
  "Current health expenditure (% of GDP)",
  "Current health expenditure per capita (current US$)",
  "Domestic general government health expenditure (% of GDP)",
  "Domestic general government health expenditure (% of general government expenditure)",
  "Out-of-pocket expenditure (% of current health expenditure)",
  "Mortality caused by road traffic injury (per 100,000 population)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70, female (%)",
  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70, male (%)",
  "Physicians (per 1,000 people)"
)

# Loop through each column and fill missing values with median
for (col in columns_to_fill) {
  world_data[[col]] <- ifelse(
    is.na(world_data[[col]]),
    median(world_data[[col]], na.rm = TRUE),
    world_data[[col]]
  )
}

# Check for missing values after filling
sapply(world_data[columns_to_fill], function(x) sum(is.na(x)))


# Load necessary libraries
library(caret)
library(rpart)

#  column names for the predictors
predictors <- world_data[, c("Current health expenditure (% of GDP)", "Life expectancy at birth, total (years)", "Out-of-pocket expenditure (% of current health expenditure)")]

# Splitting the data
data_with_physicians <- world_data[!is.na(world_data$`Physicians (per 1,000 people)`), ]
data_without_physicians <- world_data[is.na(world_data$`Physicians (per 1,000 people)`), ]

# Building the model
model <- rpart(`Physicians (per 1,000 people)` ~ ., data = data_with_physicians, method = "anova")

# Predicting and replacing missing values
predicted_physicians <- predict(model, newdata = data_without_physicians)
world_data$`Physicians (per 1,000 people)`[is.na(world_data$`Physicians (per 1,000 people)`)] <- predicted_physicians


summary(model)
sum(is.na(world_data$`Physicians (per 1,000 people)`))

# Boxplot for Current health expenditure per capita
ggplot(world_data, aes(x = factor(0), y = `Current health expenditure per capita (current US$)`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Current Health Expenditure Per Capita", x = "", y = "Expenditure (US$)")

# Boxplot for Life expectancy at birth, total (years)
ggplot(world_data, aes(x = factor(0), y = `Life expectancy at birth, total (years)`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Life Expectancy at Birth", x = "", y = "Years")

# Histogram for Out-of-pocket expenditure (% of current health expenditure)
ggplot(world_data, aes(x = `Out-of-pocket expenditure (% of current health expenditure)`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of Out-of-Pocket Health Expenditure", x = "Out-of-pocket expenditure (%)", y = "Count")

# Histogram for Physicians (per 1,000 people)
ggplot(world_data, aes(x = `Physicians (per 1,000 people)`)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of Physician Density", x = "Physicians (per 1,000 people)", y = "Count")
library(ggplot2)

#================================================
#Plots with Two Variables

qplot(data = world_data, x = `Life expectancy at birth, total (years)`, y = `Current health expenditure (% of GDP)`, col = "red")

ggplot(world_data, aes(x = `Country Name`, y = `Current health expenditure per capita (current US$)`, color = `Country Name`)) + geom_boxplot()


#Plots for a Single Sample
# Histogram of Physicians Density (To view the distribution of a single continuous variable)
ggplot(world_data, aes(x = `Physicians (per 1,000 people)`, fill = `Country Name`)) +
  geom_histogram(binwidth = 1.0) +
  scale_fill_viridis_d()

#Density Plot: To view the distribution's density.
# Density of Out-of-Pocket Expenditure
ggplot(world_data, aes(x = `Out-of-pocket expenditure (% of current health expenditure)`, fill = `Country Name`)) + geom_density(alpha = 0.5) + scale_fill_viridis_d()

#Multivariate Plots:
#Pair Plot: To visualize pairwise relationships in a dataset.
pairs(world_data[, c("Current health expenditure (% of GDP)", 
                     "Life expectancy at birth, total (years)", 
                     "Physicians (per 1,000 people)")])

#Heatmap: To visualize correlation between multiple variables.
numeric_columns <- c("Current health expenditure (% of GDP)", 
                     "Life expectancy at birth, total (years)", 
                     "Physicians (per 1,000 people)", 
                     "Out-of-pocket expenditure (% of current health expenditure)", 
                     "Domestic general government health expenditure (% of GDP)")

corr_matrix <- cor(world_data[, numeric_columns], use = "complete.obs")
heatmap(corr_matrix)

#=======================================================================
#4.2. Correlation Analysis


install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
#1. Time Series Correlation

# Time Series Correlation
library(ggplot2)
ggplot(world_data, aes(x = Year, y = `Current health expenditure (% of GDP)`, group = `Country Name`, color = `Country Name`)) +
  geom_line() +
  labs(title = "Time Series of Health Expenditure by Country", x = "Year", y = "Health Expenditure (% of GDP)")



#======================================================
#2. Correlation Between Categorical Variables

# Install and load the DescTools package
install.packages("DescTools")
library(DescTools)

# Create a contingency table
contingency_table <- table(world_data$`Country Name`, world_data$`Age dependency ratio (% of working-age population)`)

# Calculate Cramer's V
cramersV_result <- CramerV(contingency_table)

print(cramersV_result)

# 3.  Correlation Between Numerical and Categorical Variables: ANOVA
# Using 'Life expectancy at birth, total (years)' as a numerical variable
# 'Country Name' as a categorical variable
anova_result <- aov(`Life expectancy at birth, total (years)` ~ `Country Name`, data = world_data)
summary(anova_result)

#Correlation Between Two Discrete Variables
# Calculating Spearman's rank correlation
spearman_correlation <- cor(world_data$`Mortality caused by road traffic injury (per 100,000 population)`, 
                            world_data$`Domestic general government health expenditure (% of GDP)`, 
                            method = "spearman", 
                            use = "complete.obs")

# Print the Spearman's rank correlation result
print(spearman_correlation)

#4.Correlation Matrix: Correlations Between Multiple Continuous Variables]# Correlation matrix
# Load necessary libraries

library(corrplot)
library(RColorBrewer)

# Abbreviated column names 
abbreviated_names <- c("LifeExp" = "Life expectancy at birth, total (years)",
                       "HealthExpGDP" = "Current health expenditure (% of GDP)",
                       "Physicians" = "Physicians (per 1,000 people)")

# Calculate the correlation matrix using actual column names
cor_matrix <- cor(world_data[, unname(abbreviated_names)], use = "complete.obs")

# Define color palette
col <- brewer.pal(n = 8, name = "RdBu")
# Create the correlation plot with the color palette and correct labels
corrplot::corrplot(cor_matrix, method = "color", col = col, 
                   order = "hclust", addCoef.col = "black", 
                   tl.col = "black", 
                   tl.cex = 0.6, cl.cex = 0.6)


#Confidence Interval Estimations
#4.
confint(lm(`Life expectancy at birth, total (years)` ~ `Current health expenditure (% of GDP)`, data = world_data))

#5. Multilevel / Hierarchical Analysis

library(lme4)

# multilevel model
model <- lmer(`Current health expenditure (% of GDP)` ~ 1 + (1 | `Country Name`), data = world_data)

# View the summary of the model
summary(model)
#=============================

#4.3 HYPOTHESIS TESTING
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
library(ggplot2)
library(datarium)
library(qqplotr)

#Normality Checks
library(ggplot2)

#Visual Method: Q-Q Plots
# Q-Q Plot for Current Health Expenditure (% of GDP)
qqnorm(world_data$`Current health expenditure (% of GDP)`, main = "Q-Q Plot for Health Expenditure (% of GDP)")
qqline(world_data$`Current health expenditure (% of GDP)`)

# Q-Q Plot for Life Expectancy at Birth - Male
qqnorm(world_data$`Life expectancy at birth, male (years)`, main = "Q-Q Plot for Male Life Expectancy")
qqline(world_data$`Life expectancy at birth, male (years)`)

# Q-Q Plot for Life Expectancy at Birth - Female
qqnorm(world_data$`Life expectancy at birth, female (years)`, main = "Q-Q Plot for Female Life Expectancy")
qqline(world_data$`Life expectancy at birth, female (years)`)

# Q-Q Plot for Age Dependency Ratio
qqnorm(world_data$`Age dependency ratio (% of working-age population)`, main = "Q-Q Plot for Age Dependency Ratio")
qqline(world_data$`Age dependency ratio (% of working-age population)`)


#Statistical Method: Shapiro-Wilk Test
# Shapiro-Wilk Test for Health Expenditure (% of GDP)
shapiro.test(world_data$`Current health expenditure (% of GDP)`)

# Shapiro-Wilk Test for Male Life Expectancy
shapiro.test(world_data$`Life expectancy at birth, male (years)`)

# Shapiro-Wilk Test for Female Life Expectancy
shapiro.test(world_data$`Life expectancy at birth, female (years)`)

# Shapiro-Wilk Test for Age Dependecy Ratio
shapiro.test(world_data$`Age dependency ratio (% of working-age population)`)


#====================
# Applying log transformation to 'Current health expenditure (% of GDP)'
world_data$`Current health expenditure (% of GDP)_log` <- log(world_data$`Current health expenditure (% of GDP)`)

# Check for normality after the transformation
shapiro.test(world_data$`Current health expenditure (% of GDP)_log`)

# Generating a Q-Q plot to visualize normality
qqnorm(world_data$`Current health expenditure (% of GDP)_log`)
qqline(world_data$`Current health expenditure (% of GDP)_log`)

###
# Apply log transformation to life expectancy variables
world_data$`Life expectancy at birth, male (years)_log` <- log(world_data$`Life expectancy at birth, male (years)`)
world_data$`Life expectancy at birth, female (years)_log` <- log(world_data$`Life expectancy at birth, female (years)`)

# Check for normality after the transformation
shapiro.test(world_data$`Life expectancy at birth, male (years)_log`)
shapiro.test(world_data$`Life expectancy at birth, female (years)_log`)

#Non-parametric approach
#Spearman's Rank Correlation: To assess the correlation between age dependency ratios and health expenditure:
spearman_result <- cor.test(world_data$`Age dependency ratio (% of working-age population)`, world_data$`Current health expenditure (% of GDP)`, method = "spearman")
print(spearman_result)

#Wilcoxon Signed-Rank Test: To compare life expectancy differences between genders, 
wilcoxon_result <- wilcox.test(world_data$`Life expectancy at birth, male (years)`, world_data$`Life expectancy at birth, female (years)`, paired = FALSE)
print(wilcoxon_result)


#T-test: Gender-Based Life Expectancy Comparison
# Assuming 'Life expectancy at birth, male (years)' and 'Life expectancy at birth, female (years)' are the variables
t_test_result <- t.test(world_data$`Life expectancy at birth, male (years)`, 
                        world_data$`Life expectancy at birth, female (years)`,
                        paired = FALSE) # Use paired = TRUE if the samples are paired

# Print the t-test result
print(t_test_result)


# Divide countries into two groups based on out-of-pocket health expenditure
# Assuming 'HighExpenditure' and 'LowExpenditure' are logical vectors categorizing the countries
HighExpenditure <- world_data$`Out-of-pocket expenditure (% of current health expenditure)` > median(world_data$`Out-of-pocket expenditure (% of current health expenditure)`, na.rm = TRUE)
LowExpenditure <- !HighExpenditure

# Perform Mann-Whitney U test
mann_whitney_result <- wilcox.test(world_data$`Life expectancy at birth, total (years)`[HighExpenditure],
                                   world_data$`Life expectancy at birth, total (years)`[LowExpenditure])

# View the test result
print(mann_whitney_result)


#Test of Independence: Association between Country Income Level and Mortality Rates
# Categorizing countries into high-income and low-income groups
world_data$IncomeLevel <- ifelse(world_data$`Current health expenditure per capita (current US$)` >= median(world_data$`Current health expenditure per capita (current US$)`, na.rm = TRUE), 
                                 "High-Income", "Low-Income")

# Chi-square test for association between income level and mortality rate
chi_sq_test_result <- chisq.test(table(world_data$IncomeLevel, 
                                       world_data$`Mortality caused by road traffic injury (per 100,000 population)`))

# Print the chi-square test result
print(chi_sq_test_result)

#4.4 REGRESSION ANALYSIS

#====================================================

# Full model with all predictors
full_model <- lm(`Current health expenditure per capita (current US$)` ~ `Age dependency ratio (% of working-age population)` + `Population ages 0-14 (% of total population)` + `Population ages 15-64 (% of total population)` + `Population ages 80 and above, female (% of female population)` + `Population ages 80 and above, male (% of male population)`, data = world_data)

full_model
# Backward Stepwise Regression
backward_model <- step(full_model, direction = "backward")

# Summary of the final model
summary(backward_model)


library(ggplot2)

#1. Plot for 'Age dependency ratio (% of working-age population)' vs 'Current health expenditure per capita (current US$)'
ggplot(world_data, aes(x = `Age dependency ratio (% of working-age population)`, y = `Current health expenditure per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Age Dependency Ratio and Health Expenditure per Capita",
       x = "Age Dependency Ratio (% of Working-Age Population)",
       y = "Current Health Expenditure per Capita (current US$)")


# 2. Homoscedasticity Check
plot(backward_model$fitted.values, backward_model$residuals)
abline(h = 0, col = "red")

# 3. Normality Check
qqnorm(backward_model$residuals)
qqline(backward_model$residuals)

# Install the necessary packages
install.packages("car")
install.packages("lmtest")
library("car")
library("lmtest")
# 4. Independence Check
durbinWatsonTest(backward_model)

# 5. Multicollinearity Check
vif(backward_model)

# Scatter plot for linearity check
plot(world_data$`Age dependency ratio (% of working-age population)`, residuals(backward_model), 
     xlab = "Age dependency ratio (% of working-age population)", 
     ylab = "Residuals")
abline(h = 0, col = "red")
 
#=========================================
# Install and load required packages
install.packages("MASS")
library(MASS)
install.packages("mgcv")
library(mgcv)

install.packages("gam")
library(gam)


# Split data into training and test sets
set.seed(123) # for reproducibility
training_indices <- sample(1:nrow(world_data), 0.8 * nrow(world_data))
training_data <- world_data[training_indices, ]
test_data <- world_data[-training_indices, ]

# Fit different models
model_linear <- lm(`Current health expenditure per capita (current US$)` ~ `Age dependency ratio (% of working-age population)` + `Physicians (per 1,000 people)` + `Current health expenditure (% of GDP)`, data = training_data)
model_poly <- lm(`Current health expenditure per capita (current US$)` ~ poly(`Current health expenditure (% of GDP)`, 2) + `Physicians (per 1,000 people)`, data = training_data)

model_linear
model_poly


#4.5 TIME SERIES:

#1. Creating the Time Series Object
health_expenditure_ts <- ts(world_data$`Current health expenditure per capita (current US$)`, 
                            start = c(2010), 
                            end = c(2021), 
                            frequency = 1)

#2. Plotting the Time Series
plot(health_expenditure_ts, 
     main = "Time Series of Current Health Expenditure Per Capita",
     xlab = "Year", 
     ylab = "Health Expenditure per Capita (US$)")

diff_health_expenditure_ts <- diff(health_expenditure_ts, differences = 1)


#3.Aggregating Data
library(dplyr)

aggregated_data <- world_data %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)))

#4.Augmented Dickey-Fuller Test
library(tseries)

adf.test(health_expenditure_ts)


#6. ARIMA Modeling
library(forecast)

arima_model <- auto.arima(health_expenditure_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(arima_model)


#7. Forecasting
forecasted_values <- forecast(arima_model, h=10)
plot(forecasted_values)
arima_model <- auto.arima(health_expenditure_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(arima_model)


#8. Evaluation
accuracy(forecasted_values)

#Checking for the performance of the model
# AIC from ARIMA model
arima_aic <- arima_model$aic

# Forecast accuracy measures for ARIMA model
arima_accuracy <- accuracy(forecast(arima_model, h = 10))

# Output the AIC and accuracy measures
print(paste("ARIMA Model AIC:", arima_aic))
print(arima_accuracy)


############
 
# Fit an ETS model
ets_model <- ets(health_expenditure_ts)
summary(ets_model)

# Forecast with the ETS model
ets_forecast <- forecast(ets_model, h=10)
plot(ets_forecast)

# Calculate accuracy of the ETS forecast
ets_accuracy <- accuracy(ets_forecast)
print(ets_accuracy)


