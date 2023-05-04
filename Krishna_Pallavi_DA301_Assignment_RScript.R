## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')

# The whole tidyverse package.
library(tidyverse)
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

install.packages('ggplot2')

# Import the ggplot2 library.
library(ggplot2)


###############################################################################

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
head(sales)
tail(sales)

# View the dimensions of the data set i.e. the number of rows and columns.
dim(sales)


# View the titles or names of the columns in the data set.
colnames(sales)

# Determine the structure of the data set.
str(sales)

# To search for missing values in a data set.
sales[is.na(sales)]

# Print Summary
summary(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales1 <- subset(sales, 
                   select = -c(Ranking, Platform, Year, 
                               Genre, Publisher))

# View the column names
names(sales1)

# View the data frame.
head(sales1)


# View the descriptive statistics.
summary(sales1)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
ggplot(sales1, aes(x=NA_Sales, y=Global_Sales)) + 
  geom_point()

ggplot(sales1, aes(x=EU_Sales, y=Global_Sales)) + 
  geom_point()

ggplot(sales1, aes(x=NA_Sales, y=EU_Sales)) + 
  geom_point()



## 2b) Histograms
# Create histograms.
hist(sales1$NA_Sales)

hist(sales1$EU_Sales)

hist(sales1$Global_Sales)

## 2c) Boxplots
# Create boxplots.
boxplot(sales1$NA_Sales)

boxplot(sales1$EU_Sales)

boxplot(sales1$Global_Sales)


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# We see a linear relationship between EU, NA sales with Global Sales which
# tells us that there is a fairly good way to predict Global Sales with these two


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

#sales <- read.csv('turtle_sales.csv', header=T)
#head(sales)

# Get Shape of data
#dim(sales)

# Get column names
#colnames(sales)


# Check output: Determine the min, max, and mean values.
# Ranking
min(sales$Ranking)
max(sales$Ranking)

# NA Sales
min(sales$NA_Sales)
max(sales$NA_Sales)
mean(sales$NA_Sales)
var(sales$NA_Sales)
sd(sales$NA_Sales)

# EU Sales
min(sales$EU_Sales)
max(sales$EU_Sales)
mean(sales$EU_Sales)
var(sales$EU_Sales)
sd(sales$EU_Sales)

# Global Sales
min(sales$Global_Sales)
max(sales$Global_Sales)
mean(sales$Global_Sales)
var(sales$Global_Sales)
sd(sales$Global_Sales)


# View the descriptive statistics.
summary(sales1)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

df_sales = sales %>% group_by(Product) %>%
  summarise(NA_Sales_Sum = sum(NA_Sales),
            EU_Sales_Sum = sum(EU_Sales),
            Global_Sales_Sum = sum(Global_Sales),
            .groups='drop')

# View the data frame.
df_sales
write.csv(df_sales,'sales_by_product.csv')

# Explore the data frame.
summary(df_sales)
dim(df_sales)
head(df_sales)
## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(y=NA_Sales_Sum, data=df_sales)
qplot(y=EU_Sales_Sum, data=df_sales)
qplot(y=Global_Sales_Sum, data=df_sales)

# Create Bar charts
ggplot(df_sales, aes(y=Global_Sales_Sum)) + 
  geom_bar()
qplot(y=EU_Sales_Sum, data=df_sales)
qplot(y=Global_Sales, data=sales)

arrange(df_sales, desc(NA_Sales_Sum))

# Create histograms.
hist(df_sales$NA_Sales_Sum)

hist(df_sales$EU_Sales_Sum)

hist(df_sales$Global_Sales_Sum)

# Create boxplots.
boxplot(df_sales$NA_Sales_Sum)

boxplot(df_sales$EU_Sales_Sum)

boxplot(df_sales$Global_Sales_Sum)

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(df_sales$NA_Sales_Sum)
# Specify qqline function.
qqline(df_sales$NA_Sales_Sum) 

qqnorm(df_sales$EU_Sales_Sum)
# Specify qqline function.
qqline(df_sales$EU_Sales_Sum) 

qqnorm(df_sales$Global_Sales_Sum)
# Specify qqline function.
qqline(df_sales$Global_Sales_Sum) 


## 3b) Perform Shapiro-Wilk test

# Install and import Moments.
# Install.packages('moments') 
library(moments)


# Perform Shapiro-Wilk test.
shapiro.test(df_sales$EU_Sales_Sum)
# Skewness and Kurtosis.
skewness(df_sales$EU_Sales_Sum) 
kurtosis(df_sales$EU_Sales_Sum)

# Perform Shapiro-Wilk test.
shapiro.test(df_sales$NA_Sales_Sum)
# Skewness and Kurtosis.
skewness(df_sales$NA_Sales_Sum) 
kurtosis(df_sales$NA_Sales_Sum)

# Perform Shapiro-Wilk test.
shapiro.test(df_sales$Global_Sales_Sum)
# Skewness and Kurtosis.
skewness(df_sales$Global_Sales_Sum) 
kurtosis(df_sales$Global_Sales_Sum)


## 3d) Determine correlation
# Determine correlation.
cor(df_sales$NA_Sales_Sum, df_sales$Global_Sales_Sum)

cor(df_sales$EU_Sales_Sum, df_sales$Global_Sales_Sum)

cor(df_sales$NA_Sales_Sum, df_sales$EU_Sales_Sum)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
arrange(df_sales, desc(NA_Sales_Sum))
df_result1 = arrange(df_sales, desc(NA_Sales_Sum))[c(1:10), c(1:2)]
df_result1
barplot(df_result1$NA_Sales_Sum,df_result1$Product,
        xlab="Product",
        ylab="NA_Sales_Sum",
        names.arg = df_result1$Product,
        main="NA Sales by Product",border="red")

df_result2 = arrange(df_sales, desc(EU_Sales_Sum))[c(1:10), c(1,3)]
df_result2
barplot(df_result2$EU_Sales_Sum,df_result2$Product,
        xlab="Product",
        ylab="EU_Sales_Sum",
        names.arg = df_result2$Product,
        main="EU Sales by Product",border="red")

df_result3 = arrange(df_sales, desc(Global_Sales_Sum))[c(1:10), c(1,4)]
df_result3
barplot(df_result3$Global_Sales_Sum,df_result3$Product,
        xlab="Product",
        ylab="Global_Sales_Sum",
        names.arg = df_result2$Product,
        main="Global Sales by Product",border="red")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

#-	QQPlots we see divergence between -1 & -2, 1 and 2. Indicative of slightly 
#   more heavy tail values.
#-	The Shapiro Test p-values are all very small, telling us that the features 
#   are normally distributed
#-	We see that there is a positive skew in the data, and the kurtosis is higher than 
#   3 indicative of heavier tails


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model

## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(df_sales$NA_Sales_Sum, df_sales$EU_Sales_Sum)
cor(df_sales)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
model1 <- lm(Global_Sales_Sum~NA_Sales_Sum,
             data=df_sales)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# View residuals on a plot.
plot(model1$residuals)

# Plot the relationship with base R graphics.
plot(df_sales$NA_Sales_Sum, df_sales$Global_Sales_Sum)
coefficients(model1)

# Add line-of-best-fit.
abline(coefficients(model1))

qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
colnames(sales)

# Multiple linear regression model.

model2 <- lm(Global_Sales_Sum~NA_Sales_Sum+EU_Sales_Sum, data=df_sales)

summary(model2)
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# predict for model1
new_na_sales <- data.frame( NA_Sales_Sum = c (34.02, 3.93, 2.73, 2.26, 22.08))
new_na_sales
predict(model1,
        newdata=new_na_sales)

new_multi_sales <- data.frame(NA_Sales_Sum = c (34.02, 3.93, 2.73, 2.26, 22.08), 
                              EU_Sales_Sum = c(23.80, 1.56, 0.65, 0.97, 0.52))
new_multi_sales
predict(model2,
        newdata=new_multi_sales)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Multiple regression model works better for us when we compare predictions
# The R square of multiple regression is better and can be seen in the predictions

###############################################################################
###############################################################################




