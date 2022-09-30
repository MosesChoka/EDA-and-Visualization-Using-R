# get your current working environment
getwd()

# set your working directory
setwd("C:/Users/admin/Desktop/DataSessions")
getwd()

# installing libraries
# tidyverse, ggplot2, dplyr
install.packages("ggplot2")
install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)


# IMPORT DATA TO YOUR WORKSPACE
#1. From csv file / 

library(readr)
insurance <- read_csv("insurance.csv",)
View(insurance)

#2. From excel files

library(readxl)
dataset <- read_excel(NULL)
View(dataset)

#3. From SPSS files
library(haven)
dataset <- read_sav(NULL)
View(dataset)

# EXPLORATORY DATA ANALYSIS
# a)
head(insurance, n = 10) # outputs the top entries in your data
tail(insurance, n = 10) # outputs the bottom entries in your data
ncol(insurance) # outputs the number of columns in your data
nrow(insurance) # outputs the number of rows in your data
str(insurance) # describes the structure of your data ; this summarises a multiple operations
names(insurance) # outputs the columns names
summary(insurance) # outputs the descriptive statistics of your data


# DATA CLEANING
is.null()
na.omit()
mean(df$var, na.rm = TRUE) # Imputing missing values
# Alternatively, you can use Hmisc package
library(Hmisc)
x = c(1,4,5,6,7,NA)
x <- impute(x, fun = mean)

# Dealing missing values in PIpelies
blueprint = recipes::recipe(y~. , data = ) %>% 
  step_impute_mean(all_numeric()) %>%
  step_impute_knn(all_nominal())
step_nzv(all_numeric()) %>% 
  step_YeoJohnson(all_numeric())

# EXPLORING YOUR DATA USING VISUALIZATIONS

library(ggplot2)

#1. Univariate
#2. Bivariate
#3. Multivariate

# Univariate
hist()
density()
bar()
pie()

# Bivariate
plot() # numerical/numerical
corrplot() # association/correlation
stackedbar() # categorical vs categorical
bar() # categorical/quantitative

# Multivariate
ggplot() + geom_point()


# Regression / Correlation Analysis
  
install.packages("mosaicData")
library(mosaicData)
library(ggplot2)
library(scales)


marriage_data <- data(Marriage, package = "mosaicData")
Marriage
names(marriage_data)

# UNIVARIATE GRAPHS

# Categorical Data
ggplot(Marriage, aes (x = race)) + geom_bar(fill = "cornflowerblue",
                                            color = "black") + labs(x = "Race",
                                                                    y = "Frequency",
                                                                    title = "Participants by Race")

# Numerical data

ggplot(Marriage, aes(x=age)) + geom_histogram( fill = "cornflowerblue",
                                               color = "black",
                                               bins = 10
                                               ) + labs(x = "Age",
                                                                       y = "Frequency",
                                                                       title = "Participants by Age",
                                                                       subtitle = "Age distribution")

# BIVARIATE GRAPHS
# Categorical vs Categorical
data(mpg, package = "ggplot2")
mpg

ggplot(mpg, aes(x = class, fill = drv)) + geom_bar(position = "stack") + labs(x = "Class",
                                                                              y = "Count",
                                                                              title = "Car class vs drv")
# Numerical vs numerical
ggplot(insurance, aes(x = bmi, y = charges)) + geom_point()


select(starwars, gender == "feminine" &
         homeworld == "Alderaan")
starwars
