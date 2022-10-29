library(readxl)
Copy_of_Cancer_Risk_Complete_2 <- read_excel("D:/TODO LIST/Copy_of_Cancer_Risk_Complete_2.xlsx")
View(Copy_of_Cancer_Risk_Complete_2)

# Univariate Analysis
# 1. Descriptive statistics
summary(Copy_of_Cancer_Risk_Complete_2)

# 2.a) Histograms
library(ggplot2)
ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Calories)) + 
  geom_histogram(bins = 20,fill ="cornflowerblue",color = "white") +
  labs(title = "A histogram for Calories")

ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Age)) + 
  geom_histogram(bins = 10,fill ="cornflowerblue",color = "white") + 
  labs(title = "A histogram for Age")

ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Fat)) +
  geom_histogram(bins = 20, fill ="cornflowerblue",color = "white")+
  labs(title = "A histogram for Fat")

# 2. b) Boxplots
ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Calories)) +
  geom_boxplot()+ labs(title = "Boxplot for Calories")

ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Age)) +
  geom_boxplot()+ labs(title = "Boxplot for Age")

ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = Fat)) +
  geom_boxplot()+ labs(title = "Boxplot for Fat")


# 3. A categorical variable from age
# a) Create a categorical variable
Copy_of_Cancer_Risk_Complete_2$agecat<- as.factor(ifelse(Copy_of_Cancer_Risk_Complete_2$Age < 40, 'A',
                   ifelse(Copy_of_Cancer_Risk_Complete_2$Age <= 40 & Copy_of_Cancer_Risk_Complete_2$Age < 49, 'B',
                   ifelse(Copy_of_Cancer_Risk_Complete_2$Age<= 49 & Copy_of_Cancer_Risk_Complete_2$Age< 61, 'C',
                   ifelse(Copy_of_Cancer_Risk_Complete_2$Age >= 61, 'D','E')))))
agecat = Copy_of_Cancer_Risk_Complete_2$agecat
class(agecat)
# Create a frequency table
frequency_table <- table(agecat)
frequency_table

# b)Create a pie chart 
categorical_cat <- as.data.frame(agecat)
categorical_cat

cols <- hcl.colors(length(levels(agecat)), "Fall")

PieChart(agecat, 
         data = categorical_cat,
         hole = 0, fill = cols, labels_cex = 0.6)

# Plotting an ordered bar chart
library(dplyr)
plotdata <- categorical_cat %>%
  count(agecat)

ggplot(plotdata,aes(x = reorder(agecat,n),
                    y = n)) + geom_bar(stat = "identity")
                        
# MULTIVARIATE ANALYSIS
cont_table <- table(Copy_of_Cancer_Risk_Complete_2$Gender, Copy_of_Cancer_Risk_Complete_2$SmokeStat)
cont_table

cont_table1 <- addmargins(cont_table)
cont_table1

cont_table2 <- prop.table(cont_table1 )
cont_table2

prop_table <- prop.table(cont_table)
round(prop_table,2)

round(addmargins(prop_table * 100),2)

# Stacked bar chart
Copy_of_Cancer_Risk_Complete_2$Gender <- as.factor(Copy_of_Cancer_Risk_Complete_2$Gender)
levels(Copy_of_Cancer_Risk_Complete_2$Gender)<- c('Male','Female')

Copy_of_Cancer_Risk_Complete_2$SmokeStat <- as.factor(Copy_of_Cancer_Risk_Complete_2$SmokeStat)
levels(Copy_of_Cancer_Risk_Complete_2$SmokeStat)<- c('1','2','3')

ggplot(Copy_of_Cancer_Risk_Complete_2,
       aes(x = Gender,
           fill = SmokeStat)) +
  geom_bar(position = "stack") + labs(title = 'Gender vs SmokeStat')


# APPLICATION
ggplot(Copy_of_Cancer_Risk_Complete_2,
       aes(x = Calories,
           y = Age)) +
  geom_point(color="cornflowerblue") + labs(title = "A scatter plot showing relationship between Age and Calories")


Copy_of_Cancer_Risk_Complete_2$Calories <- as.numeric(Copy_of_Cancer_Risk_Complete_2$Calories)
ggplot(Copy_of_Cancer_Risk_Complete_2,
       aes(x = agecat,
           y = Calories )) +
  geom_bar(stat = "identity") + labs(title = "A bar plot showing relationship between Age and Calories")

boxp<- ggplot(Copy_of_Cancer_Risk_Complete_2, aes(x = agecat,
                         y = Calories, fill = agecat)) +
  geom_boxplot(alpha = .7) + labs(title = 'A boxplot showing the association between age and calories')

boxp

ggplot(Copy_of_Cancer_Risk_Complete_2,
       aes(x = Calories,
           fill = agecat)) +
  geom_density(alpha = 0.4) +
  labs(title = "Calories distribution by age")

ggplot(Copy_of_Cancer_Risk_Complete_2,
       aes(y = Calories,
           x = agecat,
           color = agecat)) +
  geom_jitter() +
  labs(title = "Calories distribution by Age")
