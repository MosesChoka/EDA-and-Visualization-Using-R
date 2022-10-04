library(readr)
insurance <- read_csv("C:/Users/admin/Desktop/DataSessions/insurance.csv")
View(insurance)

summary(insurance)

ggplot(insurance, aes(x = sex)) + geom_bar(fill = 'cornflowerblue', color = 'black') + labs(title = " Distribution of Gender",
                                                                                            x = "Gender",
                                                                                            y = " Frequency")
# Labelling your bars
df <- insurance %>% 
  count(sex)


ggplot(df, aes(x = sex, y = n)) + geom_bar(fill = "cornflowerblue",
                                          color = 'black',
                                          stat = 'identity') + geom_text(aes(label = n), vjust = -0.5)
# Multivariate graph

ggplot(insurance, aes(x = age, y = charges, color = sex )) + geom_point(size = 3,
                                                                     alpha = .6)


# Data cleaning
is.na(insurance)
colSums(is.na(insurance))



# Replacing missing values 
insurance$children[is.na(insurance$children)] <- 0
insurance$children[is.na(insurance$children)]<- median(insurance$children)

# Using VIM package
library(VIM)
insurance <- kNN(insurance, k = 5)
