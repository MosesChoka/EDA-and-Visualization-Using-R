# Load data into R

library(readxl)
T_Data <- read_excel("T_Data.xlsx")
View(T_Data)

# load tidyverse package
library(tidyverse)

# Remove the first row
T_Data <- T_Data %>% slice(-1)
View(T_Data)

# Rename column names

colnames(T_Data)

colnames(T_Data)[colnames(T_Data) == '...1'] <- 'Depth_cm'

colnames(T_Data)

str(T_Data)

# Convert THT and THC variables to numeric and the Depth_cm to factor
T_Data$Depth_cm <- as.factor(T_Data$Depth_cm)
T_Data$THT <- as.numeric(T_Data$THT)
T_Data$THC <- as.numeric(T_Data$THC)


# Convert the data set to a data frame
as.data.frame(T_Data)


# Check for missing values in the columns
colSums(is.na(T_Data))

# Remove the missing values

data2 <- na.omit(T_Data)


colSums(is.na(data2))

str(data2)

# Reshape the data

data3 <- data2 %>% 
  pivot_longer(
    c("THT","THC"),
    names_to = "Treatments",
    values_to = "tc_ha"
  )

colnames(data3)

str(data3)

# Rename the levels
levels(data3$Depth_cm)

levels(data3$Depth_cm)<- c('0_10', '70_100','10_30','30_50','50_70')


str(data3)


# PLOTTING Box plot

# Prepare the data for visualization
data3 <- data3 %>%
  group_by(Treatments, Depth_cm, tc_ha) %>%
  summarize(n = n(),
            mean = mean(tc_ha),
            sd = sd(tc_ha),
            se = sd / sqrt(n))

# Plot the box plot
boxp<- ggplot(data3, aes(x = Treatments,
                   y = tc_ha, fill = Treatments)) +
  geom_boxplot(alpha = .7) +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                width = .1) +
  scale_y_continuous(breaks = seq(10, 40, 10)) +
  scale_fill_manual(values = c("#66ffb2", "#66b2ff")) +
  facet_grid(.~Depth_cm) +
  labs( title = "Depths vs Treatment")+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


boxp
  
# Statistical test

s_test <- aov(tc_ha~Depth_cm, data = data3)
summary(s_test)


# Show whether there's a statistically significant difference between any of the two groups on the graph
boxp + geom_signif(comparisons = list(c("THC","THT")),map_signif_level = TRUE)


