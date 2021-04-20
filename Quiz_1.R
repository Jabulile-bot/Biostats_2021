#Author: Jabulile Leroko
#Quiz_1


# QUESTIO 1 ---------------------------------------------------------------

#Data classe
1. Numerical data. This quantitative data, it can be continuous or distcrete,
also includes dates. Continuous one is measured things like weight and then
dicrete would be counts, like how many kids each parent has. 

2. Qualitative data is about the quality,which includes categorical and ordinal data.
Categorical would be how many flowers are under the factors pink, purple and white
and the ordinal would be things that can be ranked, like how many whatever fall under...
freezing, cold, lukewarm, warm, boiling

3. There's binary' data which is for either-or. true or false. Good or bad

4. Character values would be words, not necessarily numbers... 
like species, sites in biology. 

5. Missing values are the NAs in our data.

#Functions for viewing data
view(), head(), summary(), tail(), str()

#Skewness
Most times the data is not normally distributed. if most points are on the right,
there will be a tail on the left and that data is skewed to the right and vice versa

#Kurtosis
this one tells whether there are extreme values, values too far from the mean,
tells us how much the tails of a distribution vary from a normaly distributed data



# QUESTION 2 --------------------------------------------------------------
library(tidyverse)
Orange <- datasets::Orange
str(Orange)
#Orange has 3 columns, one contains ordinal data (Tree categories) and 
#2 contain continuous data (age, circumference)

Orange[1:6,] #To show the first 6 rows
tail(Orange) #To show the last 6 rows
summary(Orange)
colnames(Orange)


Orange %>%
  summarise(mean_age = mean(age),
            med_age = median(age),
            sd_age = sd(age),
            mean_circ = mean(circumference),
            med_circ = median(circumference),
            sd_circ = sd(circumference))

library(e1071)
kurtosis(Orange$age)  
kurtosis(Orange$circumference)

skewness(Orange$age)
skewness(Orange$circumference)


Orange %>% 
  summarise(min_circ = min(circumference),
            qrt1_circ = quantile(circumference, p = 0.25),
            qrt3_circ = median(circumference, p = 0.75),
            max_circ = max(circumference))
?Orange

Orange %>%
  ggplot(aes(x = Tree, y = circumference)) +
  geom_boxplot(fill = "green") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Circumference of trees") + 
  theme_pubr()


# QUESTION 3 --------------------------------------------------------------

separate() splits a variable into 2
gather() reshapes the data from wide to long

