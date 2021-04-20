#Jabulile Leroko

#1. Interested in the data PlantGrowth, load the built-in data as an R object
Growth <- PlantGrowth
#let me see the structure of the data
str(Growth) #Has 30 obs. of 2 variables, weight (num) and group(fctr)

#What is it about dou?
?PlantGrowth

View(Growth)

#2. Titanic
#What is it about?
?Titanic
#= On survival of titanic passengers

#Structure
str(Titanic) 
head(Titanic)
View(Titanic)

#Descriptive stats

#How to calc sample size... use unique()
# first load the tidyverse packages that has the pipe operator, %>%
library(tidyverse)
chicks <- as_tibble(ChickWeight)
nrow(chicks) #nrow doesn't give the sample size, rows are observations and here
#...there are multiple obs. for each individ

unique(chicks$Chick)
#There are 50 unique chickens that were sampled
#So, sample size = 50

#So, note the difference b/w nrow() and unique(chicks$Chick)

#We can summarise the final mass of the chicken weights
#In a normally distributed pop., the mean would beat the centre of the 
#...observations and equal to the median
#mean = sum of all x/n

#Calc mean of chick weights that made it to day20
library(tidyverse)
chicks %>% filter(Time == "20") %>%
  group_by(Diet) %>%
  summarise(mean_weight = mean(weight),
            med_weight = median(weight),
            sd_weight = sd(weight))

chicks %>% filter(Time == "20") %>%
  group_by(Diet) %>%
  summarise(med_weight = median(weight))

library(e1071)
kurtosis(chicks$weight)
#leptokurtic

#Now how much is the spread of the data from the mean

chicks %>% filter(Time == "20") %>%
  group_by(Diet) %>%
  summarise(sd_weight = sd(weight))

#Quantiles
chicks %>% filter(Time == "20") %>% 
  group_by(Diet) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))
#to find min and max (range)
range(chicks$weight)
#Or
chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

#Missing values
dat1 <- c(NA, 12, 76, 34, 23)

# Without telling R to ommit missing data
mean(dat1)
# Ommitting the missing data
mean(dat1, na.rm = TRUE)
