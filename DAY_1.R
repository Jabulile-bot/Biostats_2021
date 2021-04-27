#Jabulile Leroko


library(tidyverse)
library(tidyr)
library(dplyr)
#1. Interested in the data PlantGrowth, load the built-in data as an R object
Growth <- PlantGrowth


#let me see the structure of the data
str(Growth) #Has 30 obs. of 2 variables, weight (num) and group(fctr)
summary(Growth)
                #What is it about dou?
?PlantGrowth

#Boxplot to see distribution 
ggplot(Growth, aes(x = group, y = weight)) +
  geom_boxplot(aes(fill = "group")) +theme_classic()

#From the boxplot, it looks like group trt1 is not normally distributed

#Test for normality and homoscedasticity
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

Growth %>% 
  group_by(group) %>% 
  summarise(group_var = two_assum(weight)[1],
            group_norm = two_assum(weight)[2])
#group trt1 is not similar to group trt2, group ctr1 is similar to both trt1 and trt2
#Only group trt1 is not normally distributed 

#In my hypothesis testing, I will just deal with the normally distributed and homogenous groups

#H0: group ctrl is not differnt to trt2

?PlantGrowth
#T-test:
t.test(weight ~ group, data = Growth %>% filter(group == c("ctrl", "trt2"),
                                                var.equal = TRUE))

#P-value< 0.07667, not significant to reject Null hypothesis
#Therefore, the two groups are not different from each other

#2. airquality
aq <- airquality
summary(aq)
str(aq)


grp_stat <- aq %>%
  group_by(Month, Temp, Wind) %>% 
  summarise(mean_Wind = round(mean(Wind, na.rm = TRUE), 2),
            med_Wind = median(Wind, na.rm = TRUE),
            sd_Wind = round(sd(Wind, na.rm = TRUE), 2),
            sum_Wind = sum(Wind),
            min_Wind = min(Wind),
            qrt1_Wind = quantile(Wind, p = 0.25),
            med_Wind = median(Wind),
            qrt3_Wind = median(Wind, p = 0.75),
            max_Wind = max(Wind),
            n_Wind = n())
grp_stat

#Want to plot something to see distribution
data_histogram <- aq %>%
  mutate(Month = factor(Month)) %>%
  group_by(Month) %>%
  summarize(mean_Wind = round(mean(Wind), 2))
ggplot(data_histogram, aes(x = Month, y = mean_Wind)) +
  geom_bar(fill = "coral", stat = "identity") +
  labs(y = "Temperature ()") + 
  theme_classic()


#H0: Wind strength means of all months are equal, meaning wind strength does not change
#HA: Wind strength means of all months are not equal, wind strength changes

#Tests of normality and homogeneousity
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

aq %>% 
  group_by(Month) %>% 
  summarise(group_var = two_assum(Wind)[1],
            group_norm = two_assum(Wind)[2])
#From the above tests, all the samples are normally distributed as p>0.05,...
#and the samples are similar to each other because no sample has var 4x> than the other

#ANOVA
aov1 <- aov(Wind~Month, data = aq)
summary(aov1)
#p-value <0.0275, there is significant evidence to reject the null hypothesis
#therefore, wind strength changes by month

#Descriptive stats

#How to calc sample size... use unique()
# first load the tidyverse packages that has the pipe operator, %>% >loaded already

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

#H0: Doet has no effect on chicks weight
#HA: Diet has effect on the weight of chicks
#Check normality and homoscedasticity of the chicks weight at time 20
chicks %>% filter(Time == "20") %>%
  group_by(Diet) %>% 
  summarise(weight_var = two_assum(weight)[1],
            weight_norm = two_assum(weight)[2])
#All samples have p>0.05, therefore, normally distributed
#No sample has var 4x> than the other, therefore, samples are similar

#ANOVA
aov1 <- aov(weight~Diet, data = chicks)
summary(aov1)
#p< 6.43e-7, statistically significant to reject the null hypthesis
#Chick weight is affected by diet.