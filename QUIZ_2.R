#Jabulile Leroko

#QUESTION 1
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)

Orange <- datasets::Orange
?Orange
#H0: The older the tree the higher the circumference
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

Orange %>% 
  group_by(Tree) %>% 
  summarise(tree_var = two_assum(age)[1],
            tree_norm = two_assum(age)[2])


model <- lm(age ~ circumference, data = Orange)
summary(model)

ggplot(data = Orange, aes(x = circumference, y = age)) + geom_point()  +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model on points")
#We reject the null hypothesis, there is no relationship between age and circumference


Toothgrowth <- datasets::ToothGrowth

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

Toothgrowth %>% 
  group_by(supp) %>% 
  summarise(supp_var = two_assum(len)[1],
            supp_norm = two_assum(len)[2])
#H0: There's a linear relationship between the dose and the supp
TGdose = factor(ToothGrowth$dose, levels=c(0.5,1.0,2.0), 
                          labels=c("low","med","high"))
AOV <- aov(len ~ supp * dose, data=Toothgrowth)
summary(AOV)
#We accept the hypothesis because p is less than 0.05


warpbreaks <- datasets::warpbreaks

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

warpbreaks %>% 
  group_by(wool) %>% 
  summarise(wool_var = two_assum(breaks)[1],
            wool_norm = two_assum(breaks)[2])
#Hypothesis: Wool A is stronger than wool B
ggplot(data = warpbreaks, aes(x = tension, y = breaks, fill = wool)) +
  geom_boxplot()

yho <- lm(breaks ~ wool*tension, data = warpbreaks)
anova(yho)

#We reject the hypothesis that wool A is better than B, both wools have same...
#... strength since p is greaer than 0.05

#QUESTION 2

SAC <- read_csv("SACTN_data.csv")

summary(SAC$src)
str(SAC)
grp <- SACTN_data %>%
  mutate(yr = year(date),
         mon = month(date)) %>%
  group_by(site, src, mon) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>%
  ungroup()

ggplot(grp, aes(x = mon, y = mean_temp)) +
  geom_line(aes(group = site), color = "red") +
  facet_wrap(~site, nrow = 3) +
  labs(x = "Year", y = "Temperature (degrees celcius)",
       title = "Monthly mean temperature")
