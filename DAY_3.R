#Jabulile Leroko
#Day 3

library(dplyr)
library(tidyr)
library(tidyverse)

data("faithful")
eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

#Graph of the linear regression
slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point(aes(colour = "blue")) +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) + #The annotations
  #... you are telling it the position and what the annotation must be
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


#Correlations
#There are also assumptions to be met.
#1. Pair-wise data, 
#2. No outliers
#3. Linearity
#4. Normality
#5. Homoscedasticity
#6. Type of measurement. If continuous, Pearson corr. if Ordinal, Spearman corr.


library(ggpubr)
library(corrplot)

ecklonia <- read_csv("ecklonia.csv")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID) #select function with - sign excludes species, site and id
#... from the new data frame, if you wanted to include them, you would not put - sign

#Corr compares variables within the same data set

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the defaut settings.
# They are only shown here to illustrate that they exist.
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson") #stipe length and frond length are
#both continuous, so we use Pearson corr.
#The closer the corr value to 1, the stronger the corr.

#The above compared 2 variables... what if i want to compare all the variables?
#BELOw is for the ultimate corr
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#Let us say you have ordinal data... very rare in biology

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)
#Not that much of a correlation

#Kendall rank corr works fr both ordinal and continuous, also for data 
#that is not normally distributed
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

#VISUALS

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2)) #r-print
#stores that thing so that you can paste it there by geom_label
#The round and 2 at then end, are for rounding off to 2 decimal places
# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) + #lm = applying linear 
  #regress line to the plot
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

#Simplest and most common way to plot corr
corrplot(ecklonia_pearson, method = "circle")
#The strong colours are more correlated. The size of circle is also influenced by
#... correlation

#EXERCISE
#Heat map on ggplot2

#Let us do this
ecklonia <- read_csv("ecklonia.csv")
library(reshape2)

melted <- melt(ecklonia_pearson)

ggplot(data = melted, mapping = aes(Var1, Var2, fill = value)) +
  geom_tile() + theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_continuous(low = "violetred", high = "aquamarine")

# END OF HEATMAP ----------------------------------------------------------
library(plyr)
?dlply 

#Basically, it takes an existing list, splits it apart and extracts some info
#from it... still not sure
dlply(snakes, "day")

#This splits the days from the whole data, separate them and now shows me 
#the days alone with their snake and openings readings


