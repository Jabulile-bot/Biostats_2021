#Jabulile Leroko

library(tidyverse)
library(plotly)
library(tidyr)
library(dplyr)

#At some point, we'll have to compare tests
#Is group A different from B
#If p-value is <0.05 it means there's significant difference,
#If it's >o.o5, there's no signif difference

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram... for histogram, we only specify x-axis
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value") + theme_classic()
h
#We can see that both samples are normally distributed
#Now we need to statistically prove it is normal, use shapiro 
shapiro.test(r_dat$dat)

#The value is <0.05, which means it's saying it's not normal... 
#But the graph said they are
#Problem? We need to check for each sample, shapiro combined the samples
# We need to group the two samples, and apply shapiro to each sample

r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#Testing homoscedasticity... tests homogeneousity, similarity
#rule, var for 1 group must not be 4x > than the other

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))
#var for A is not more than 4x > than that of B... so, they're similar



two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}
#The function does normality and shapiro in one

r_dat %>% 
  group_by(sample) %>% #Group_by sample because we have two sampes
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])
#For t-tests, your p value needs to be <0.05 for it to be signif
#But for shapiro, >0.05 means significant

#Most common... Two-sample t-tests... comparing differences between samples

# random normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#well... my pvalue was > than 0.05, so the difference in means is not signific

ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() + theme_classic()

# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
#Check for normality and homoscedasticity
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])
#Samples are normal and similar
# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
#p-value is 0.03657 which is less than 0.05, which means batsata has stipe...
#mass greater than boulders beach

#Let me test stipe length
ecklonia_sub2 <- ecklonia %>% 
  filter(variable == "stipe_length")

ggplot(data = ecklonia_sub2, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#H0: The two sites have same stipe length
#Check for normality and homoscedasticity
ecklonia_sub2 %>% 
  group_by(site) %>% 
  summarise(stipe_length_var = two_assum(value)[1],
            stipe_length_norm = two_assum(value)[2])
#The samples are homogenous, but Batsasa rock is not normally distributed

#Wilcox test
wilcox.test(value ~ site, data = filter(ecklonia_sub2, 
                                        variable == "stipe_length"))
#p< 0.001752, significant to reject the null hypothesis
#The sites are not equal in stipe length

library(ggpubr)


#ANOVA!!!!
#the assumptions for t-tests need to apply as well
#If your data isn't normal, you can transform the data and make it normal
# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #We are specifically filter diet 1 and 2
#... hence %in% c(1,2) and at day 21
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")
#does diet influence the weight of chicken? No because p-value>than 0.05

#Single factor anova..
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

TukeyHSD(chicks.aov1)
#Multiple anova

chicks.aov2 <- aov(weight ~ as.factor(Time), 
                   data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)

#If your data is not normal, you can transform the data... but also, there...
#are tests that allow data that is not normal

#Wilcox test
compare_means(weight ~ Diet,
              data = filter(chicks, Time == 0, Diet %in% c(1, 2)), 
              method = "wilcox.test")
