#Author: Jabulile Leroko
#Day2 homework

snakes <- read_csv("snakes.csv") #Read in the data snakes.csv
snakes$day = as.factor(snakes$day) #make the column day a factor because ANOVA...
#... works with factors only and day is a continuous variable

snakes.summary <- snakes %>% 
  group_by(day, snake) %>% #Group by the day and snake columns
  summarise(mean_openings = mean(openings), #calc mean and sd of openings and...
            #give them new columns
            sd_openings = sd(openings)) %>% 
  ungroup() #Reverse the grouping
snakes.summary
#The problem is we grouped by day and snake, these two columns are independent

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, 
                             measurevar = "openings", groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, 
               aes(x = day, xend = day, y = openings - ci, 
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  #geomsegment draws those solid lines middle of the boxes
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05) +#jitter adds those black dots. it adds random... 
  #variation to the location of each point
  labs(x = "Day", y = "Number of openings") +
  theme_classic()

#Perfom ANOVA
snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

par(mfrow = c(2, 2)) #Telling r to plot graphs 1 'board', 2 rows, 2 columns
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")
#The residuals look more or less normal

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")

#TukeyHSD finds means of a factor that are signif different from each other
#These means are not signif different, are similar

