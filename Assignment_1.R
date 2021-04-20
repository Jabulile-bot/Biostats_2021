#Name: Jabulile Leroko
#Assignment 1

#SECTION 1
bod <- BOD
?BOD

#C is true. A, B, and D are false.


# SECTION 2 ---------------------------------------------------------------

#Loading dplyr
library(dplyr)
#Loading dslabs
library(dslabs)

mur <- murders
?murders
str(mur)
head(mur)
summary(mur)
#The dataset contains total gun murders from 51 states of US. It contains...
#5 columns: 1. state, a character, containing all states of US
#2. abb, a character, showing the abbreviations of the states.
#3. region, a factor, showing the regions from which the states are in, which...
#... can be Northeast(9), South(17), North Central(12), and West(13)
#4. population, a continuous (num) type of data, showing the population size...
#... of each state, ranging from 563,626 to 37,253,956
#5. total, continuous data, showing number of of gun murders of each state...
#... ranging from 2 to 1,257 murders

#Select state and population only
mur %>% select(state, population)

#Remove florida
mur[mur$state != "Florida", ]

#Remove South and create data frame
no_south <- mur[mur$region != "South", ]
#The new data frame has 34 states, South region had 17... The question wasn't clear

#Calc population of south and west regionally
mur %>% 
  filter(region == "South") %>% 
  summarise(south_pop = sum(population))

mur %>% 
  filter(region == "West") %>% 
  summarise(west_pop = sum(population))

#new data frame with only pop. of NE
NE_pop <- mur %>% 
  filter(region == "Northeast")


library(ggpubr)
# a plot to show relationship between deaths and population

ggplot(data = mur, aes(x = population, y = total)) +
  geom_point(aes(colour = region)) +
  geom_smooth() +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Population", y = "Total gun murders", 
       title = "Gun murders by population in four US states")
#The larger the population, the more the murders by gun

#Boxplot of deaths by region

mur %>%
  ggplot(aes(x = region, y = total)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Murders by gun", x = "Region") + 
  theme_pubr()
#The South region has more gun murders than the other regions, because the median
#... is larger than the others.
#NE, NC and West have same median, they just differ in the differences between
#... states in each region, with West region having the smallest variety between
#...states

mur %>% select(region, population) %>% summarise(s.pop = mean(South))

#The Southern population is almost 2x the western population..

total_data <- mur %>% filter(total > 20) %>% 
  filter(total < 100)

object <- mur %>% 
  slice(10:24, 26)

murders_tibble <- as_tibble(mur)


tib_region <- as_tibble(mur) %>% group_by(region)


# SECTION 3 ---------------------------------------------------------------

data("heights")
summary(heights)
summary(heights$height)
#The data heights has 238 females and 812 males. The height ranges from 50 to 82.68
#... 1st quater being 66, median being 68 and 3rd quarter being 71

heights %>% filter(sex == "Male") %>% 
  summarise(ave_mal = mean(height),
            sd_mal = sd(height),
            min_mal = min(height),
            med_mal = median(height),
            max_mal = max(height))

heights %>% filter(sex == "Female") %>% 
  summarise(ave_fem = mean(height),
            sd_fem = sd(height),
            min_fem = min(height),
            med_fem = median(height),
            max_fem = max(height))


# SECTION 4 ---------------------------------------------------------------

x <- c( 1, 6, 21, 19 , NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA)

summary(x)
#answer= 2 NAs
summary(y)
#answer= 4 NAs



# SECTION 5 ---------------------------------------------------------------
library(tidyverse)
Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            autumn = c(57, 66, 52, 56))
str(Seasonal_data)
New <-pivot_longer(Seasonal_data, col = 2:5,
             names_to = "season", "value")
New
str(New)
#Hypothesis: From 2015 to 2018 seasonal temperatures have increased over the
#... the years
ggplot(data = New, aes(x = year, y = value)) +
  geom_line(aes(colour = season)) +
  labs(x = "Year", y = "", title = "The change in seasonal temperatures over years") +
  theme_classic()


ggplot(New, aes(x=season, y=value, color=year)) + 
  geom_point(size=6) +
  labs(x = "Year", y = "", title = "The change in seasonal temperatures in different seasons") +
  theme_classic()
#The temperatures have decreased over the years, thefore we reject the hypothesis
cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data
#Separate data
cats_data %>%
  separate(col = "position", into = c("first_place", "second_place", "third_place"), sep = "-")

#Unite
cats_data %>% 
  unite(minutes, seconds, col = "total_time", sep = ":")



# SECTION 6 ---------------------------------------------------------------

co <- datasets::CO2

#Joining the column Type and Treatment into one column, TT. In column TT, type...
#and treatment will be separated by the symbol -
United <- co %>% 
  unite(Type, Treatment, col = "TT", sep = "-")

#Separate things under column TT into columns Type and Treatment
Separated <- United %>%
  separate(col = TT, into =c("Type", "Treatment"))

#Add another column by multiplying the conc column by 1000, call the new column...
#ppm
co %>% 
  mutate(ppm = conc * 1000)

#Group the data by the columns plant and Type
grouped <- co %>% 
  group_by(Plant, Type)

#From the co, I only want the columns Treatment, conc and uptake
selected <- co %>% select(Treatment, conc, uptake)

#Arrange co by type such that everything type Quebec comes first
arranged <- co %>% arrange(Type)

#spread the data such that the different conc are now columns, in their rows be...
#the uptake values
spred <- co %>% spread(conc, uptake)

#colapse columns 4 to 10 in spred by putting them in a column...
#called conc, their rows be in the column uptake
gathered <- spred %>% gather("conc", "uptake", 4:10)



# THE END!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ------------------------------


