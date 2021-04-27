#Jabulile Leroko
#DAY 4

#Confidence intervals... what is the range of the 
#mean values that you can arrive at if you were to repeat the experiment a 100 times?

#Bootstrapping
#measures the variability and distribution of the mean by repeating the experiment
#and measures how many times the mean falls out of the 95% range

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)
#
P <- groupwiseMean(Steps ~ Sex,data = data, conf = 0.95, digits = 3)
  library(tidyverse)
#
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3, traditional = FALSE)
#
str(data)

library(tidyverse)
ggplot(data = P) + 
  geom_col(aes(x = Sex, y = Mean, fill = "red", col = "black")) +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper,
                    x = Sex), 
                col = "black",
                width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")
#Even though it might look like females are less lazy from the columns,
#the error bars are overlapping which means they are equally lazy.

Z = groupwiseMean(Steps ~ Teacher + Sex,data = data, conf = 0.95, digits = 3)

ggplot(data = Z) + 
  geom_col(aes(x = Sex, y = Mean, fill = "red", col = "black")) +
  geom_errorbar(aes(ymin = Trad.lower, ymax = Trad.upper,
                    x = Sex), 
                col = "black",
                width = 0.2) +
  facet_wrap(~Teacher, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Sex") + ylab("Steps")

#The difference in teachers has no effect on the peformance of the students, 
#... because all these error bars overlap

#What tests can we do to test the three hypotheses that
#1. No difference in peformance between males and females
#2. No relationship between sex and teacher
#3. No difference in performances of students from different teachers

#We can use anova tests
anova <- aov(Steps~Sex*Teacher, data = data)
summary(anova)

#We can do Tukey to compare every combination of sex and teacher
anova_Tukey = TukeyHSD(anova)
plot(anova_Tukey)

#My practice
#Use dlply to separate the data into a list of the columns grouped under teacher
dlply(data, "Teacher")

#Have the column separated into female and male sex
dlply(data, "Sex")
#Let's try something else
data %>% select(Sex, Teacher, Rating) %>% #I only want these columns 
  filter(Teacher == "Sadam")#Only data for the teacher Sadam


Females <- data %>% select(-Student, -Steps) %>%
  filter(Sex == "female")
Males <- data %>% select(-Student, -Steps) %>%
  filter(Sex == "male")

a <- ggplot(Females, aes(x = Teacher, y = Rating)) +
  geom_col(fill = "darkgoldenrod") + labs(x = "Teacher", y = "Female ratings") +
  theme_dark()
b <- ggplot(Males, aes(x = Teacher, y = Rating)) +
  geom_col(fill = "chocolate1") + labs(x = "Teacher", y = "Male ratings") +
  theme_dark()
ggarrange(a, b, ncol = 2, labels = "AUTO")


