#script for importing cancer data for statistical thinking for public health mooc
#program created on July 24th

g <- read.csv(file = 'Desktop/ICL data annalysis R/cancer data for MOOC 1.csv', header = TRUE, sep=',')

#scenarios where mean and median differ, data is probably skewed

fruit <- g[, 'fruit']
veg <- g[, 'veg']
fruitveg <- fruit + veg

table(fruitveg)

#creat a histogram

hist(fruitveg)

#include exclude so as to get the number of missing values

table(g$smoking, exclude = NULL)

#using ifelse to find out how many people eat 5 or more fruits or vegetables a day
#to find out more about a function use ? e.g. ?ifelse
g$fruitveg <- g$fruit + g$veg

g$fiveaday <- ifelse(g$fruitveg >= 5, 1, 0)


#changing the names of axes and title of a histogram

hist(g$fruitveg, xlab = 'Portions of fruits and vegetables',
     main = 'Daily consumption of fruit and vegetables combined', axes = F)

axis(side =1, at = seq(0, 11, 1))
axis(side = 2, at = seq(0,16, 2))


#installing new packages
install.packages('ggplot2')

require(ggplot2)

#creating a plot with ggplot

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = 'darkgreen', col = 'black')

#going ahead to relabel the plot plus adding a theme 

ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = 'darkgreen')+
  labs(x='Portions of fruit and vegetables', y = 'Frequency') +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_classic()

#dichotomise BMI
g$dichbmi = ifelse(g$bmi >= 18.5 & g$bmi <= 24.9, 1, 0)

#chi-squared test for the proportion of people getting their five a day versus the proportion of people who had cancer
chisq.test(x = g$fiveaday, y = g$cancer)

#running a t-test
t.test(g$bmi~g$cancer, var.equal = TRUE) #in the form t.test(y~x)
#returns alternative hypothesis, tells us that difference is not zero
#but it does not tell us which of the patients (those with or those without cancer) have higher BMI hence a two-tailed test

#compare the proportion who are overweight by cancer
cancer <- g$cancer
ovrwgt <- ifelse(g$bmi >= 25, 1, 0)

chisq.test(x = ovrwgt, y = cancer)


