# Name:
#Onthatile

# Student Number:
#u20589507

# Data

source("http://www.openintro.org/stat/data/cdc.R")


# Question 1 - Plot
plot(x = cdc$weight, y = cdc$wtdesire)


# Question 2
Weight_Diff <- cdc$weight - cdc$wtdesire
Weight_Diff


# Question 3


# Question 4
#plot(Weight_Diff)
#add labels
boxplot(Weight_Diff)

# Question 5
Gend_sum = tapply(Weight_Diff,cdc$gender,summary)
boxplot(Gend_sum)

# Male Boxplot
boxplot(Gend_sum$m)

# Female Boxplot
boxplot(Gend_sum$f)


# Question 6
Mean <- mean(Weight_Diff)
Mean


# Question 7
Standard <- sd(Weight_Diff)
Standard


# Question 8
#Proportion <- (17531-1124)/20000
mean_weight <- mean(cdc$weight)
std_weight <- sd(cdc$weight)
Proportion <- mean((cdc$weight <= (mean_weight + std_weight)) & (cdc$weight >= (mean_weight - std_weight)))
Proportion