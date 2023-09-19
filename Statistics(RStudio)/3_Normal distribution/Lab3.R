# Name: Onthatile
# Student Number: u20589507

# Data

download.file("http://www.openintro.org/stat/data/bdims.RData", destfile = "bdims.RData")
load("bdims.RData")
fdims <- subset(bdims, sex == 0)
mdims <- subset(bdims, sex == 1)

# Question 1
# Create variable
m <- mean(fdims$bii.di)
std <- sd(fdims$bii.di)
Standardized_Pelvic <- (fdims$bii.di-m)/std

# Plots
hist(Standardized_Pelvic,
     main = "Histogram of Standardized Female Pelvic Diameter",
     ylab = "Frequency",
     xlab = "Diameter(cm)",
     plot = TRUE,
     xlim = c(-4,4),
     ylim = c(0,60))

qqnorm(Standardized_Pelvic,
       main = "Probability Plot of Female Pelvic Diameter",
       ylab = "Standardized Pelvic Diameter",
       xlab = "Pelvic Diameter")
qqline(Standardized_Pelvic)


# Question 2
# Create variable
m_elb <- mean(fdims$elb.di)
std_elb <- sd(fdims$elb.di)
Standardized_Elbow <- (fdims$elb.di-m_elb)/std_elb


# Plots
hist(Standardized_Elbow,
     main = "Histogram of Standardized Female Elbow Diameter",
     ylab = "Frequency",
     xlab = "Diameter(cm)",
     plot = TRUE,
     xlim = c(-4,4),
     ylim = c(0,60))

qqnorm(Standardized_Elbow,
       main = "Probability Plot of Female Elbow Diameter",
       ylab = "Standardized Elbow Diameter",
       xlab = "Elbow Diameter")
qqline(Standardized_Elbow)

# Question 3
# Create variable
m_age <- mean(fdims$age)
std_age <- sd(fdims$age)
Standardized_Age <- (fdims$age-m_age)/std_age


# Plots
hist(Standardized_Age,
     main = "Histogram of Standardized Female Age",
     ylab = "Frequency",
     xlab = "Age(Years)",
     plot = TRUE,
     xlim = c(-2,5),
     ylim = c(0,60))

qqnorm(Standardized_Age,
       main = "Probability Plot of Age",
       ylab = "Standardized Age",
       xlab = "Age")
qqline(Standardized_Age)


# Question 4
# Create variable
m_ch <- mean(fdims$che.de)
std_ch <- sd(fdims$che.de)
Standardized_Chest <- (fdims$che.de-m_ch)/std_ch


# Plots
hist(Standardized_Chest,
     main = "Histogram of Standardized Female Chest Depth",
     ylab = "Frequency",
     xlab = "Depth(cm)",
     plot = TRUE,
     xlim = c(-5,-1),
     ylim = c(0,50))

qqnorm(Standardized_Chest,
       main = "Probability Plot of Female Chest Depth",
       ylab = "Standardized Chest Depth",
       xlab = "Chest Depth")
qqline(Standardized_Chest)



# Question 5
#The distribution is positively skewed.
qqnorm(bdims$kne.di,
       main = "Normal Probability Plot of Female Knee Diameter",
       ylab = "Knee Diameter",
       xlab = "Scores")
qqline(bdims$kne.di)

hist(bdims$kne.di,
     main = "Histogram of  Female Knee Diameter",
     ylab = "Frequency",
     xlab = "Diameter(cm)",
     plot = TRUE,
     xlim = c(14,26),
     ylim = c(0,200))



# Question 6
mFem <- mean(fdims$hgt)
sdFem <- sd(fdims$hgt)
Theo_Female <- pnorm(q = 170, mean = mFem, sd = sdFem)


# Question 7
mMale <- mean(mdims$hgt)
sdMale <- sd(mdims$hgt)
Theo_Male <- pnorm(q = 180, mean = mMale, sd = sdMale) - pnorm(q = 170, mean = mMale, sd = sdMale)



# Question 8
Emp_Female <- sum(fdims$hgt < 170 ) / length(fdims$hgt)


# Question 9
Emp_Male <- (sum(mdims$hgt < 180) - sum(mdims$hgt <= 170)) / length(mdims$hgt)  


# Question 10
# NB! Do not remove the statement below. Create your plot below the following statement.
library(openintro)

histPlot(mdims$wgt,
     main = "Histogram of Male & Female Weight",
     ylab = "Frequency",
     xlab = "Weight(kg)",
     border = "Red",
     hollow = TRUE,
     plot = TRUE,
     xlim = c(40,120),
     ylim = c(0,80))

histPlot(fdims$wgt,
     add = TRUE,
     border = "Blue",
     hollow = TRUE)
legend("topright", legend = c("Male","Female"), col = c("Red", "Blue"), pch = 15)
