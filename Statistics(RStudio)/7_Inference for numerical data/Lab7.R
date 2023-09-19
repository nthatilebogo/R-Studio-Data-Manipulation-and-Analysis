# Name: Onthatile

# Student Number: u20589507

# Data

download.file("http://www.openintro.org/stat/data/nc.RData", destfile = "nc.RData")
load("nc.RData")

# Question 1
y_m <- subset(nc, mature == "younger mom")
m_m <- subset(nc, mature == "mature mom")

Q1a <- max(y_m$mage, na.rm = TRUE)
Q1b <- min(m_m$mage, na.rm = TRUE)


# Question 2
# Negatively skewed
library(statsr)
inference(nc$weeks, est = "mean", 
          type = "ci", 
          alternative = "twosided",
          method = "theoretical")
hist(nc$weeks,
     main = "Histogram of length of pregnancy",
     xlab = "Weeks",
     ylab = "Frequency")

# Question 3
#90% is narrower
inference(nc$weeks, est = "mean", type = "ci", method = "theoretical", conflevel = 0.90)


# Question 4
#No impact. 
inference(y = nc$gained, x = nc$mature, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical", 
          order = c("younger mom","mature mom"))
boxplot(nc$gained ~ nc$mature,
        main = "Boxplot of weight gained",
        xlab = "Maturity",
        ylab = "Weight gained")

# Question 5
#
inference(y = nc$weeks, x = nc$marital, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical", 
          order = c("married","not married"))

p_e <- 38.0803 - 38.4951
up <- p_e + (qnorm(0.975) * 0.203)
low <- p_e - (qnorm(0.975) * 0.203)

