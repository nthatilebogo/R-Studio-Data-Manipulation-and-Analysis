#################
# Name: Onthatile Lebogo
# Student number: u20589507
#################

# Data
download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")

# Question 1
Spain <- subset(atheism, year == "2012" & nationality == "Spain")
n <- nrow(Spain)

Q1a <- sum(Spain$response == "atheist")/n

std <- sqrt(Q1a * (1 - Q1a)/n)
z1 <- qnorm(0.975)

Q1b <- z1 * std

Q1c_lower <- Q1a-Q1b
Q1d_upper <- Q1a+Q1b  


# Question 2
# Reject null hypothesis because p-value is less than level of significance.
h <- 0.12
z2 <- qnorm(0.95)
std2 <- sqrt(h * (1 - h)/n)

Q2a <- z2 * std2

Q2b <- (Q1a-h)/std2


Q2c <- 2 * pnorm(Q2b, lower.tail = TRUE)



# Question 3
us_05 <- subset(atheism, year == "2005" & nationality == "United States")
us_12 <- subset(atheism, year == "2012" & nationality == "United States")

n_05 <- nrow(us_05)
n_12 <- nrow(us_12)
pe_05 <- sum(us_05$response == "atheist")/n_05
pe_12 <- sum(us_12$response == "atheist")/n_12

Q3a <- pe_05 - pe_12

std3 <- sqrt((pe_05 * (1 - pe_05)/n_05)+(pe_12 * (1 - pe_12)/n_12))
z3 <- qnorm(0.995)
  
Q3b <- z3 * std3

Q3c_lower <- Q3a - Q3b

Q3d_upper <- Q3a + Q3b

# Question 4
# Reject null hypothesis because p-value is less than level of significance.

suc_05 <- sum(us_05$response == "atheist")
suc_12 <- sum(us_12$response == "atheist")
pool <- (suc_05 + suc_12)/(n_05 + n_12)
stder <- sqrt((pool * (1 - pool)/n_05)+(pool * (1 - pool)/n_12))

Q4a <- (Q3a - 0)/(stder)

Q4b <- 2 * pnorm(Q4a, lower.tail = TRUE)

# Question 5
library(statsr)
inference(us_12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")
Q5_pe <- 0.09

Q5_z <- qnorm(0.975)

Q5_mo <- Q5_z * 0.0069

Q5_lower <- 0.0364 
Q5_upper <- 0.0634 




