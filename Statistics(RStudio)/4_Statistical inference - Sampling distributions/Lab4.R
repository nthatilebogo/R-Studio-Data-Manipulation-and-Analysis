#########################################################
# Name & Surname:Onthatile Lebogo                       #
# Student Number:u20589507                              #
#########################################################

# data

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

area <- ames$Gr.Liv.Area
price <- ames$SalePrice

# load the necessary packages



# Question 1

set.seed(6)
Sample_Price <- sample(price,50)


# Question 2
Estimate <- mean(Sample_Price,na.rm = TRUE)


# Question 3

set.seed(20)
Sample_Means50 <- rep(NA,5000)

for (i in 1:5000) {
  samp <- sample(price,50)
  Sample_Means50[i] <- mean(samp)
  
}



# Question 4
Mean_50 <- mean(Sample_Means50)


# Question 5
# Skewed to the right,unimodal
hist(Sample_Means50,
     main = "Histogram of Sale Price",
     ylab = "Frequency",
     xlab = "Sale Price($)",
     breaks = 32,
     plot = TRUE,
     xlim = c(140000,250000),
     ylim = c(0,400))


# Question 6
#p-value < 0.05, null hypothesis is rejected.  

SH_p = shapiro.test(Sample_Means50)$p.value1.084e-10

# Question 7

set.seed(25)
Sample_Means150 <- rep(NA,5000)

for (i in 1:5000) {
  samp <- sample(price,150)
  Sample_Means150[i] <- mean(samp)
  
}


# Question 8
# Symmetrically skewed,unimodal distribution
hist(Sample_Means150,
     main = "Histogram of Sale Price",
     ylab = "Frequency",
     xlab = "Sale Price($)",
     breaks = 32,
     plot = TRUE,
     xlim = c(150000,210000),
     ylim = c(0,700))



# Question 9
#p-value > 0.05, null hypothesis is not rejected.  

AD_p <- ad.test(Sample_Means150)$p.value

# Question 10
#The sample mean of sales price with a sample size of 150 is the distribution that has a smaller spread.

sd(Sample_Means50)
sd(Sample_Means150)