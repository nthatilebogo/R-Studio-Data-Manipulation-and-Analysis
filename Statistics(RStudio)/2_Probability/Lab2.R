# Name: Onthatile 
# Student Number: u20589507

# Data
download.file("http://www.openintro.org/stat/data/kobe.RData", destfile = "kobe.RData")
load("kobe.RData")

# Question 1
set.seed(13)
outcomes <- c("H","M")
Sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55)) 
Sim_basket


# Question 2
Streak <- calc_streak(Sim_basket)
Streak

# Plot
barplot(table(Streak))


# Interpretation
# Positively Skewed


# Question 3
Max_streak <- max(Streak)


# Question 4
Typical_streak <- median(Streak)


# Question 5
# Interpretation
# It does not affect the distribution. It is still positively skewed 



# Question 6
# Plot
barplot(table(Streak),
        border = "black", 
        col = c("Green"))
trans <- rgb(0,0,0, max= 255,alpha = 1)
K <- calc_streak(kobe$basket)
barplot(table(K),
        border = "Red",
        col = c(trans),
        add = TRUE)

#Interpretation
#Kobe data set is more skewed to the right than the simulated streak lengths.







