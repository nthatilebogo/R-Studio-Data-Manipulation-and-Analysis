##################
## Name: Onthatile
## Student number: u20589507
##################

# Data

download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

# Question 1
# Strong positive linear relationship
plot(mlb11$hits,mlb11$runs,
     xlab = "Number of hits",
     ylab = "Number of runs",
     main = "Scatterplot of runs vs hits",
     pch = 19)

# Question 2
m <- lm(mlb11$runs ~ mlb11$hits, data = mlb11)
summary(m)
runs_hits <- coef(m)
abline(m, col = "blue")


# Question 3
m1 <- lm(mlb11$runs ~ mlb11$hits, data = mlb11)
R2_hits <-  summary(m1)$r.squared

m2 <- lm(mlb11$runs ~ mlb11$at_bats, data = mlb11)
R2_bats <-  summary(m2)$r.squared

# Question 4
m_home <- lm(mlb11$runs ~ mlb11$homeruns, data = mlb11)
R2_home <-  summary(m_home)$r.squared

m_bat <- lm(mlb11$runs ~ mlb11$bat_avg, data = mlb11)
R2_bat <-  summary(m_bat)$r.squared

m_strike <- lm(mlb11$runs ~ mlb11$strikeouts, data = mlb11)
R2_strike <-  summary(m_strike)$r.squared

m_stole <- lm(mlb11$runs ~ mlb11$stolen_bases, data = mlb11)
R2_stole <-  summary(m_stole)$r.squared

m_w <- lm(mlb11$runs ~ mlb11$wins, data = mlb11)
R2_w <-  summary(m_w)$r.squared

R2 <- max(R2_bat,R2_bats,R2_hits,R2_home,R2_stole,R2_strike,R2_w)

plot(mlb11$bat_avg,mlb11$runs,
     xlab = "Number of hits",
     ylab = "Number of runs",
     main = "Scatterplot of runs vs hits",
     pch = 19)
abline(m_bat, col = "blue")
coef(m_bat)

# Question 5
m_new <- lm(mlb11$runs ~ mlb11$new_onbase, data = mlb11)
residuals <- summary(m_new)$residuals

# Question 6
#sum(residuals)
plot(mlb11$runs,residuals,
     xlab = "no-base percentage",
     ylab = "Residuals",
     main = "Residual plot of the model having the \n no-base percentage",
     pch = 19)
abline(h = 0, lty = 3, col = 'red')

