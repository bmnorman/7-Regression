library(glmnet)
library(ggplot2)
library(caret)

if (!exists(fullData)) {
  fullData <- data(mtcars)
}

# Analyze mpg as a function of automatic vs. manual transmission.
# Description of columns:
# 1 - mpg - miles/US gallon
# 2 - cyl - number of cylinders
# 3 - disp - displacement in cubic inches
# 4 - hp - gross horsepower
# 5 - drat - rear axle ratio
# 6 - wt - weight (1000 lbs)
# 7 - qsec - 1/4 mile time in seconds
# 8 - vs - V/S
# 9 - am - transmission (0=automatic, 1=,manual)
# 10 - gear - number of forward gears
# 11 - carb - number of carburetors

# specify factor (categorical) variables
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

# Use this to print out a brief summary of the columns - str(mtcars)
# Use this to print out a more detailed summary of the data - summary(mtcars)

fit <- lm(mpg ~ cyl + wt + am , mtcars)
par(mfrow = c(2,2)) # 2x2 plot window
plot(fit)

# par(mfrow = c(1,1)) - go back to normal plot window

summarize <- function(type=1) {
  if (type == 1) {
    print(fit$coef)
  }
}