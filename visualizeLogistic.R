library(manipulate)
library(glmnet)
library(ggplot2)

sampLogReg <- function() {
  download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda",
                destfile="ravensData.rda", method = "curl")
  load("ravensData.rda")
  
  # generalized logistic regression
  logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore, family="binomial")
  summary(logRegRavens)
  
  plot(ravensData$ravenScore, logRegRavens$fitted, pch=19, col="blue", xlab="score", ylab="Prob Ravens Win")
  
  exp(logRegRavens$coeff)
  
  exp(confint(logRagRavens))
}

visLogistic <- function() {
  x <- seq(-10, 10, length = 1000)
  manipulate(
    plot(x, exp(b0 + b1 * x) / (1+exp(b0 + b1*x)),
         type = "l", lwd = 3, frame = FALSE),
    b1 = slider(-2, 2, step=0.1, initial = 2),
    b0 = slider(-2, 2, step=0.1, initial = 0)
  )
}