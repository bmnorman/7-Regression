testFit <- function () {
  data(swiss)
  par(mfrow = c(2, 2))
  fit <- lm(Fertility ~ ., data = swiss)
  
  plot(fit)
  #plot(predict(fit), resid(fit), pch = ".")
}