## ------------------------------------------------------------------------
library(bis557)
data(ridge_train)
data(ridge_test)

## ------------------------------------------------------------------------
predict.ridge_reg = function(object,...) {
  newdata = list(...)[[1]]
  m = model.matrix(object$form, newdata)
  m %*% object$coefficients
}

## ------------------------------------------------------------------------
lambdas = seq(0.1, 100, by=0.1)
MSEs = rep(NA, length(lambdas))
for (i in 1:length(lambdas)){
  fit = ridge_reg(y ~., lambdas[i], ridge_train)
  error = ridge_test$y - predict(fit, ridge_test)
  MSEs[i] = mean(error^2)
}
lambdas[which.min(MSEs)]

