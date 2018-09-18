
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model = function(formula,data){
  res = all.vars(formula)[1]
  if( all.vars(formula)[-1] == '.'){
    var = colnames(data)[colnames(data)!= res]
  }
  else{
    var = all.vars(formula)[-1]
  }
  X = model.matrix(as.formula(paste("~", paste(var, collapse = " + "))), data = data)
  Y = data[,res]
  #beta = solve((t(X) %*% X)) %*% t(X) %*% Y
  beta = qr.solve(X,Y)
  beta[beta == 0] = NA
  fitted = X %*% beta
  residuals = Y - fitted
  result = list(coefficients = beta, residuals = residuals, fitted.values = fitted, rank = ncol(X), weights = NULL, df.residual = nrow(X) - ncol(X), call = call('lm',formula), terms = terms(x = formula, data = data),contrasts = NA, xlelves = NA, offset = NA, y = Y, x = X, model = formula, na.action = NA, qr = qr(X))
  class(result) = 'lm'
  #browser()
  return(result)
}
