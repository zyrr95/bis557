#' Fit a Ridge Regression model
#'
#' @description This function passes parameters to the ridge regression function.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a penalty parameter
#' @return An ridge_reg object
#' @importFrom stats model.matrix
#' @examples
#' fit <- ridge_reg(Sepal.Length ~.,1, iris)
#' summary(fit)
#' @export

ridge_reg = function(form, lambda, data){
    rownames(data) = NULL
    m = model.matrix(form, data)
    y = matrix(data[,as.character(form)[2]], ncol=1)
    y = y[as.numeric(rownames(m)),, drop = FALSE]
    
    svd_obj = svd(m)
    U = svd_obj$u
    V = svd_obj$v
    svals = svd_obj$d
    
    D = diag(svals / (svals^2 + lambda))
    beta = V %*% D %*% t(U) %*% y
    rownames(beta) = colnames(m)
    ret = list(coefficients = beta, lambda = lambda, form=form)
    class(ret) = "ridge_reg"
    ret
}


