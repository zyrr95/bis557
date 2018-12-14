#' @description create sparse matrix using sparse.matrix, operations defined including add, multiply and transpose
#' @param i row index 
#' @param j col index 
#' @param x value of the (i,j) entry
#' @param dims sparse matrix dimension
#' @return A sparse.matrix object
#' @export

# define class
sparse.matrix = function(i, j, x, dims = c(max(i), max(j))){
  structure(list(data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1)), dims), class = "sparse.matrix")
}



#' @description create sparse matrix addition method.
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return A sparse.matrix object a + b
#' @export

# from the question we have add:

`+.sparse.matrix` = function(a, b){
# making sure the format is correct
    if (!identical(a[[2]],b[[2]]))
    stop("non-conformable arrays")
  if (!inherits(a, "sparse.matrix"))
    stop ("a is not a sparse.matrix object")
  if (!inherits(b, "sparse.matrix"))
   stop ("b is not a sparse.matrix object")
  c = merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] = 0
  c$x2[is.na(c$x2)] = 0
  c$x = c$x1 + c$x2
  c[, c("i", "j", "x")]
  c = sparse.matrix(c$i, c$j, c$x, dims = a[[2]])
  return(c)
}

#Before defining multiply and transpose, both methods are not s3 objects, so that we need to overload them. 
#reference : http://adv-r.had.co.nz/S3.html

# %*% is not S3 object
`%*%.default` = .Primitive("%*%")  # keep defalut operation

`%*%` = function(x, y) {
  UseMethod("%*%", x)
}


t = function (x, ...) {
  UseMethod("t", x)
}

#' @description create sparse matrix multiply method.
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return A sparse.matrix object a %*% b
#' @export


`%*%.sparse.matrix` = function(a, b){
  
  if (!identical(a[[2]][2],b[[2]][1]))
    stop("non-conformable arguments")
  if (!inherits(a, "sparse.matrix"))
    stop ("a is not a sparse.matrix object")
  if (!inherits(b, "sparse.matrix"))
   stop ("b is not a sparse.matrix object")
  
#from CASL p294 to 295 casl_sparse_multiply we have
  
  colnames(b[[1]]) = c("i2", "j2", "x2")
  c = merge(a[[1]], b[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x = c$x * c$x2
  c$key = paste(c$i, c$j, sep = "-")
  x = tapply(c$x, c$key, sum)
  key = strsplit(names(x), "-")
  d = data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  d = sparse.matrix(c$i, c$j, c$x, dims = c(a[[2]][1], b[[2]][2]))
  return(d)
}

#' @description create sparse matrix transpose method.
#' @param a a sparse matrix
#' @return A transposed sparse.matrix object t(x)
#' @export
`t.sparse.matrix` = function(a){
  temp = a[[1]]$i
  a[[1]]$i = a[[1]]$j
  a[[1]]$j = temp
  a[[2]] = rev(a[[2]])
  return(a)
}
