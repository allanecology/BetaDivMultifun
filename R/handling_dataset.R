#' show head in two directions
#'
#' To show the first 10 lines of a table, \code{head()} can be used. To show
#' the first 10 columns as well, \code{head(x[,1:10])} can be used. The 
#' "\code{hedo()}" function 
#' shortens this command to \code{hedo(x,10)}. The number of 
#' rows/columns shown can be changed to any value.
#'
#' @param x Input data frame or matrix.
#' @param n Show the first n rows and columns, defaults to 10.
#' @return The first n rows and columns of a data frame or matrix.
#' @examples
#' m <- matrix(sample(seq(0,100),225, replace=T), 15, 150)
#' m
#' head(m)
#' hedo(m)
#' @export
hedo <- function(x, n=10) {
  head(x[,1:n])
}
NULL


  