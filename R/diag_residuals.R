#' Compute Item Residuals from a Factor Loading Vector or Matrix
#'
#' The function diag_residuals will compute item residuals from the standardized 
#' factor loadings. 
#' 
#' @param x This is a vector of factor loadings of a p x m matrix of factor 
#' loadings, where p is the total number of items and m is the total number of 
#' dimensions.
#' @return diag_residuals will return a p x p diagonal matrix of residuals, 
#' in which the off diagonal cells are zero, and p is the total number of items. 
#'         
#' @export
#' @examples
#' library(AUTTT)
#' # Example 1 
#' lambs <- runif(12, .7, .95)
#' x <- lambs
#' diag_residuals(x = lambs)
#' 
#' # Example 2
#' lambs <- runif(12, .7, .95)
#' mymodel <- list(c(1,2,3,4), c(5,6,7), c(8, 9,10, 11, 12))
#' to_loadingmatrix(loading_vec = lambs, model = mymodel)
#' x <- lambs
#' diag_residuals(x = lambs)

diag_residuals <- function(x){
x <- as.matrix(x)
  H2 <- apply(x^2, MARGIN = 1, FUN = sum) # 1 indicate rows, aka rowSum()
  epsilon <- 1 - H2 # uniqueness
  epsilon_matrix <- diag(epsilon)
  return(epsilon_matrix)
}


