#' Convert a Correlation Matrix to Covariance Matrix
#'
#' Given a correlation matrix and correct number of standard deviations, tocovmatrix() converts the correlation matrix
#' to a covariance matrix. If you only have a vector correlation, you can convert it to a correlation matrix first, using 
#' the function to_cormatrix(). 
#' 
#' @param cor_matrix A a symmetric correlation matrix.
#' @param sd_vec A vector of the variables' standard deviations. 
#'         
#' @export
#' @examples
#' mycormat <- to_cormatrix(cor_vec = c(0.3, 0.5, 0.6), n_dim=3)
#' to_covmatrix(mycormat, sd_vec = c(0.95, 0.98, 1.1))

to_covmatrix <- function(cor_matrix, sd_vec){
  sd_matrix <- diag(sd_vec)
  cov_matrix <- sd_matrix %*% cor_matrix %*% sd_matrix
  return(cov_matrix)
}


