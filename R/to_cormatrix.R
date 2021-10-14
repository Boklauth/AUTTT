#' Convert a Correlation Vector to a Correlation Matrix
#'
#' The function to_cormatrix converts a correlation vector to a correlation matrix
#' 
#' @param cor_vec This is a correlation matrix that multiple pair-wise correlations in the arrangement of pairs i and j, 
#' where i = 1,...,n_dim - 1 and j = 2,...n_dim and n_dim is the number of dimensions. 
#' @param n_dim The number of dimensions or columns. 
#' @return to_cormatrix will return a n_dim x n_dim correlation matrix with 1 in the diagonal cells. 
#'         
#' @export
#' @examples
#' library(AUTTT)
#' to_cormatrix(cor_vec = c(0.3, 0.4, 0.5, 0.6, 0.42, 0.52), 
#' n_dim = 4)
#' 
to_cormatrix <- function(cor_vec, n_dim){
  
  bivar_cor <- n_dim*(n_dim -1)/2
  if (length(cor_vec) < bivar_cor){
    stop(paste0("You need ", bivar_cor, " correlations for the correlation vector, cor_vec"))
  } else if (length(cor_vec) > bivar_cor){
    stop(paste0("There are more correlation values than needed."))
  } 
  
  # Arrange factor correlation values into correlation matrix
  f_cor_cell <- n_dim*n_dim
  f_cor_matrix <- matrix(rep(1, f_cor_cell), 
                        nrow=n_dim, 
                        ncol=n_dim) 
  
  id <- 1
  for (i in 1:(n_dim-1)){
    for (j in (i+1):n_dim){
      f_cor_matrix[i,j] <- cor_vec[id]
      f_cor_matrix[j,i] <- cor_vec[id]
      id <- id+1
    }
  }
  f_cor_matrix
  return(f_cor_matrix)
} # end function




