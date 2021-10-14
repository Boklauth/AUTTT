#' Convert a Correlation Vector to a Correlation Matrix
#'
#' The function to_loadingmatrix converts a vector of factor loadings
#' to a p x m factor loading matrix in a confirmatory factor analysis 
#' model. This creates a loading matrix with no cross-factor loadings. 
#' 
#' @param loading_vec This is a vector of factor loadings for each item listed 
#' in order of item1, item2,..., itemp. 
#' @param model is a list object specifying items in each factor in order.
#' See the example. 
#' @return to_loadingmatrix will return a p x m matrix of factor loadings
#' according to the confirmatory factor analysis (CFA)  model provided, 
#' where p is the total number of items, and m 
#' is the total number of dimensions. 
#'         
#' @export
#' @examples
#' library(AUTTT)
#' 
#' lambs <- runif(12, 7, 9.5)
#' mymodel <- list(c(1,2,3,4), c(5,6,7), c(8, 9,10, 11, 12))
#' to_loadingmatrix(loading_vec = lambs, model = mymodel)

to_loadingmatrix <- function(loading_vec, model){
  # declare variables
  param <- lambs
  nvar <- length(param)
  df <- matrix(rep(0, nvar), 
               nrow = nvar, ncol = length(model))
  
  if (is.null(model)){
    stop("To turn a vector of factor loadings into a p x m matrix, a model is needed.")
    } else {
  for (f in 1:length(model)) {
    start_n <-   min(unlist(model[f]))
    end_n <- max(unlist(model[f]))
    for (j in start_n : end_n) 
      df[j,f] <- param[j]
      }
    }
  
  LAMB <- df
  return(LAMB)
}