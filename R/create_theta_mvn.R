#' A Function to Create Variables from a Multivariate Normal Distribution
#'
#' Given a vector means, standard deviation, and interfactor correlation, 
#' create_theta_mvn() creates theta values from a multivariate normal distribution
#' with the number of dimensions equal to the length of the vector of 
#' standard deviation. 
#' 
#' @param size A sample size, which is a positive integer value. 
#' @param mean_vec A vector of means of the variables. 
#' @param sd_vec A vector of standard deviation.
#' @param ifcor_vec This vector contains a pair-wise interfactor correlation values,
#' which are arranged for the pair Factors i and j, where i = 1, 2,...,m-1, 
#' j = 2, 3,...,m, and m is the total number of dimensions. For example, 
#' with three factors, the pair facotrs i and j is arranged as follows: Factor 1 and Factor 2, 
#' Factor 1 and Factor 3, and Factor 2 and Factor 3.
#' @param seed_num A integer value used as a seed number for random number generation.
#' 
#' @return It will return two data sets: a raw data set and a scaled data set. 
#' A scale data set has each dimension standardized. That is each dimension is 
#' mean centered by the dimension's mean and scaled by the dimension's 
#' standard deviation (standardized). 
#' 
#' @export
#' @examples
#' # example: Three-factor model
#'	library(AUTTT)
#'	mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
#'	sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
#'	f_cor <- c(0.3, 0.5, 0.75) # interfactor correlation vector
#'	
#'	output <- create_theta_mvn(size = 300, 
#'	                 mean_vec = mean_vec, 
#'	                 sd_vec = sd_vec, 
#'	                 ifcor_vec = f_cor, 
#'	                 seed_num = 45679)
#' 
#' @seealso [to_cormatrix()],[to_covmatrix()], [TSK()]
 

create_theta_mvn <- function(size, mean_vec, sd_vec, ifcor_vec, seed_num){
  # n of dimensions
  d <- length(sd_vec)
  
  if(length(sd_vec)!=length(mean_vec)){
    stop("The length of mean vector must equal to the length of standard deviation vector.")
  }
  
  # check: n of factor correlations ####
  # iterate pair-wise factor correlations
  nFs <- d
  Fcor.tb <- NULL
  for (i in 1:(nFs-1)){
    for (j in (i+1):nFs){
      Fcor.tb <- rbind(Fcor.tb, c(i, j))
    }
  }
  
  # Check: n of factor correlations given n of factors ####
  # factor correlation less than 1
  vfactor.cor <- ifcor_vec
  for (i in 1:length(vfactor.cor)){
    if(vfactor.cor[i]>=1){
      stop(paste("Factor correlation values must be less than 1. Check value number", i, '.', sep=' '))
    }
  }
  
  if (is.null(vfactor.cor)){
    stop("A vector of vfactor.cor is needed.")
  } else if (nrow(Fcor.tb)!=length(vfactor.cor)){
    stop(paste("The length of factor correlation vector (vfactor.cor) must be ",
               nrow(Fcor.tb), ",(", nFs, "*", (nFs-1), "/2)",
               ",given the number of factors:",nFs, sep=' '))
  } else {
    my_cormat_input = to_cormatrix(cor_vec = vfactor.cor, n_dim=3)
  }
  
  
  # convert interfactor cor to cov
  my_covmat_input <- to_covmatrix(cor_matrix = my_cormat_input, sd_vec=sd_vec)
  unscaled_ds_names <- NULL
  scaled_ds_names <- NULL
  set.seed(seed_num)
    X <- MultiRNG::draw.d.variate.normal(no.row = size,
                                          d = d, 
                                          mean.vec = mean_vec, 
                                          cov.mat = my_covmat_input)
    
    
    skew <- moments::skewness(X)
    kurt <- moments::kurtosis(X)
    cor_mat <- cor(X)
    cov_mat <- cov(X)
    scaled_X = scale(X)  
    return(list("X" = X, 
                "scaled.X" = scaled_X,
                "skew" = skew, 
                "kurt" = kurt, 
                "cor_mat" = cor_mat, 
                "cov_mat" = cov_mat))
}
