#' Simulate A Gamma Multivariate Distribution from a Multivariate Normal Distribution
#'
#' To generate a positively skewed multivariate distribution from a 
#' multivariate normal distribution, this function uses the rejection sampling 
#' method, also known as the accept-reject method. 
#' 
#' @param shape A shape parameter for a gamma distribution.
#' @param rate A rate parameter for a gamma distribution.
#' @param mean_fec A mean vector for the multivariate normal (MVN) distribution.
#' @param cov_matrix A m x m covariance matrix for an MVN distribution, where m 
#' is the number of dimensions and must be equal to the length of the mean
#' vector. 
#' @param size is the sample size. 
#' @param c This is a constant such that f(x)/g(x) <= c, where f(x) is the density 
#' function of the standard normal distribution, and g(x) is a gamma distribution. 
#' @param seed_num This is an integer value served as the seed number. 
#'         
#' @export
#' @examples
#' find the constant
#'	y <- seq(-3, 3, 0.01)
#'	# plotting pdf of a gamma dist
#'	shape <- 5
#'	rate <- 5
#'	par(mfrow=c(1,1))
#'	plot(y, dgamma(y, shape=shape, rate = rate))
#'	plot(y, dgamma(y, shape = shape, rate = rate)/dnorm(y), 
#'	     ylim=c(1,6), 
#'	     xlim=c(0, 3.5), 
#'	     ylab = expression("f(y)/g(y)"), 
#'	     xlab = "A Random Variable Y")
#'	abline(h=4)
#'	# Constant c = 4 is appropriate.
#'	c <- 4
#'	
#'	mean_vec <- c(-0.1, 0, 0.1) # mean
#'	sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
#'	f_cor <- c(0.49, 0.74, 0.87) # correlation vector
#'	# convert the cor. vector to cor.matrix
#'	my_cormat_input <- AUTTT::to_cormatrix(cor_vec = f_cor, n_dim=3)
#'	# covert correlation matrix to covariance matrix
#'	my_covmat_input <- AUTTT::to_covmatrix(cor_matrix = my_cormat_input, sd_vec=sd_vec)
#'	seed_num <- 45679
#'	
#'	gamma_from_normal(shape = shape, 
#'	                  rate = rate, 
#'	                  mean_vec = mean_vec, 
#'	                  cov_matrix = my_covmat_input, 
#'	                  size = 300, 
#'	                  c = c, 
#'	                  seed_num = seed_num)
#'	# see the shape the sample
#'	par(mfrow = c(2,2))
#'	for (i in 1:3){
#'	  y <- sort(X[,i])
#'	  hist(y, prob=TRUE, ylim=c(0, 1))  
#'	  lines(y, dgamma(y, shape = 5, scale=1/4))
#'	}
#' @references
#' \insertRef{Braun2016}{AUTTT}
#' 
#' \insertRef{Maria2007}{AUTTT}
#' 
#' \insertRef{Robert2010}{AUTTT}
#' @seealso [dgamma()], [dnorm()]
#' 
gamma_from_normal <- function(shape, rate, mean_vec, cov_matrix, size, c,seed_num){
  require(MultiRNG)
  set.seed(seed_num)
  n_dim <- length(mean_vec)
  k <- 0 # counter for accepted
  j <- 0 # counter for iterations
  X <- matrix(rep(0, size*n_dim), ncol=n_dim)
  while (k < size) {
      j <- j+1
      U <- runif(1, 0, 1)
      # sample an observation from mvn
      Y <- MultiRNG::draw.d.variate.normal(no.row = 1,
                                           d = n_dim,
                                           mean.vec = mean_vec,
                                           cov.mat = my_covmat_input)
      
      if (dgamma(Y[1], shape = shape, rate = rate)/(dnorm(Y[1])*c) > U &
          dgamma(Y[2], shape = shape, rate = rate)/(dnorm(Y[2])*c) > U &
          dgamma(Y[3], shape = shape, rate = rate)/(dnorm(Y[3])*c) > U){
        # we accept y
        k <- k + 1
        X[k,1] <- Y[1]
        X[k,2] <- Y[2]
        X[k,3] <- Y[3]
      }
    }
    
  
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

