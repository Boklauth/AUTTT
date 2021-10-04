#' Simulate Observed Binary and Ordinal Variables Using Factor Analysis with Ordinal Variables 
#'
#' This function simulates observed binary and ordinal variables in unidimensional and 
#' multidimensional models using the common factor model (CFM) factor analysis 
#' with ordinal variables, which is also known as the categorical variable
#' methodology, CVM.
#'  
#' @param model A model as a list object (see examples).
#' @param f_mean A mean vector for the latent variables.
#' @param f_cov_matrix A m x m matrix of the latent variances and covariances, 
#' where m is the total number of dimensions. 
#' @param theta_matrix An n x m matrix of latent variables, where n is 
#' the number of sample size, and m is the total number of dimensions. 
#' @param vloadings A p x 1 vector of factor loadings, where p is the total 
#' number of the observed variables. This vector must be 
#' @param thresholds An p x Cj-1 matrix of the standardized thresholds,
#' where Cj is the total number of category responses. A category response is 
#' represented by k, where k = 1,2,...,Cj.
#' @param N A numeric value for the  sample size
#' @param R A numberic value for the total number of replications. 
#' @param seed A seed number used to generate the population latent variables
#' variance-covariance matrix. If a seed number is not provided, a seed number 
#' 12345 will be used as a default value. When a the latent values are supplied, 
#' the seed number is not used. 
#' @param file_dir A directory for storing the data sets. 
#' @param file_prefix A file prefix in characters or/and numbers but special 
#' characters. 
#' @return It generates data sets with the model specifications given. It 
#' provides also response probabilities for each data set and average 
#' response probabilities across the items and replications. 
#' 
#' @export
#' @examples
#' library(AUTTT)
#' # example: Three-factor model
#' n_dim <- 3 # n. of dimensions 
#' mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
#' sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
#' f_cor <- c(0.3, 0.5, 0.6) # interfactor correlation vector
#' # convert the cor. vector to cor.matrix
#' library(AUTTT)
#' my_cormat_input <- to_cormatrix(cor_vec = f_cor, n_dim=3)
#' my_covmat_input <- to_covmatrix(cor_matrix = my_cormat_input, sd_vec=sd_vec)
#' 
#' nvar <- 12
#' lambda <- runif(nitems, 0.8, 0.93) # loadings for 12 items
#' cat_prob2 <- c(0.04, 0.06, 0.11, 0.37, 0.42) # m. skewed prob.
#' temp <- TSK(n=300, res_prop = cat_prob2)
#' 
#' T2 <- temp$thresholds
#' thresholds_m <- matrix(rep(T2, nvar), 
#'                         nrow = nvar, 
#'                         ncol=length(T2),
#'                         byrow = TRUE)
#'                         
#' setwd("C:/Users/shh6304/Documents/My Documents/test_faov")
#' 
#' simdata_faov(model = list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)),
#'              f_mean = mean_vec,
#'              f_cov_matrix = my_covmat_input,
#'              theta_matrix = NULL,
#'              vloadings = lambda,
#'              thresholds = thresholds_m,
#'              N = 300,
#'              R = 5,
#'              seed = 123456,
#'              file_dir = getwd(),
#'              file_prefix = "test1")
#'   
#' # One factor model
#' 
#' simdata_faov(model = list(seq(1,12)),
#'              f_mean = 0,
#'              f_cov_matrix = 0.95,
#'              theta_matrix = NULL,
#'              vloadings = lambda,
#'              thresholds = thresholds_m,
#'              N = 300,
#'              R = 5,
#'              seed = 123456,
#'              file_dir = getwd(),
#'              file_prefix = "test1")
#' 
#' # Binary responses
#' # One factor model
#'  
#' cat_prob2 <- c(0.4, 0.6) # m. skewed prob.
#' temp <- TSK(n=300, res_prop = cat_prob2)
#' T2 <- temp$thresholds
#' thresholds_m <- matrix(rep(T2, nvar), 
#'                         nrow = nvar, 
#'                         ncol=1,
#'                         byrow = TRUE)
#' simdata_faov(model = list(seq(1,12)),
#'              f_mean = 0,
#'              f_cov_matrix = 0.95,
#'              theta_matrix = NULL,
#'              vloadings = lambda,
#'              thresholds = thresholds_m,
#'              N = 300,
#'              R = 5,
#'              seed = 123456,
#'              file_dir = getwd(),
#'              file_prefix = "test1")
#' @references
#' 
#' \insertRef{Muthen1978}{AUTTT}
#' 
#' \insertRef{Muthen1984a}{AUTTT}
#' 
#' \insertRef{DeMars2012}{AUTTT}

simdata_faov <- function(model,
                         f_mean,
                         f_cov_matrix,
                         theta_matrix = NULL,
                         vloadings,
                         thresholds,
                         N, 
                         R, 
                         seed = 123456,
                         file_dir,
                         file_prefix){
  
  # declare some global parameters ####
  thresholds_m <- as.matrix(thresholds)
  nTs <- ncol(thresholds_m) # n of thresholds
  nvar <- length(unlist(model))# number of variables/items
  seed_num <- seed
  
  # f_cov_matrix and theta values
  if (!is.null(theta_matrix)) {
    set.seed(seed_num)
    theta_values <- theta_matrix
    f_cov_matrix <- cov(theta_matrix)
    f_mean <- colMeans(theta_matrix)
  } else if (is.null(theta_matrix)) {
    theta_values <- NULL
    f_mean <- f_mean
    f_cov_matrix <- f_cov_matrix
  }
  

  # FA-OV item parameters #### 
  
  # Loadings
  # turn loadings into a matrix
  
  df <- matrix(rep(0, nvar), 
               nrow = nvar, ncol = length(model))
  param <- vloadings
  
  f <- 1
  for (f in 1:length(model)) {
    start_n <-   min(unlist(model[f]))
    end_n <- max(unlist(model[f]))
    for (j in start_n : end_n) 
      df[j,f] <- param[j]
  }

LAMB<- df

# communality: You sum for each variable (row) across factors (columns).
H2 <- apply(LAMB^2, 1, sum) # 1 indicate rows, aka rowSum()

# Error variances
# residual variance/uniqueness/error leftover from communality
# This is the error of the observed indicators that are not explained by the factors.
epsilon <- 1 - H2 # uniqueness
epsilon_matrix <- diag(epsilon)
# model implied covariance matrix: if dimension > 1 ####
# calculate observed covariance matrix ####
# (pxm)*(mxm)*(mxp) + (pxp)
if (length(model)>1){
  obse_cov_matrix <- LAMB %*% f_cov_matrix %*% t(LAMB) + epsilon_matrix
} else if (length(model) ==1){
  obse_cov_matrix <- LAMB %*% f_cov_matrix %*% t(LAMB) + epsilon_matrix
}

# SIMULATE DATA SET FROM 1 TO R
rep_list1 <- NULL
res_prob_tb_BR2 <- NULL
for (r in 1:R) {
  dataset <- MASS::mvrnorm(n = N, 
                           mu = rep(0, nvar), # mean of the variables = 0
                           Sigma = obse_cov_matrix,
                           tol=1e-6, 
                           empirical=FALSE,
                           EISPACK = FALSE)
  # Categorize the data set
  dataset <- scale(dataset)
  cat <- dataset
  for (item in 1:nvar) {
    for (i in 1:N) {
      if(dataset[i, item] <= thresholds_m[item, 1]) {
        cat[i, item] = 1
      } else if (cat[i, item] > thresholds_m[item, nTs]) {
        cat[i, item] = nTs + 1
      } else {
        for (tt in 1:(nTs - 1)) {
          if (dataset[i, item] > thresholds_m[item, tt] & dataset[i, item] <= thresholds_m[item, (tt + 1)]) {
            cat[i, item] = tt + 1
          }
        }
      }
    }
  }
  
  
  # for binary response
  if (nTs == 1){
    cat[cat==1] <- 0
    cat[cat==2] <- 1
  }
  
  # check response probabilities per replication
  res_prob_tb <- NULL
  for (item in 1:nvar){
    res_prob <- table(cat[,item])
    res_prob_tb <- rbind(res_prob_tb, res_prob)
  }
  
  
  res_prob_tb_WR <- cbind(Rep = r, 
                          item=seq(1,nvar), 
                          res_prob_tb/rowSums(res_prob_tb))
  
  res_prob_tb_BR2 <- rbind(res_prob_tb_BR2, res_prob_tb_WR) 
  
  # write data out
  write.table(cat, 
              paste0(file_dir, "/", file_prefix, "_faov_rep", r, ".dat"),
              row.names = FALSE, 
              col.names = FALSE,
              quote = FALSE)
  # compile a list of rep names ####
  rep_name <- paste0(file_prefix, "_faov_rep", r, ".dat")
  rep_list1 <- rbind(rep_list1, rep_name)
  # write it out
  write.table(rep_list1, 
              paste0(file_dir, "/", file_prefix, "_faov_replist.dat"),
              row.names = FALSE, 
              col.names = FALSE,
              quote = FALSE)
} 
# END DATA REPLICATION

# response probability average over all items and replications
last_col <- ncol(res_prob_tb_BR2)
start_col <- 3
x <- res_prob_tb_BR2[,start_col:last_col]
n <- nrow(x)
avg_res_prob <- apply(x, MARGIN = 2, function(x) FUN = sum(x)/n)

# write theta values out ####
if(!is.null(theta_matrix)){ 
  write.csv(theta_values, 
            paste0(file_dir, "/", file_prefix, "_dsrep_theta.csv"),
            row.names = FALSE)
  return(list(pop_theta = theta_values,
              pop_loadings = LAMB,
              pop_factor_covariances = f_cov_matrix,
              pop_error_covariances = epsilon_matrix,
              pop_observed_var_covariances = obse_cov_matrix,
              pop_thresholds_m = thresholds_m,
              res_prob = res_prob_tb_BR2, 
              avg_res_prob = avg_res_prob,
              model_spec = list(model = model,
                                f_mean = f_mean,
                                f_cov_matrix = f_cov_matrix,
                                theta_matrix = theta_values,
                                vloadings = vloadings,
                                thresholds = thresholds,
                                N = N,
                                R = R,
                                seed = seed,
                                file_dir = file_dir,
                                file_prefix = file_prefix)))
} else {
  return(list(pop_theta = theta_values,
              pop_loadings = LAMB,
              pop_factor_covariances = f_cov_matrix,
              pop_error_covariances = epsilon_matrix,
              pop_observed_var_covariances = obse_cov_matrix,
              pop_thresholds_m = thresholds_m,
              res_prob = res_prob_tb_BR2, 
              avg_res_prob = avg_res_prob,
              model_spec = list(model = model,
                                f_mean = f_mean,
                                f_cov_matrix = f_cov_matrix,
                                vloadings = vloadings,
                                thresholds = thresholds,
                                N = N,
                                R = R,
                                seed = seed,
                                file_dir = file_dir,
                                file_prefix = file_prefix)))}

} # END FUNCTION
