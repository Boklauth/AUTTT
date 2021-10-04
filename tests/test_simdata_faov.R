# example: Three-factor model
n_dim <- 3 # n. of dimensions 
mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
f_cor <- c(0.3, 0.5, 0.6) # interfactor correlation vector
# convert the cor. vector to cor.matrix
library(AUTTT)
my_cormat_input <- to_cormatrix(cor_vec = f_cor, n_dim=3)
my_covmat_input <- to_covmatrix(cor_matrix = my_cormat_input, sd_vec=sd_vec)

nvar <- 12
lambda <- runif(nitems, 0.8, 0.93) # loadings for 12 items
cat_prob2 <- c(0.04, 0.06, 0.11, 0.37, 0.42) # m. skewed prob.
temp <- TSK(n=300, res_prop = cat_prob2)

T2 <- temp$thresholds
thresholds_m <- matrix(rep(T2, nvar), 
                       nrow = nvar, 
                       ncol=length(T2),
                       byrow = TRUE)

setwd("C:/Users/shh6304/Documents/My Documents/test_faov")

simdata_faov(model = list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)),
             f_mean = mean_vec,
             f_cov_matrix = my_covmat_input,
             theta_matrix = NULL,
             vloadings = lambda,
             thresholds = thresholds_m,
             N = 300,
             R = 5,
             seed = 123456,
             file_dir = getwd(),
             file_prefix = "test1")

# One factor model

simdata_faov(model = list(seq(1,12)),
             f_mean = 0,
             f_cov_matrix = 0.95,
             theta_matrix = NULL,
             vloadings = lambda,
             thresholds = thresholds_m,
             N = 300,
             R = 5,
             seed = 123456,
             file_dir = getwd(),
             file_prefix = "test1")

# Binary responses
# One factor model

cat_prob2 <- c(0.4, 0.6) # m. skewed prob.
temp <- TSK(n=300, res_prop = cat_prob2)
T2 <- temp$thresholds
thresholds_m <- matrix(rep(T2, nvar), 
                       nrow = nvar, 
                       ncol=1,
                       byrow = TRUE)
simdata_faov(model = list(seq(1,12)),
             f_mean = 0,
             f_cov_matrix = 0.95,
             theta_matrix = NULL,
             vloadings = lambda,
             thresholds = thresholds_m,
             N = 300,
             R = 5,
             seed = 123456,
             file_dir = getwd(),
             file_prefix = "test1")
