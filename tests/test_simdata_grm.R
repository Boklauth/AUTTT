# Test simdata_grm
library(AUTTT)

# n. of dimensions = 3
mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
f_cor <- c(0.3, 0.5, 0.6) # interfactor correlation vector
# convert the cor. vector to cor.matrix

# create theta values
# 2-multivariate normal theta with three dimensions

theta1 <- create_theta_mvn(size = 300, 
                           mean_vec = c(-0.1, 0, 0.1), 
                           sd_vec = c(0.95, 0.98, 1.1),
                           ifcor_vec = c(0.3, 0.5, 0.6))

theta1$scaled_ds



# CFA-OV parameters
set.seed(1234)
lambda <- runif(12, 0.8, 0.93) # loadings for 12 items
cat_prob2 <- c(0.04, 0.06, 0.11, 0.37, 0.42) # m. skewed prob.
temp <- TSK(n=300, res_prop = cat_prob2)
T2 <- temp$thresholds
# Convert to IRT parameters
# A vector of item discrimination
a_vec <- lambda/sqrt(1-lambda^2)
# a vector of item difficulty
nvar <- length(a_vec)
thresholds_m <- matrix(rep(T2, nvar), 
                       nrow = nvar, 
                       ncol=length(T2),
                       byrow = TRUE)
# item difficulty parameters
b_vec <- thresholds_m/lambda

# intercept parameters
d_vec <- -a_vec*b_vec

setwd("C:/Users/shh6304/Documents/My Documents/test_grm")

test1 <- simdata_grm(model = list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)),
                     # theta_matrix = unscaled_3f_mvn300,
                     theta_matrix = scaled_3f_mvn300, 
                     a = a_vec,
                     d = d_vec,
                     N = 300,
                     R = 50,
                     method = "U",
                     file_dir = getwd(),
                     file_prefix = "test1")

# response probabilities 
test1$res_prob

# response probabilities across items and replications
test1$avg_res_prob


# One factor model

# 1 dimension of theta
scaled_1f_mvn300 <- theta1$scaled_ds[,1]

test2 <- simdata_grm(model = list(seq(1,12)),
            theta_matrix = scaled_1f_mvn300,
            a = a_vec,
            d = d_vec,
            N = 300,
            R = 5,
            method = "N",
            file_dir = getwd(),
            file_prefix = "test1")

# Binary responses
# One factor model

cat_prob2 <- c(0.4, 0.6) # m. skewed prob.
temp <- TSK(n=300, res_prop = cat_prob2)
T2 <- temp$thresholds
# Convert to IRT parameters
# A vector of item discrimination
a_vec <- lambda/sqrt(1-lambda^2)
# a vector of item difficulty
nvar <- length(a_vec)
thresholds_m <- matrix(rep(T2, nvar), 
                       nrow = nvar, 
                       ncol=1,
                       byrow = TRUE)

b_vec <- thresholds_m/lambda

# intercept parameters
d_vec <- -a_vec*b_vec
  
setwd("C:/Users/shh6304/Documents/My Documents/test_grm")

simdata_grm(model = list(seq(1,12)),
            theta_matrix = unscaled_1f_mvn300,
            a = a_vec,
            d = d_vec,
            N = 300,
            R = 5,
            method = "N",
            file_dir = getwd(),
            file_prefix = "test1")
