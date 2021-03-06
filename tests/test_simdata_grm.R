# Test simdata_grm
library(AUTTT)

# n. of dimensions = 3
mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
f_cor <- c(0.3, 0.5, 0.75) # interfactor correlation vector
# convert the cor. vector to cor.matrix

# create theta values
# 2-multivariate normal theta with three dimensions

theta1 <- create_theta_mvn(size = 300, 
                           mean_vec = mean_vec, 
                           sd_vec = sd_vec,
                           ifcor_vec = f_cor, 
                           seed_num = 45678)

theta1$scaled.X



# CFA-OV parameters
set.seed(1234)
nvar <- 21
lambda <- runif(nvar, 0.8, 0.95) # loadings for 21 items
cat_prob2 <- c(0.04, 0.06, 0.11, 0.37, 0.42) # m. skewed prob.
temp <- TSK(n=300, res_prop = cat_prob2)
T2 <- temp$thresholds

# Convert to IRT parameters
# A vector of item discrimination
a_vec <- lambda/sqrt(1-lambda^2)

# threshold matrix 
thresholds_m <- matrix(rep(T2, nvar), 
                       nrow = nvar, 
                       ncol=length(T2),
                       byrow = TRUE)
# intercept parameters
d_vec <- thresholds_m/sqrt(1-lambda^2)

setwd("C:/Users/shh6304/Documents/My Documents/test_grm")
model1 <- list(c(1,2,3,4,5, 6, 7), 
               c(8, 9, 10, 11, 12, 13, 14), 
               c(15, 16, 17, 18, 19, 20, 21))
test1 <- simdata_grm(model = model1,
                     theta_matrix = theta1$scaled.X, 
                     a = a_vec,
                     d = d_vec,
                     N = 300,
                     R = 2,
                     method = "N",
                     file_dir = getwd(),
                     file_prefix = "test1")

test1$res_prob
test1$avg_res_prob

setwd("C:/Users/shh6304/Documents/My Documents/test_grm")

test2 <- simdata_grm(model = model1,
                     # theta_matrix = unscaled_3f_mvn300,
                     theta_matrix = theta1$scaled.X, 
                     a = a_vec,
                     d = d_vec,
                     N = 300,
                     R = 50,
                     method = "U",
                     file_dir = getwd(),
                     file_prefix = "test1")

# response probabilities 
test2$res_prob

# response probabilities across items and replications
test1$avg_res_prob


# One factor model

# 1 dimension of theta
scaled_1f_mvn300 <- theta1$scaled.X[,1]

test3 <- simdata_grm(model = list(seq(1,21)),
            theta_matrix = scaled_1f_mvn300,
            a = a_vec,
            d = d_vec,
            N = 300,
            R = 5,
            method = "N",
            file_dir = getwd(),
            file_prefix = "test1")

test3$avg_res_prob

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

# intercept parameters
d_vec <- thresholds_m/sqrt(1-lambda^2)
  
setwd("C:/Users/shh6304/Documents/My Documents/test_grm")

simdata_grm(model = list(seq(1,21)),
            theta_matrix = scaled_1f_mvn300,
            a = a_vec,
            d = d_vec,
            N = 300,
            R = 50,
            method = "N",
            file_dir = getwd(),
            file_prefix = "test1")
