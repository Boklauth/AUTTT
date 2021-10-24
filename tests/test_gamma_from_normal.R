# find the constant
library(AUTTT)
y <- seq(-3, 3, 0.01)
# plotting pdf of a gamma dist
shape <- 5
rate <- 5
par(mfrow=c(1,1))
plot(y, dgamma(y, shape=shape, rate = rate))
plot(y, dgamma(y, shape = shape, rate = rate)/dnorm(y), 
     ylim=c(1,6), 
     xlim=c(0, 3.5), 
     ylab = expression("f(y)/g(y)"), 
     xlab = "A Random Variable Y")
abline(h=4)
# Constant c = 4 is appropriate.
c <- 4

mean_vec <- c(-0.1, 0, 0.1) # mean
sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
f_cor <- c(0.49, 0.74, 0.87) # correlation vector
# convert the cor. vector to cor.matrix
my_cormat_input <- AUTTT::to_cormatrix(cor_vec = f_cor, n_dim=3)
# covert correlation matrix to covariance matrix
my_covmat_input <- AUTTT::to_covmatrix(cor_matrix = my_cormat_input, sd_vec=sd_vec)
seed_num <- 45679

gamma_from_normal(shape = shape, 
                  rate = rate, 
                  mean_vec = mean_vec, 
                  cov_matrix = my_covmat_input, 
                  size = 300, 
                  c = c, 
                  seed_num = seed_num)
# see the shape the sample
par(mfrow = c(2,2))
for (i in 1:3){
  y <- sort(X[,i])
  hist(y, prob=TRUE, ylim=c(0, 1))  
  lines(y, dgamma(y, shape = 5, scale=1/4))
}
