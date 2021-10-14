library(AUTTT)
# Example 1
lambs <- runif(12, .7, .95)
x <- lambs
diag_residuals(x = lambs)

# Example 2
lambs <- runif(12, .7, .95)
mymodel <- list(c(1,2,3,4), c(5,6,7), c(8, 9,10, 11, 12))
to_loadingmatrix(loading_vec = lambs, model = mymodel)
x <- lambs
diag_residuals(x = lambs)
