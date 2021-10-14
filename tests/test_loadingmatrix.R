# test for to_loadingmatrix
library(AUTTT)
?to_loadingmatrix()
# 12 items with unequal number of items per dimension, 3 dimensions
lambs <- runif(12, 0.7, 0.95)
mymodel <- list(c(1,2,3,4), c(5,6,7), c(8, 9,10, 11, 12))
to_loadingmatrix(loading_vec = lambs, model = mymodel)

# 12 items with equal number of items per dimension, 2 dimensions
lambs <- runif(12, 0.7, 0.95)
mymodel <- list(c(1,2,3,4, 5, 6), c(7, 8, 9, 10, 11, 12))
to_loadingmatrix(loading_vec = lambs, model = mymodel)

# 12 items per dimension, 1 dimension
lambs <- runif(12, 0.7, 0.95)
mymodel <- list(c(1,2,3,4, 5, 6, 7, 8, 9, 10, 11, 12))
to_loadingmatrix(loading_vec = lambs, model = mymodel)

