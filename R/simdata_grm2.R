#' Simulate Observed Binary and Ordinal Variables Using Unidimenstional and Multidimensional Graded Response Models
#'
#' This function simulates observed binary and ordinal variables in unidimensional and
#' multidimensional IRT models using the graded response model (GRM) by Samejima (1969)
#' according to MPlus, which uses negative intercept as opposed to positive intercept in IRT.
#' It includes a seed number so that data can be reproducible.
#'
#' @param model A model as a list object (see examples).
#' @param theta_matrix An n x m matrix of latent variables (theta), where n is
#' the number of sample size, and m is the total number of dimensions.
#' @param a An p x 1 vector of item discrimination parameters, where p is the
#' total number of the observed variables. This vector must be
#' arranged in the order of appearance in the model.
#' @param d An p x Cj-1 matrix of the IRT intercept parameters, where Cj is the
#' total number of category responses. A category response is represented by k,
#' where k = 1,2,...,Cj.
#' @param R A numberic value for the total number of replications.
#' @param method A name for method: "U" and "N". "U" uses a lower-tail
#' cumulative probability , which is randomly drawn from U(0,1), a uniform
#' distribution from 0 and 1. "N" uses a lower-tail cumulative probability
#' from the standard normal distribution.
#' @param file_dir A directory for storing the data sets.
#' @param file_prefix A file prefix in characters or/and numbers but special
#' characters.
#' @return It generates data sets with the model specifications given. It also
#' provides response probabilities for each data set and average response
#' probabilities across the items and replications.
#'
#' @export
#' @examples
#' library(AUTTT)
#' # n. of dimensions = 3
#' mean_vec <- c(-0.1, 0, 0.1) # mean vector for each dimension
#' sd_vec <- c(0.95, 0.98, 1.1) # sd vector for each dimension
#' f_cor <- c(0.3, 0.5, 0.6) # interfactor correlation vector
#' # convert the cor. vector to cor.matrix
#' # 2-multivariate normal theta with three dimensions
#'
#' theta1 <- create_theta_mvn(size = 300,
#'                            mean_vec = c(-0.1, 0, 0.1),
#'                            sd_vec = c(0.95, 0.98, 1.1),
#'                            ifcor_vec = c(0.3, 0.5, 0.6),
#'                            seed_num = 456789)
#'
#' theta1$scaled.X
#'
#'
#'
#' # CFA-OV parameters
#' set.seed(1234)
#' lambda <- runif(12, 0.8, 0.93) # loadings for 12 items
#' cat_prob2 <- c(0.04, 0.06, 0.11, 0.37, 0.42) # m. skewed prob.
#' temp <- TSK(n=300, res_prop = cat_prob2)
#' T2 <- temp$thresholds
#' # Convert to IRT parameters
#' # A vector of item discrimination
#' a_vec <- lambda/sqrt(1-lambda^2)
#' # a vector of item difficulty
#' nvar <- length(a_vec)
#' thresholds_m <- matrix(rep(T2, nvar),
#'                        nrow = nvar,
#'                        ncol=length(T2),
#'                        byrow = TRUE)
#' # item difficulty parameters
#' b_vec <- thresholds_m/lambda
#'
#' # intercept parameters
#' d_vec <- -a_vec*b_vec
#'
#' setwd("C:/Users/shh6304/Documents/My Documents/test_grm")
#'
#' test1 <- simdata_grm2(model = list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12)),
#'                      # theta_matrix = unscaled_3f_mvn300,
#'                      theta_matrix = theta1$scaled.X,
#'                      a = a_vec,
#'                      d = d_vec,
#'                      R = 50,
#'                      method = "U",
#'                      file_dir = getwd(),
#'                      file_prefix = "test1",
#'                      seed_num = 12345)
#'
#' # response probabilities
#' test1$res_prob
#'
#' # response probabilities across items and replications
#' test1$avg_res_prob
#'
#'
#' # One factor model
#'
#' # 1 dimension of theta
#' scaled_1f_mvn300 <- theta1$scaled_ds[,1]
#'
#' simdata_grm2(model = list(seq(1,12)),
#'             theta_matrix = scaled_1f_mvn300,
#'             a = a_vec,
#'             d = d_vec,
#'             R = 5,
#'             method = "N",
#'             file_dir = getwd(),
#'             file_prefix = "test1",
#'             seed_num = 12345)
#'
#' # Binary responses
#' # One factor model
#'
#' cat_prob2 <- c(0.4, 0.6) # m. skewed prob.
#' temp <- TSK(n=300, res_prop = cat_prob2)
#' T2 <- temp$thresholds
#' # Convert to IRT parameters
#' # A vector of item discrimination
#' a_vec <- lambda/sqrt(1-lambda^2)
#' # a vector of item difficulty
#' nvar <- length(a_vec)
#' thresholds_m <- matrix(rep(T2, nvar),
#'                        nrow = nvar,
#'                        ncol=1,
#'                        byrow = TRUE)
#'
#' b_vec <- thresholds_m/lambda
#' # intercept parameters
#'
#' d_vec <- -a_vec*b_vec
#' #dir.create("C:/Users/shh6304/Documents/My Documents/test_grm")
#' setwd("C:/Users/shh6304/Documents/My Documents/test_grm")
#'
#' simdata_grm2(model = list(seq(1,12)),
#'             theta_matrix = unscaled_1f_mvn300,
#'             a = a_vec,
#'             d = d_vec,
#'             R = 5,
#'             method = "N",
#'             file_dir = getwd(),
#'             file_prefix = "test1",
#'             seed_num = 12345)
#' @references
#'
#' \insertRef{Samejima1969}{AUTTT}
#'
#' \insertRef{Luecht2018}{AUTTT}
#'
#' \insertRef{DeMars2012}{AUTTT}

simdata_grm2 <- function(model,
                        theta_matrix,
                        a,
                        d,
                        R,
                        method,
                        file_dir,
                        file_prefix,
                        seed_num
                        ){

  # declare global parameters ####
  theta_values <- theta_matrix
  nTs <- ncol(d) # n of thresholds

  # sample size
    N <- nrow(theta_matrix)


  # arrange item discrimination in a p x m form,
  # p = max n. of items, m max n. of factors
  df <- matrix(rep(0, length(a)), nrow=length(a), ncol=length(model))
  param <- a
  f <- 1
  for (f in 1:length(model)){
    start_n <-   min(unlist(model[f]))
    end_n <- max(unlist(model[f]))
    for (j in start_n : end_n){
      df[j,f] <- param[j]
    }
  }

  a_jf <- df

  # SIMULATE DATA SET FROM 1 TO R
  rep_list3 <- NULL
  res_prob_tb_BR2 <- NULL


  # convert theta_nf to type of data frame for valid indexing when dim = 1
  if(length(model)==1){
    theta_nf <- as.data.frame(theta_values)
  } else if (length(model)>1){
    theta_nf <- theta_values
  }

  for (r in 1:R){
    set.seed(seed_num + r)
    # data set using probability
    # if (sim_methods=="grm2" | sim_methods=="all"){
      cat_grm2 <- matrix(rep(0, N*nvar), nrow=N, ncol=nvar)
      person <- 1
      for (person in 1:N){
        x <- matrix(rep(0, nvar*nTs), nrow=nvar, ncol=nTs)
        cp_x <- matrix(rep(0, nvar*nTs), nrow=nvar, ncol=nTs)

        for (t in 1: nTs){
          x[,t] <- (a_jf %*% theta_nf[person,] + d[,t]) # iterate for each threshold for a person
          # convert it to cum. probabilities
          # per GRM's model (Samejima, 1969) P(x>=0) = 1, P(x >= k+1) = 0
          # P(x>=1) > P(x >= 2) >...> P(x >= k)
          cp_x[,t] <-  pnorm(x[,t], 0, 1, lower.tail = FALSE) # lower.tail should be FALSE
        }
        # x_jk (incorporating theta value in the term) for person i
        x_jk <- t(x) # will iterate for 1 person and all thresholds and items at a time
        # cumulative probabilities for 1 person, all thresholds, and all items
        # but I must add the prob. of item category 0, P(x >= 0) = 1
        # to avoid error at min(which()) below
        cp_x_jk <- t(cbind(1, cp_x))


        # select a probability from a uniform distribution

        # approach 1: using U(0,1); i.e., uniform dist. from 0 and 1
        if (method == "U"){
          q_u <- runif(nvar, 0, 1) # this equals punif(q_u, 0, 1, lower.tail = TRUE)
          p <- punif(q_u, 0, 1, lower.tail = FALSE)
        } else if (method == "N"){ # approach 2: using N(0,1); i.e., standard normal dist.
          q_n <- rep(rnorm(1, 0, 1), nvar)
          p <- pnorm(q_n, 0, 1, lower.tail = FALSE)
        }

        # categorize the underlying variables

        for (item in 1: nvar){
          cat_grm2[person,item] <- max(which(cp_x_jk[,item] >= p[item]))
        }


      } # END iterations for all persons

      # for binary response
      if (nTs == 1){
        cat_grm2[cat_grm2==1] <- 0
        cat_grm2[cat_grm2==2] <- 1
      }

      # check response probabilities per replication
      res_prob_tb <- NULL
      for (item in 1:nvar){
      res_prob <- table(cat_grm2[,item])
      res_prob_tb <- rbind(res_prob_tb, res_prob)
      }


      res_prob_tb_WR <- cbind(Rep = r,
                              item=seq(1,nvar),
                              res_prob_tb/rowSums(res_prob_tb))

      res_prob_tb_BR2 <- rbind(res_prob_tb_BR2, res_prob_tb_WR)


      # end checking response probabilities per replication

      # write data out per each replication
      write.table(cat_grm2,
                  paste0(file_dir, "/", file_prefix, "_grm2_rep", r, ".dat"),
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE)


# compile a list of rep names ####
rep_name <- paste0(file_prefix, "_grm2_rep", r, ".dat")
rep_list3 <- rbind(rep_list3, rep_name)

 # write it out
write.table(rep_list3,
            paste0(file_dir, "/", file_prefix, "_grm2_replist.dat"),
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE)




  } # END DATA REPLICATION

 # response probability average over all items and replications
  last_col <- ncol(res_prob_tb_BR2)
  start_col <- 3
  x <- res_prob_tb_BR2[,start_col:last_col]
  n <- nrow(x)
  avg_res_prob <- apply(x, MARGIN = 2, function(x) FUN = sum(x)/n)

  # write theta values out ####
  write.csv(theta_values,
            paste0(file_dir, "/", file_prefix, "_dsrep_theta.csv"),
            row.names = FALSE)

  return(list(model_spec = list(model = model,
                                theta_matrix = theta_matrix,
                                N = N,
                                R = R,
                                file_dir = file_dir,
                                file_prefix = file_prefix),
              pop_a = a_jf,
              pop_d = d,
              res_prob = res_prob_tb_BR2,
              avg_res_prob = avg_res_prob)
         )

} # END FUNCTION

