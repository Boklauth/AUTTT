#' Thresholds, Skewness, and Kurtosis
#'
#' This package contains the TSK function {TSK()},
#' which allows you to calculate thresholds, skewness, and kurtosis
#' give categorical response probabilities.
#'
#' @param n A sample size as an interger.
#' @param res_prob A vector of categorical response probabilities.
#' @return TSK will return threshold values and corresponding response probabilities and cumulative probabilities.
#'         It will also return skewness and kurtosis values. Kurt1 uses the regular formula of kurtosis
#'         but it subtract the value with -3. Kurt2 is the original value.
#'         Kurt3 uses formula found in Sheskin (2000).
#' @export
#' @examples
#' # Probability of each response category
#' res1<-4/120
#' res2<-16/120
#' res3<-50/120
#' res4<-50/120
#' res.prop<-c(res1, res2, res3, res4)
#' TSK(n = 1200, res_prop = res.prop)
#'
#' @references
#'
#' \insertRef{Hahs_Vaughn2012}{AUTTT}
#'
#' \insertRef{Finney2013}{AUTTT}
#'
#' \insertRef{Kline2015}{AUTTT}
#'
#' \insertRef{McDonald1999}{AUTTT}
#'
#' \insertRef{UCLA}{AUTTT}

TSK <- function(n, res_prop){
  ncat <- length(res_prop)
  cat <- seq(1, ncat)
  freq <- res_prop*n
  total_all <- sum(cat*res_prop*n)
  mu <- total_all/n

  # skewness
  g1 <- sum((freq*(cat-mu)^3)/n)/(sum((freq*(cat-mu)^2)/n)^(3/2)) # correct

  # adjusted skewness
  G1 <- g1*sqrt(n*(n-1))/(n-2)

  # kurtosis
  s2 <- sum((freq*(cat-mu)^2))
  s4 <- sum((freq*(cat-mu)^4))
  m2 <- s2/n
  m4 <- s4/n
  var_responses <- m2*n/(n-1)
  Kurt1 <- (m4/m2^2)-3 # correct
  Kurt2 <- Kurt1 + 3
  Kurt3 <- n*(n+1)*s4/((n-1)*(n-2)*(n-3)*var_responses^2)-3*((n-1)^2)/((n-2)*(n-3))

  # test with R function

  # responses <- rep(cat, freq)
  # m2 <- sum((responses-mean(responses))^2)/n
  # m4 <- sum((responses-mean(responses))^4)/n
  # R.g1 <- moments::skewness(responses)
  # R.G1 <- R.g1*sqrt(n*(n-1))/(n-2)
  # R.Kurt <- moments::kurtosis(responses)-3

  # calculate thresholds
  # cumulative proportions
  cumulative_pro <- cumsum(res_prop)

  # Threshold or quantile for factor analysis with ordinal variables

  Tau <- qnorm(cumulative_pro, 0, 1, lower.tail = TRUE)[1:(ncat-1)]

  # threshold_tb<-cbind(res_prop, cumulative_pro, Tau)



    return(list(cat_prob = res_prop,
                cum_prob = cumulative_pro,
                thresholds = Tau,
                skew = g1,
                adjusted_skew = G1,
                Kurt1 = Kurt1,
                Kurt2 = Kurt2,
                Kurt3 = Kurt3))

}

