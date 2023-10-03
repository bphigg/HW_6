g_dist <- rgamma(100, .3, 4)
mean(g_dist)
hyp_test_wrap(g_dist, .075, .01, "right")

gamma_test <- function(size, shape, rate, alpha, direction){
  g_dist <- rgamma(size, shape, rate)
  mean <- shape*(1/rate)
  return(hyp_test_wrap(g_dist, mean, alpha, direction))
}
gamma_test(100, 2, 5, .05, "right")

gamma_test(10, .2, 1, .05, "two-sided")

gamm_test <- function(size, shape, rate, alpha, direction){
  df <- size -1
  mean <- shape*(1/rate)
  return(hyp_test_wrap(x, mean, alpha, direction))
}

sapply(X=mean(rgamma(10, .2, 1)), FUN = hyp_test_wrap(x, .2, .05, "two-sided"), na.rm=TRUE)
