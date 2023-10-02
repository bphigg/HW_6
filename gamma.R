g_dist <- rgamma(100, .3, 4)
mean(g_dist)
hyp_test_wrap(g_dist, .075, .01, "right")

gamma_test <- function(size, shape, rate, alpha, direction){
  g_dist <- rgamma(size, shape, rate)
  mean <- shape*(1/rate)
  return(hyp_test_wrap(g_dist, mean, alpha, direction))
}
gamma_test(100, 2, 5, .05, "right")
