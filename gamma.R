gamma_test <- function(size, shape, rate, alpha, direction){
  g_dist <- rgamma(size, shape, rate)
  mean <- shape*(1/rate)
  return(hyp_test_wrap(g_dist, mean, alpha, direction))
}
gamma_test(100, 2, 5, .05, "right")

sum(replicate(10000, gamma_test(10, .2, 1, .05, "two-sided"), simplify="array"))/10000

mean(replicate(10, gamma_test(10, .2, 1, .05, "two-sided"), simplify="array"))

#######################

sum(sapply(1:100, FUN = gamma_test, size=10, shape=.2))/99

size <- c(10, 20, 30, 40, 50)
shape <- c(.2, .5, 1, 2, 5, 10, 20)
par_list <- as.list(expand.grid(size=size, shape=shape))

lapply(1:35, FUN=gamma_test, size=par_list$size[x], shape=par_list$shape[x])
