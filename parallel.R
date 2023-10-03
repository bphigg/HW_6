gamma_test <- function(x, size, shape, rate = 1){
  g_dist <- rgamma(size, shape, rate)
  mean <- shape*(1/rate)
  return(hyp_test_wrap(g_dist, mean, .05, "two-sided"))
}
gamma_test(100, 2, 5, .05, "right")

sum(replicate(10000, gamma_test(x, 10, .2), simplify="array"))/10000

sum(sapply(1:100, FUN = gamma_test, size=10, shape=.2))/99


size <- c(10, 20, 30, 40, 50)
shape <- c(.2, .5, 1, 2, 5, 10, 20)
par_list <- as.list(expand.grid(size=size, shape=shape))

get_results <- function(x, size = par_list$size[x], shape = par_list$shape[x]){
  gamma_test(x, size, shape, rate = 1)
}

lapply(1:35, FUN=get_results)
replicate(10, sapply(1:35, FUN=get_results))
