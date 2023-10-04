gamma_test_p <- function(size, shape, rate = 1){
  g_dist <- rgamma(size, shape, rate)
  mean <- shape*(1/rate)
  return(hyp_test_wrap(g_dist, mean, .05, "two-sided"))
}
gamma_test(100, 2, 5, .05, "right")

sum(replicate(10000, gamma_test_p(x, 10, .2), simplify="array"))/10000

sum(sapply(1:100, FUN = gamma_test_p, size=10, shape=.2))/99

# mean(replicate(10, gamma_test(10, .2, 1, .05, "two-sided"), simplify="array"))
library(dplyr)

size <- c(10, 20, 30, 40, 50)
shape <- c(.2, .5, 1, 2, 5, 10, 20)
par_list <- expand.grid(size=size, shape=shape)

parameters <- lapply(1:35, FUN = function(x){as.list(par_list[x, ])})
pars_size <- lapply(1:35, FUN = function(x){parameters[[x]][1]})
pars_shape <- lapply(1:35, FUN = function(x){parameters[[x]][2]})
# data_frame(shape = unlist(pars_shape), size = unlist(pars_size))

get_results <- function(x){
  mean(replicate(10, gamma_test_p(parameters[[x]][[1]], parameters[[x]][[2]], rate = 1)))
}
df_temp <- data_frame(parameters, results)

results <- lapply(1:35, FUN=get_results)
#results <- replicate(10, lapply(1:35, FUN=get_results))
#temp <- apply(results, MARGIN=1, FUN=sum)/9
shape_0.2 <- c(temp[1:5])
shape_0.5 <- c(temp[6:10])
shape_1 <- c(temp[11:15])
shape_2 <- c(temp[16:20])
shape_5 <- c(temp[21:25])
shape_10 <- c(temp[26:30])
shape_20 <- c(temp[31:35])
results_df <-data.frame(shape_0.2, shape_0.5, row.names=c("10", "20", "30", "40", "50"))
