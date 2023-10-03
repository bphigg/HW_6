t_stat <- function(data, mu){
  denom <- sd(data, na.rm = TRUE)/sqrt(length(data))
  numer <- (mean(data) - mu)
  return(numer/denom)
}
################################################################################
#hyp_test_1 <- function(t_stat, n, sig_level, direction){
#  df <- n-1
#  significance <- ifelse(direction == "left", qt(sig_level, df),
#                    ifelse(direction == "right", qt(1-sig_level, df),
#                           ifelse(direction == "two-sided", qt(c(sig_level/2, 1-(sig_level/2)), df),
#                                   stop("enter direction ('left', right', two-sided')"))))
#  return(all(t_stat >= significance))
#}
################################################################################
hyp_test <- function(t_stat, n, sig_level, direction){
  df <- n-1
  significance <- ifelse(direction == "left", return(t_stat <= qt(sig_level, df)),
                         ifelse(direction == "right", return(t_stat >= qt(1-sig_level, df)),
                                ifelse(direction == "two-sided", return(t_stat <= qt(sig_level/2, df) | t_stat >= qt(1-sig_level/2, df)),
                                       stop("enter direction ('left', right', two-sided')"))))
}

hyp_test_wrap <- function(data, mu, sig_level, direction){
  n <- length(data)
  df <- n-1
  t_stat <- t_stat(data, mu)
  test <- hyp_test(t_stat, n, sig_level, direction)
  return(test)
}

hyp_test_wrap(iris$Petal.Length, 4, .05, "left")
hyp_test(-2.31, 150, .10, "two-sided")
