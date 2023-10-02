t_stat <- function(data, mu){
  denom <- sd(data, na.rm = TRUE)/sqrt(length(data))
  numer <- (mean(data) - mu)
  return(numer/denom)
}

hyp_test <- function(t_stat, n, sig_level, direction){
  df <- n-1
  p_value <- ifelse(direction == "left", qt(sig_level, df),
                    ifelse(direction == "right", qt(1-sig_level, df),
                           ifelse(direction == "two-sided", qt(c(sig_level/2, 1-(sig_level/2)), df),
                                   stop("enter direction ('left', right', two-sided')"))))
  return(all(p_value <= t_stat))
}
