aux_mean <- function(trials, prob) {
  return(trials*prob)
}

aux_variance <- function(trials,prob) {
  return(trials*prob*(1-prob))
}

aux_mode <- function(trials, prob) {
  return(as.integer(trials*prob+prob))
}

#calculate the skewness of the distribution
aux_skewness <- function(trials, prob) {
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}

#calculate the kurtosis
aux_kurtosis <- function(trials, prob) {
  top <- 1-6*prob*(1-prob)
  bottom <- trials*prob*(1-prob)
  return(top/bottom)
}

