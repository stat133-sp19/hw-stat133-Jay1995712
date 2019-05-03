#' @title variance of binomial distribution
#' @description calculate the variance of binomial distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return calculated variance of binomial distribution
#' @export
#' @examples
#'
#' bin_variance(10, 0.3)
#'
bin_variance <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials,prob))
}
