#' @title kurtosis of binomial distribution
#' @description calculate the kurtosis of binomial distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return calculated kurtosis of binomial distribution
#' @export
#' @examples
#'
#' bin_kurtosis(10, 0.3)
#'
bin_kurtosis <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}
