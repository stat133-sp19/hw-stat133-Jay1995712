#' @title skewness of binomial distribution
#' @description calculate the skewness of binomial distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return calculated skewness of binomial distribution
#' @export
#' @examples
#'
#' bin_skewness(10, 0.3)
#'
bin_skewness <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}
