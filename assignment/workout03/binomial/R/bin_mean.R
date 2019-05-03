#' @title mean of binomial distribution
#' @description calculate the mean of binomial distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return calculated mean of binomial distribution
#' @export
#' @examples
#'
#' bin_mean(10, 0.3)
#'
bin_mean <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}
