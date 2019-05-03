#' @title mode of binomial distribution
#' @description calculate the mode of binomial distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return calculated mode of binomial distribution
#' @export
#' @examples
#'
#' bin_mode(10, 0.3)
#'
bin_mode <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}
