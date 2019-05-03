#' @title the probability of binomial distribution
#' @description  calculate the probability of having k successes in n trials
#' @param success the number of successes
#' @param trials the number of trials
#' @param prob the probability of success in a single trial
#' @return calculated proability of k successes in n trials
#' @export
#' @examples
#'
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'

bin_probability <- function(success,trials,prob) {
  check_success(success,trials)
  check_trials(trials)
  check_prob(prob)

  combo <- bin_choose(trials,success)
  part <- (prob^success)*(1-prob)^(trials-success)
  return(combo*part)
}
