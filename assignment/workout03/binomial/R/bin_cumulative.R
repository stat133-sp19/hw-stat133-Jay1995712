#' @title binomial culmulative distribution
#' @description display the binomial culmulative distribution
#' @param trials the number of trials
#' @param prob the probability of a single success
#' @return a data frame displaying the binomial culmulative distribution
#' @export
#' @examples
#'
#' bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob) {
  dat <- bin_distribution(trials,prob)
  cum_prob <- cumsum(dat$probability)
  dat <- data.frame(dat,cumulative=cum_prob)
  class(dat) <- c("bincum","data.frame")
  return(dat)
}

#' @export
plot.bincum <- function(x,...) {
  plot(x$success,x$cumulative, xlab = "successes",ylab="probability")
  lines(x$success,x$cumulative)
  invisible(x)
}
