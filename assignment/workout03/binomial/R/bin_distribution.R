#' @title binomial proability distribution
#' @description display the data frame of a binomial proability distribution
#' @param trials the number of trials
#' @param prob the proability of a single success
#' @return a data frame displaying the binomial proability distribution
#' @export
#' @examples
#'
#'  bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <-function(trials, prob) {
  vector_prob <- bin_probability(0:trials,trials,prob)
  dat <- data.frame(success=0:trials,probability=vector_prob)
  class(dat) <- c("bindis","data.frame")
  dat
}

#' @export
plot.bindis <- function(x,...) {
  graph <- barplot(x$probability, names.arg = x$success,
                   xlab = "successes", ylab = "probability")
  return(graph)
  invisible(x)
}
