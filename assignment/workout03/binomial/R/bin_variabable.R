#' @title binomial variable
#' @description return the binomial variables
#' @param trials the number of tirals
#' @param prob the probability of a single success
#' @return a list that contains the number of tirals and the probability of a single success
#' @export
#' @examples
#'
#'  bin1 <- bin_variable(trials = 10, p = 0.3)
#'
bin_variable <- function(trials, prob) {
  x <- list(trials=trials,
            prob=prob)
  class(x) <- "binvar"
  return(x)
}

#' @export
print.binvar <- function(x,...) {
  cat('"Binomial Variable"', "\n")
  cat("\n")
  cat('Parameters', "\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success:", x$prob, "\n")
  invisible(x)
}

#' @export
summary.binvar <- function(x,...) {
  mean <- aux_mean(x$trials,x$prob)
  variance <- aux_variance(x$trials,x$prob)
  mode <- aux_mode(x$trials,x$prob)
  skewness <- aux_skewness(x$trials,x$prob)
  kurtosis <- aux_kurtosis(x$trials,x$prob)
  x <- list(trials=x$trials,prob=x$prob,
            mean=mean,variance=variance,
            mode=mode,skewness=skewness,
            kurtosis=kurtosis)
  class(x) <- "summary.binvar"
  return(x)
}

#' @export
print.summary.binvar <- function(x,...) {
  cat('"Summary Binomial"', "\n")
  cat("\n")
  cat('Parameters', "\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success:", x$prob, "\n")
  cat("\n")
  cat('Measures', "\n")
  cat('- mean:',x$mean, "\n")
  cat('- variance:',x$variance, "\n")
  cat('- mode:',x$mode,"\n")
  cat('- skewness:',x$skewness, "\n")
  cat('- kurtosis:',x$kurtosis, "\n")
  invisible(x)
}
