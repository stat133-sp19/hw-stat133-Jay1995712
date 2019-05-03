#' @title the number of combination
#' @description calculate the number of combination
#' @param n the number of trials
#' @param k the number of successes
#' @return the calculated number of combination in the situation of k successes in n trials
#' @export
#' @examples
#'
#' bin_choose(n=5,k=2)
#'

bin_choose <- function(n,k) {
  if (any(k>n)) {
    stop("k cannot be greater than n")
  } else {
    bottom <- factorial(k)*factorial(n-k)
    top <- factorial(n)
    return(top/bottom)
  }
}
