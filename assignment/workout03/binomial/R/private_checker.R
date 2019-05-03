
check_prob <- function(prob) {
  if (prob>=0 & prob<=1) {
    x=T
  } else {
    stop("invaild prob value")
  }
  return(x)
}

check_trials <- function(trials) {
  if (trials>=0 & trials==as.integer(trials)) {
    return(T)
  } else {
    stop("invaild trials value")
  }
}

check_success <- function(success, trials) {
  if (any(success!=as.integer(success)) | any(success<0)) {
    stop("invaild success value")
  } else if (any(success>trials)) {
    stop("success cannot be
greater than trials")
  } else {
    return(T)
  }
}


