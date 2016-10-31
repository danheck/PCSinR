check_length <- function(list){
  if(any(sapply(list, length) != length(list[[1]]) ) )
    stop("Number of cues/validities does not match.")
}

check_val <- function(v){
  if(any(v<0 | v>1))
    stop("The validity vector 'v' must contain probabilities in [0,1].")
}


check_pars <- function(p=1.9,
                       decay=.1,
                       maxiter=1000,
                       stability=10^6,
                       lambda =2.9){
  if(p<0)
    stop("Sensitivity parameter 'p' must be nonnegative.")
  if(decay<0 | decay >= 1)
    stop("Decay parameter must be in [0,1).")
  if(maxiter != round(maxiter) | maxiter <1)
    stop("Maximum number of iterations must be a positive integer.")
  if(stability <= 0)
    stop("Stability parameter (convergence criterion) must be positive.")
  if(lambda <0)
    stop("Luce's choice-rule parameter 'lambda' must be nonnegative.")
}

check_start <- function(start, dim){
  if(length(start) != dim){
    stop("Length of 'start' does not match the number of cues (should be ", dim,")")
  }
}
