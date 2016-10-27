
#' Luce's Choice Rule
#'
#' Transforms node-activation to chocie probabilities according to an exponential scaling rule (a special case of Thurstonian scaling).
#' @param activation vector with node activations
#' @inheritParams pcs_multi
#' @examples
#' luce_choice(c(A=-.5, B=.7, C=.3))
#' @export
luce_choice <- function(activation, lambda=2.9){
  if(!is.numeric(activation))
    stop("'activation' must be a numeric vector.")
  check_pars(lambda=lambda)
  exp(lambda*activation)/sum(exp(lambda*activation))
}
