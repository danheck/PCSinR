#' PCS-DM Weighting of Validities
#'
#' Transform validities (= probability of cue leading to correct decision) into decision weights for PCS network.
#'
#' @param v validities
#' @param p sensitivity parameter
#' @details The weighting function is:
#'
#' \eqn{w = sign(v-.5)*abs(v-.5)^p}
#'
#' Results in rather compensatory \eqn{p<1} or noncompensatory \eqn{p>1} weighting of cues.
#' @examples
#' # compensatory:
#' curve(validity_weight(x, p=.5), xlab="Validity")
#' # noncompensatory:
#' curve(validity_weight(x, p=2.5), xlab="Validity")
#' @export
validity_weight <- function(v, p=1.9) {
  check_pars(p)
  check_val(v)
  sign(v-.5)*abs(v-.5)^p
}
