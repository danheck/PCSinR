#' PCS-DM for Multiattribute Binary Decisions
#'
#' @param c1 cue values for first option (with values +/-1 for positive/negative and 0 for missing cue values)
#' @param c2 cue values for second option
#' @param v vector of cue valdities defined as the probability that cue leads to correct decision if it discriminates between the the options (v=.5 for nonvalid cues)
#' @param p sensitivity parameter determining weighting of cue validities (see \link{validity_weight})
#' @param lambda parameter used for Luce's choice rule (\link{luce_choice})
#' @inheritParams pcs_matrix
#' @examples
#' bb <- pcs_multi(c1 = c(-1,1,1), c2 = c(1,-1,-1),
#'                v = c(.85,.75,.6))
#' bb[1:4]
#' @export
pcs_multi <- function(c1, c2, v,
                     p=1.9,
                     decay=.1,
                     maxiter=1000,
                     stability=10^-6,
                     convergence="floor",
                     lambda =2.9) {
  check_length(list(c1,c2,v))
  check_val(v)
  check_pars(p, decay, maxiter, stability, lambda)

  cc <- length(c1)
  dim <- 1 + cc + 2

  # source ; cues; options
  weights = matrix(0,dim, dim)

  # Driver <-> Cues
  weights[1+1:cc,1] <- weights[1,1+1:cc] <- validity_weight(v, p)

  # Cues <-> Options
  weights[dim-1, 1+1:cc] <- weights[1+1:cc, dim-1]  <- .01*c1;
  weights[dim-2, 1+1:cc] <- weights[1+1:cc, dim-2]  <- .01*c2;

  # Option <-> Option
  weights[dim, dim-1] <- weights[dim-1,dim] <- -.2

  res <- pcs_matrix_cpp(weights, start = c(1, rep(0, dim-1)),
                        reset = c(1, rep(0, dim-1)),
                        decay, maxiter, stability, convergence)

  # choice prediction and probability
  activ_options = res$activation[dim + -1:0]
  res$choice <- which.max(activ_options)
  res$prob.option1 <- luce_choice(activ_options)[1]

  rownames(res$activation) <- colnames(res$process) <- names(res$activation) <-
    c("driver",paste0("cue",1:length(c1)),"option1","option2")

  res[c("choice","iterations","energy","prob.option1","activation","process","weights","convergence")]
}
# library(microbenchmark)
# microbenchmark(bb <- pcs_binary(c(-1,1,1), c(1,-1,-1),
#                                 c(.85,.75,.6)))
