
#' PCS-DM for Gambles (to do)
#'
#' Modeling gambles
#' @param g1 Gamble 1, represented as a vector of probabilities, where the possible gains and losses are encoded by the vector names (e.g., \code{g1 = c("100"=.3, "-50"=.2, "0"=.5)})
#' @param g2 Gamble 2, see \code{g1}
#' @inheritParams pcs_matrix
#' @export
pcs_gamble <- function(g1, g2,  decay = 0.1, maxiter = 1000,
                       stability = 10^-6, convergence = "floor"){

  # construct weight matrix

  # run pcs
  # pcs_matrix(weights, ...)
  NULL
}
