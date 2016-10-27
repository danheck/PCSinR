#' Generic PCS-DM Algorithm
#'
#' Takes a weight-matrix as input and performs fast iterated updating in C++
#' @param weights matrix of connection weights (rows: originating node; columns: incoming node)
#' @param start vector of node activations at start
#' @param reset vector indicating which node activations should be reset to the activation at start. For nodes with a value of 0, the activation is updated; for any other number (e.g., \code{reset=1}), the respective number replaces the activation.
#' @param decay decay parameter
#' @param maxiter maximum number of iterations
#' @param stability stability criterion that determines convergence(i.e., when to stop iterating)
#' @param convergence convergence criterion that evaluates the network's energy of the last 10 iterations. One of \code{"floor"} (identical floor of energy/stability for the current vs. remaining 9 nodes), \code{"sum"} (sum of absolute consecutive energy differences), or \code{"max"} (maximum of the absolute differences between the current and remaining 9 nodes)
#' @param full whether to store the node activations of all iterations in a matrix
#' @details Note that a network with only bidirectional will result in a symmetric weight matrix.
#' @examples
#' w <- matrix(c(0,     0.047, 0,     0,
#'               0.047, 0,     0.01, -0.01,
#'               0,     0.01,  0,    -0.2,
#'               0,   -.01,   -0.2,   0), 4, 4)
#' pcs_matrix(w, start = c(1,0,0,0))
#' @importFrom Rcpp evalCpp
#' @useDynLib PCSinR
#' @export
pcs_matrix <- function(weights,
                       start,
                       reset = start,
                       decay=.1,
                       maxiter = 1000,
                       stability=10^-6,
                       convergence="floor",
                       full=TRUE) {
  check_pars(decay=decay, maxiter=maxiter, stability=stability)

  res <- pcs_matrix_cpp(weights,
                        start,
                        reset,
                        decay,
                        maxiter,
                        stability,
                        convergence,
                        full)
  if(!full) res$process <- NULL
  colnames(res$activation) <- colnames(weights)
  if(!is.null(rownames(weights)))
    colnames(res$activation) <- rownames(weights)
  res
}
