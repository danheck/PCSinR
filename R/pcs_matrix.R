#' Generic PCS-DM Algorithm
#'
#' Takes a weight-matrix as input and performs fast iterated updating in C++
#' @param weights matrix of connection weights (rows: originating node; columns: incoming node)
#' @param start vector of node activations in the interval [-1,1] when starting the iterative algorithm
#' @param reset vector indicating which node activations should be reset to the activation at start. For nodes with a value of 0, the activation is updated; for any other number (e.g., \code{reset=1}), the respective number replaces the activation.
#' @param decay decay parameter
#' @param maxiter maximum number of iterations
#' @param stability stability criterion that determines convergence(i.e., when to stop iterating)
#' @param convergence convergence criterion that evaluates the network's energy of the last 10 iterations. One of \code{"floor"} (compare floor of energy/stability for the current energy level vs. that of the last 10 iterations), \code{"sum"} (sum of absolute consecutive energy differences), or \code{"max"} (maximum of the absolute differences between the current energy level vs. that of the last 10 iterations)
#' @param full whether to store the activation of all nodes for all iterations in a matrix
#' @details Note that a network with only bidirectional will result in a symmetric weight matrix.
#' @examples
#' w <- matrix(c(0,     0.047, 0,     0,
#'               0.047, 0,     0.01, -0.01,
#'               0,     0.01,  0,    -0.2,
#'               0,   -.01,   -0.2,   0), 4, 4)
#' pcs_matrix(w, start = c(1,0,0,0))
#' @author Daniel Heck
#' @export
pcs_matrix <- function(weights, start, reset = start, decay=.1, maxiter = 1000,
                       stability = 10^-6, convergence = "floor", full=TRUE) {

  check_pars(decay=decay, maxiter=maxiter, stability=stability)
    res <- pcs_matrix_cpp(weights, start, reset, decay, maxiter, stability,
                        convergence, full)

  dimnames(res$weights) <- dimnames(weights)
  names(res$activation) <- colnames(res$process) <- colnames(weights)
  if(!is.null(rownames(weights)))
    names(res$activation) <- colnames(res$process) <- rownames(weights)
  if(!full){
    res$process <- NULL
  }
  res
}
