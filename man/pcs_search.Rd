% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcs_search.R
\name{pcs_search}
\alias{pcs_search}
\title{PCS-DM+S Cue Search for Multiattribute Decisions}
\usage{
pcs_search(c1, c2, v, start = NULL, bottomup = TRUE, p = 1.9,
  decay = 0.1, maxiter = 1000, stability = 10^-6, convergence = "floor",
  lambda = 2.9)
}
\arguments{
\item{c1}{cue values for Option 1: +/-1 for positive/negative cue values; 0 for hidden cue value; \code{NA} for non-available cue value)}

\item{c2}{cue values for second option}

\item{v}{vector of cue valdities defined as the probability that cue leads to correct decision if it discriminates between the the options (v=.5 for nonvalid cues)}

\item{start}{vector of node activations in the interval [-1,1] at start, ordered as: \code{c(driver, cues, option 1, option 2)}. Default: \code{start=c(1,0,0,...,0)}}

\item{bottomup}{whether to add unidirectional links from the option nodes to the hidden cue-value nodes (i.e., whether the attention to the options directly affects the attention on hidden cue values)}

\item{p}{sensitivity parameter determining weighting of cue validities (see \link{validity_weight})}

\item{decay}{decay parameter}

\item{maxiter}{maximum number of iterations}

\item{stability}{stability criterion that determines convergence(i.e., when to stop iterating)}

\item{convergence}{convergence criterion that evaluates the network's energy of the last 10 iterations. One of \code{"floor"} (compare floor of energy/stability for the current energy level vs. that of the last 10 iterations), \code{"sum"} (sum of absolute consecutive energy differences), or \code{"max"} (maximum of the absolute differences between the current energy level vs. that of the last 10 iterations)}

\item{lambda}{parameter used for Luce's choice rule (\link{luce_choice})}
}
\description{
For modeling the attraction search effect.
}
\examples{
search_next_cue <- pcs_search(c1=c(1,0), c2=c(0,0), v=c(.8,.7))
search_next_cue[1:6]
search_other_option <- pcs_search(c1=c(-1,0), c2=c(0,0), v=c(.8,.7))
search_other_option[1:6]
}
\author{
Daniel Heck
}
