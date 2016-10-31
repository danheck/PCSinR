
#' PCS-DM Cue Search for Multiattribute Decisions
#'
#' For modeling the attraction search effect.
#' @param c1 cue values for Option 1: +/-1 for positive/negative cue values; 0 for hidden cue value; \code{NA} for non-available cue value)
#' @inheritParams pcs_multi
#' @inheritParams pcs_matrix
#' @param start vector of node activations in the interval [-1,1] at start, ordered as: \code{c(driver, cues, option 1, option 2)}. Default: \code{start=c(1,0,0,...,0)}
#' @examples
#' search_next_cue <- pcs_search(c1=c(1,0), c2=c(0,0), v=c(.8,.7))
#' search_next_cue[1:6]
#' search_other_option <- pcs_search(c1=c(-1,0), c2=c(0,0), v=c(.8,.7))
#' search_other_option[1:6]
#' @export
pcs_search <- function(c1, c2,
                       v,
                       start=NULL,
                       p=1.9,
                       decay=.1,
                       maxiter=1000,
                       stability=10^-6,
                       convergence="floor",
                       lambda =2.9) {
  cues <- length(c1)
  check_length(list(c1,c2,v))
  check_val(v)
  check_pars(p, decay, maxiter, stability, lambda)

  # make a weight matrix
  names <- c("driver",
             paste0("cue",1:cues),
             paste0("opt1cue",1:cues),
             paste0("opt2cue",1:cues),
             "option1","option2")
  d <- 1 +  length(c1) + length(c1)*2 + 2
  # driver / cues / cue-option combinations / options
  weights <- matrix(0, d, d,
                    dimnames=list("from"=names,"to"=names))

  # Driver <-> Cue
  weights[1,1+1:cues] <- weights[1+1:cues,1] <- validity_weight(v, p = p)
  # Option1 <-> Option 2
  weights[d-1,d] <- weights[d,d-1] <- -.2

  for(cc in 1:cues){
    #### Option 1 ###
    if(is.na(c1[cc])){
    }else if(c1[cc] == 0){
      # cue --> cue-option
      weights[1+cc,1+cues+cc] <- .1
      # cue-option <-- option
      weights[d-1,1+cues+cc] <- .01
    }else{
      # cue <-> cue-option
      weights[1+cc,1+cues+cc] <- weights[1+cues+cc,1+cc] <- .1
      # cue-option <-> option
      weights[d-1,1+cues+cc] <- weights[1+cues+cc,d-1] <- c1[cc]*.01
    }

    #### Option 2 ###
    if(is.na(c2[cc])){
    }else if(c2[cc] == 0){
      # cue --> cue-option
      weights[1+cc,1+cues*2+cc] <-.1
      # cue-option <-- option
      weights[ d,1+cues*2+cc] <- .01
    }else{
      # cue <-> cue-option
      weights[1+cc,1+cues*2+cc] <- weights[1+cues*2+cc,1+cc] <- .1
      # cue-option <-> option
      weights[d,1+cues*2+cc] <- weights[1+cues*2+cc,d] <- c2[cc]*.01
    }
  }
  # check: round(weights, 2)
  # heatmap(weights, NA, NA, symm=T)


  reset <- c(1, rep(0, d-1))
  if(is.null(start)){
    start <- reset
  }else{
    check_start(start, dim=d)
  }
  res <- pcs_matrix_cpp(weights, start, reset, decay,
                        maxiter = maxiter, stability=stability,
                        convergence=convergence, full=TRUE)

  rownames(res$activation) <- colnames(res$process) <- names
  act <- res$activation
  res$choice = which.max(act[(d-1):d])
  res$prob.option1 = luce_choice(act[-1:0+nrow(act)])[1]


  sel.oc <- 1+cues+1:(2*cues)
  sel.hidden <- !c(is.na(c1),is.na(c2)) & c(c1 == 0, c2 == 0)
  act.co <- act[sel.oc][sel.hidden]
  res$psearch <- luce_choice(act.co, lambda=lambda)
  names(res$psearch) <- names[sel.oc][sel.hidden]
  res$search <- names(res$psearch)[which.max(act.co)]

  res[c("choice","iterations","search","energy","prob.option1","psearch",
        "activation","process","weights","convergence")]
}

