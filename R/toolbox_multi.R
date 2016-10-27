#' Adaptive-Toolbox Strategies for Multiattributive Decisions
#'
#' For modeling strategies such as take-the-best (TTB), equal-weight (EQW), and weighted-additive (WADD).
#' @inheritParams pcs_multi
#' @param error fixed error probability of applying a strategy (usefull for maximum-likelihood estimation)
#' @seealso For PCS-DM predictions see \link{pcs_multi}
#' @examples
#' toolbox_multi(c1 = c(1,-1,1,1), c2=c(-1,1,1,1), v=c(.9,.8,.7,.6))
#' @export
toolbox_multi <- function(c1, c2, v, error=0.00){

  if(error <0 | error>=.5)
    stop("Error probability 'error' should be in [0,.5)")
  check_length(list(c1,c2,v))
  check_val(v)

  mat <- cbind(c1=c1, c2=c2)
  rownames(mat) <- v
  mat <- mat[order(v, decreasing=TRUE),]

  # TTB
  discr <- (mat[,1] != mat[,2]) & (mat[,1]==1 | mat[,2]==1)
   if(sum(discr) == 0){
    ttb <- .50
  }else{
    ttb <- ifelse(which.max(mat[discr,,drop=FALSE][1,])==1,1-error, error)
    names(ttb) <- NULL
  }

  # EQW
  p1 <- sum(c1 == 1)
  p2 <- sum(c2 == 1)
  eqw <- ifelse(p1>p2, 1-error, ifelse(p1 == p2, .5, error))

  # WADDuncorr
  diff <- sum(v*c1) - sum(v*c2)
  waddu <- ifelse(diff>0, 1-error, ifelse(diff==0, .5, error))

  # WADDcorr
  diff <- sum((v-.5)*c1) - sum((v-.5)*c2)
  waddc <- ifelse(diff>0, 1-error, ifelse(diff==0, .5, error))

  # WADDcorr
  diff <- sum((v-.5)*c1) - sum((v-.5)*c2)
  waddc <- ifelse(diff>0, 1-error, ifelse(diff==0, .5, error))

  # WADDbayes
  v1 <- ifelse(c1==0, .5, ifelse(c1==1, v, 1-v))
  v2 <- ifelse(c2==0, .5, ifelse(c2==1, v, 1-v))
  diff <- sum(log(v1/(1-v1))) - sum(log(v2/(1-v2)))
  waddb <- ifelse(diff>0, 1-error, ifelse(diff==0, .5, error))

  list(prob.option1 = c(TTB=ttb, EQW=eqw,
                        WADDuncorr=waddu, WADDcorr=waddc,
                        WADDbayes=waddb, GUESS=.5))
}

