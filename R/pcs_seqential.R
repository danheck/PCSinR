#' PCS-DM+S Sequential Cue Search for Multiattribute Decisions
#'
#' Starts with a partially known cue matrix and searches missing cue values sequentially until all cues are known.
#'
#' @param c1 positive/negative/hidden/non-available cue values for Option 1 (+/-1 for positive/negative cue values; 0 for hidden cue values; \code{NA} for nonavailable information)
#' @param c2 positive/negative/hidden/non-available cue values for Option 2
#' @param t1 true cue values for Option 1 (relevant for hidden cues that are opened sequentially)
#' @param t2 true cue values for Option 2
#' @param restart whether to reset the activation of the nodes to zero after novel information becomes (new cue values) available
#' @inheritParams pcs_multi
#' @inheritParams pcs_matrix
#' @inheritParams pcs_search
#' @author Daniel Heck
#' @examples
#' ###### each step starts with the most recent activation levels
#' seq <- pcs_sequential(c1=c(1,0,0), c2=c(0,NA,1),
#'                       t1=c(1,-1,-1), t2=c(-1,NA,1),
#'                       v=c(.8,.7,.6), restart=FALSE)
#' sapply(seq, function(ss) ss[1:5])
#'
#' ###### each step resets the activation pattern to zero
#' seq2 <- pcs_sequential(c1=c(1,0,0), c2=c(0,NA,1),
#'                        t1=c(1,-1,-1), t2=c(-1,NA,1),
#'                        v=c(.8,.7,.6), restart=TRUE)
#' sapply(seq2, function(ss) ss[1:5])
#' @export
pcs_sequential <- function(c1, c2,
                       v,
                       t1, t2,
                       restart=FALSE,
                       bottomup=TRUE,
                       p=1.9,
                       decay=.1,
                       maxiter=1000,
                       stability=10^-6,
                       convergence="floor",
                       lambda =2.9) {
  cues <- length(c1)
  check_length(list(c1,c2,v,t1,t2))

  if(all(c1 != 0, na.rm = TRUE) & all(c2 != 0, na.rm = TRUE))
    stop("Available cue values c1 and c2 do not have missings!")
  if(any( c1!=0 & c1!=t1 , na.rm = TRUE) | any(c2!=0 & c2!=t2, na.rm = TRUE))
    stop("Available (c1,c2) and true (t1,t2) cue values must match!")

  res <- list()
  cnt <- 1
  while(any(c1 ==0, na.rm = TRUE) | any(c2 == 0, na.rm = TRUE)){
    if(cnt == 1){
      start <- c(1, rep(0, cues+2*cues+2))
    }else if(!restart){
      start <- res[[cnt-1]]$activation
    }
    res[[cnt]] <- pcs_search(c1=c1, c2=c2, v=v,
                             start=start, bottomup=bottomup,
                             p=p, decay=decay, maxiter=maxiter,
                             stability=stability, convergence=convergence,
                             lambda=lambda)
    search <- res[[cnt]]$search
    if(grepl("opt1", search)){
      # update available cues for Option 1
      sel <- as.numeric(gsub("opt1cue","",search))
      c1[sel] <- t1[sel]
    }else{
      sel <- as.numeric(gsub("opt2cue","",search))
      c2[sel] <- t2[sel]
    }
    cnt <- cnt + 1
  }
  names(res) <- paste0("step", seq_along(res))

  res
}
