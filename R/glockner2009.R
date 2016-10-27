#' Data Set: Multiattributive Decisions (Gloeckner, 2009)
#'
#' Dataset with multiattributive decisions (Glöckner, 2009).
#' @details Typical decision experiment.
#'
#' @format A data frame 6 variables:
#' \describe{
#'   \item{\code{PARTICIPANT}}{Participant number}
#'   \item{\code{DEC}}{Trial number}
#'   \item{\code{TYPE}}{Item type (i.e., specific combination of cue values and validities)}
#'   \item{\code{ACHOICES}}{1 = Option A chosen, 0 = Option B chosen}
#'   \item{\code{DECTIMES}}{Decision times}
#'   \item{\code{CONFJUDGMENTS}}{Confidence judgments (between 0 and 100)}
#' }
#' @references
#' Glöckner, A. (2009). Investigating intuitive and deliberate processes statistically: The multiple-measure maximum likelihood strategy classification method. Judgment and Decision Making, 4, 186-199.
#' @examples
#' head(glockner2009)
#'
#' \dontrun{
#' # MM-ML:
#' mmml(data = glockner2009, strategy = "TTB",id=1,
#'      choice = c(10, 10, 10, 10, 10, 10),
#'      time = c(0.167, 0.167, 0.167, 0.167, 0.167, 0.833),
#'      conf = c(0.167, 0.167, 0.167, 0.167, 0.167, 0.833))
#' }
"glockner2009"
