#' @title filterOverData
#'
#' @description
#' \code{filterOverData} filters qualitative effects of species
#' to function by sign.
#'
#' @details Takes a matrix of functions and effects
#' of species -  1's and -1's, s - and filters out
#' only the sign of contributions desired.  Typically used by other functions in the package.
#'
#' @author Jarrett Byrnes.
#' @param overData Matrix of functions and which species affect them from \code{getRedundancy}.
#' @param type Are the kinds of effects we're looking at "positive", "negative" or "all".
#'
#' @export
#' @return Returns a filtered matrix.
#'
#' @examples
#' data(all_biodepth)
#' allVars <- qw(biomassY3, root3, N.g.m2, light3, N.Soil, wood3, cotton3)
#'
#' germany <- subset(all_biodepth, all_biodepth$location == "Germany")
#'
#' vars <- whichVars(germany, allVars)
#' species <- relevantSp(germany, 26:ncol(germany))
#'
#' # re-normalize N.Soil so that everything is on the same
#' # sign-scale (e.g. the maximum level of a function is the
#' # "best" function)
#' germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)
#'
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#' redund <- getRedundancy(vars, species, germany)
#'
#' filterOverData(redund, type = "positive")
#'
#'
#' #########
#' # filterOverData takes a matrix of 1s, 0s, and -1s
#' # and filters it so that only the positive, negative, or both contributions
#' # are 1 for later overlap function usage
#' #########
filterOverData <- function(overData, type = "positive") {
  # format the data properly given the type of analysis being done
  neg <- which(overData < 0, arr.ind = TRUE)
  pos <- which(overData > 0, arr.ind = TRUE)

  if (type == "positive" | type == "all") overData[neg] <- 0 
  if (type == "negative") {
    overData[pos] <- 0
    overData[neg] <- 1
  }
  overData
}
