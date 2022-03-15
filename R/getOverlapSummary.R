#' @title getOverlapSummary
#'
#' @description
#' \code{getOverlapSummary} summarizes the number of species necessary for each function
#' including means, SDs, and other metrics
#'
#' @details getOverlapSummary takes a matrix of 1s and -1s, and depending on whether we're
#' interested in positive, negative, or both types of interactions looks for the
#' m-wise overlap between species and then reports summary metrics of mean overlap,
#' SD, and number of combinations
#'
#' @author Jarrett Byrnes.
#' @param overData Matrix of functions and which species affect them from \code{getRedundancy}.
#' @param m Number of functions. Defaults to 2.
#' @param type Are the kinds of effects we're looking at "positive", "negative" or "all".
#' @param index Type of overlap index to be used by \code{getOverlap}.
#' @param denom Type of denominator to be used by \code{getOverlap}.
#'
#'
#' @export
#' @return Returns a data frame of the mean overlap, SD, and number of possible combinations.
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
#' # sign-scale (e.g. the maximum level of a function is
#' # the "best" function)
#' germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)
#'
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#' redund <- getRedundancy(vars, species, germany)
#'
#' getOverlapSummary(redund, m = 2)
#'
#'
#' #########
#' # getOverlapSummary takes a matrix of 1s and -1s, and depending on whether we're
#' # interested in positive, negative, or both types of interactions looks for the
#' # m-wise overlap and then reports summary metrics of mean overlap, SD, and number of combinations
#' #########
getOverlapSummary <- function(overData, m = 2, type = "positive", index = "sorensen", denom = "set") {
  overlap <- getOverlap(overData, m = m, type = type, index = index, denom = denom)
  return(c(
    meanOverlap = mean(overlap),
    sdOverlap = stats::sd(overlap),
    n = length(overlap)
  ))
}
