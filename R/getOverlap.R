#' @title getOverlap
#'
#' @description
#' \code{getOverlap} goes through all m-wise combinations of species
#' and returns the amount of overlap between species in functions they perform
#' for each combination
#'
#' @details getOverlap takes a matrix of 1s and -1s, and depending on whether we're
#' interested in positive, negative, or both types of interactions looks for the
#' m-wise overlap between species and returns the overlap index for each combination
#'
#' @author Jarrett Byrnes.
#' @param overData Matrix of functions and which species affect them from \code{getRedundancy}.
#' @param m Number of functions. Defaults to 2.
#' @param type Are the kinds of effects we're looking at "positive", "negative" or "all".
#' @param index Type of overlap index to be used. Defaults to "sorenson" but currently incorporates
#' "mountford" and "jaccard" as well.
#' @param denom Should the denominator be "all" species or just the "set" of species with the types
#' of interactions being considered? Defaults to "set".
#'
#'
#' @export
#' @return Returns a vector of overlap indices.
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
#' # re-normalize N.Soil so that everything is on the
#' # same sign-scale (e.g. the maximum level of a function is the "best" function)
#' germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)
#'
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#' redund <- getRedundancy(vars, species, germany)
#'
#' getOverlap(redund, m = 2)
#' getOverlap(redund, m = 2, index = "jaccard")
#' getOverlap(redund, m = 2, index = "mountford")
#'
#' #########
#' # getOverlap takes a matrix of 1s and -1s, and depending on whether we're
#' # interested in positive, negative, or both types of interactions looks for the
#' # m-wise overlap
#' #########
getOverlap <- function(overData, m = 2, type = "positive", index = "sorensen", denom = "set") {
  if (denom == "all") denomMat <- abs(overData)

  overData <- filterOverData(overData, type = type)

  if (denom != "all") denomMat <- overData

  overlap <- NA

  # jaccard of sorensen overlap?  default is sorensen as it is what was used in Hector and Bagchi
  if (index == "jaccard" || index == "Jaccard") {
    overlap <- utils::combn(1:nrow(overData), m, function(x) length(which(colSums(overData[x, ]) == m)) / length(which(colSums(denomMat[x, ]) > 0)))
  }

  # from woulda 1981 Oecologia, mountford 1962
  if (index == "mountford" || index == "Mountford") {
    overlap <- utils::combn(1:nrow(overData), m, function(x) {
      j <- length(which(colSums(overData[x, ]) == m))
      a_b <- sum(rowSums(denomMat[x, ]))
      ab <- prod(rowSums(denomMat[x, ]))
      overlap <- m * j / (2 * ab - a_b * j)
    })
  }

  # non-ascii characters below, but foreign users may use the \u00f8
  if (index == "sorensen" || index == "Sorensen" || index == "s\u00F8rensen" || index == "S\u00F8rensen") {
    overlap <- utils::combn(1:nrow(overData), m, function(x) length(which(colSums(overData[x, ]) == m)) / (sum(rowSums(denomMat[x, ])) / m))
  }

  if (is.na(overlap[1])) stop("Index should be jaccard, sorensen, or mountford")

  overlap
}
