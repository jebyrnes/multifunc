#' @title divNeeded
#'
#' @description
#' \code{divNeeded} Determines, for every combination of functions, how many species
#' influence those functions.
#'
#' @details Iterates over all possible combinations of functions.  Checks the matrix of which species
#' have positive, negative, or both influences on those functions.  Tally's total number of species
#' that have an effect on those functions
#'
#' @author Jarrett Byrnes.
#' @param overData Matrix of functions and which species affect them from \code{getRedundancy}.
#' @param type Are the kinds of effects we're looking at "positive", "negative" or "all".
#'
#'
#' @export
#' @return Returns a data frame of all combinations and how many species are needed to
#' influence all of them.
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
#' # same sign-scale (e.g. the maximum level of a
#' # function is the "best" function)
#' germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)
#'
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#' redund <- getRedundancy(vars, species, germany)
#'
#' posCurve <- divNeeded(redund, type = "positive")


divNeeded <- function(overData, type = "positive") {
  overData <- filterOverData(overData, type = type)

  # go over all possible combinations of functions and see how many species are needed
  dn <- sapply(1:nrow(overData), function(m) { # sapply for num. of functions
    if (m == 1) {
      rowSums(overData) # species case if m=1 because combn borks on it
    } else {
      utils::combn(1:nrow(overData), m, function(x) length(which(colSums(overData[x, ]) > 0))) # combn to get each individual combo
    }
  })

  ret <- data.frame(nfunc = 1, div = dn[[1]])
  for (i in 2:length(dn)) ret <- rbind(ret, data.frame(nfunc = i, div = dn[[i]]))
  return(ret)
}
