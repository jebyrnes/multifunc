#' @title stdEffects
#'
#' @description
#' \code{stdEffects} obtains the standardized effect of each species on each function
#'
#' @details stdEffects takes a matrix of coefficients for relationships
#' between species and functions, the data frame used to generate those coefficients
#' and the names of species and function, and then it calculates standardized coefficients
#' using std coef = b *sx/sy
#'
#' @author Jarrett Byrnes.
#' @param cmat Matrix of coefficients of species effects on functions from \code{getRedundancy} with output="coef".
#' @param adf Data frame with plot level data for species and functions.
#' @param vars Names of columns with data for functions in adf.
#' @param species Names of columns with data for species in adf.
#'
#'
#' @export
#' @return Returns a matrix of standardized coefficients.
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
#' coefs <- getRedundancy(vars, species, germany, output = "coef")
#' stdCoefs <- stdEffects(coefs, germany, vars, species)
#'
#' #########
#' # A function that uses the coefficient matrix and information from the
#' # data to calculate standardized effects of species using the method
#' # std coef = b *sx/sy
#' #########
stdEffects <- function(cmat, adf, vars, species) {
  sFunc <- sapply(adf[, which(names(adf) %in% vars)], 
                  function(x) stats::sd(x, na.rm = TRUE))
  sTrt <- sapply(adf[, which(names(adf) %in% species)], 
                 function(x) stats::sd(x, na.rm = TRUE))
  sFunc_sTrt <- t(sTrt %*% t(1 / sFunc))

  # get the order right
  sFunc_sTrt <- sFunc_sTrt[match(rownames(sFunc_sTrt), rownames(cmat)), ]

  # return the standardized coefs
  cmat[, -ncol(cmat)] * sFunc_sTrt
}
