#' @title getStdAndMeanFunctions
#'
#' @description
#' \code{getStdAndMeanFunctions} creates an average function multifunctionality index.
#'
#' @details iterates over all functions and
#' standardizes them between 0 and 1.  Then it creates an averaged
#' multifunctionality index by averaging over all standardized functions
#'
#' @author Jarrett Byrnes.
#' @param data A data frame with functions.
#' @param vars The column names of the functions to be assessed.
#' @param standardizeFunction A function to standardize each individual
#' function to the same scale, such as \code{standardizeUnitScale} or
#' \code{standardizeZScore}
#'
#' @export
#' @return Returns a data frame with
#' standardized values for each function and an averaged index.
#'
#' @examples
#' data(all_biodepth)
#' allVars <- qw(biomassY3, root3, N.g.m2, light3, N.Soil, wood3, cotton3)
#'
#' germany <- subset(all_biodepth, all_biodepth$location == "Germany")
#'
#' vars <- whichVars(germany, allVars)
#'
#' # re-normalize N.Soil so that everything is on the same
#' # sign-scale (e.g. the maximum level of a function is
#' # the "best" function)
#' germany$N.Soil <- -1 * germany$N.Soil + max(germany$N.Soil, na.rm = TRUE)
#'
#' germany <- cbind(germany, getStdAndMeanFunctions(germany, vars))
getStdAndMeanFunctions <- function(data, vars, standardizeFunction = standardizeUnitScale) {
  ret <- dplyr::mutate(data, dplyr::across(vars, standardizeUnitScale, .names = "{.col}.std")) %>%
    dplyr::select(paste0(vars, ".std"))

  ret$meanFunction <- rowSums(ret) / ncol(ret)

  return(ret)
}
