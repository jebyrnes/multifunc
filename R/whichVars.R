#' @title whichVars
#'
#' @description
#' \code{whichVars} takes a data frame and the names of a set of columns
#' and returns the names of those columns that do not have an excessive fraction
#' of NA values
#'
#' @details This is a helper function for data processing.
#'
#' @author Jarrett Byrnes.
#' @param a.df A data frame
#' @param vars The names of the columns that contain data of interest
#' @param thresh The fraction of NA values in a column that is acceptable
#'
#'
#' @export
#' @return A vector of column names
#'
#' @examples
#' data(all_biodepth)
#' allVars <- qw(biomassY3, root3, N.g.m2, light3, N.Soil, wood3, cotton3)
#'
#' germany <- subset(all_biodepth, all_biodepth$location == "Germany")
#'
#' vars <- whichVars(germany, allVars)
whichVars <- function(a.df, vars = NA, thresh = 2 / 3) {
  if (is.na(vars[1])) stop("No column names supplied.")
  tot <- nrow(a.df)

  # figure out which vars we can actually use
  usevar <- sapply(vars, function(x) sum(is.na(a.df[[x]])) / tot > thresh)
  vars <- vars[!usevar]

  vars
}
