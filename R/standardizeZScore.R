#' @title standardizeZScore
#'
#' @description
#' \code{standardizeZScore} Z-standardizes a vector.
#'
#' @details Centers a vector and divides it by its standard deviation.
#'
#' @author Jarrett Byrnes.
#' @param afun A vector of measurements of a function.

#'
#' @export
#' @return Returns a z-standardized vector.
#'


standardizeZScore <- function(afun) 
  (afun - mean(afun, na.rm = FALSE)) / stats::sd(afun, na.rm = TRUE)
