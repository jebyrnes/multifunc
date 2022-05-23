#' @title standardizeUnitScale
#'
#' @description
#' \code{standardizeUnitScale} standardized a variable so its maximum is 1
#'
#' @details Takes a vector and then divides it by a maximum value.
#'
#' @author Jarrett Byrnes.
#' @param afun A vector of measurements of a function.
#' @param min0 Must a minimum value be greater than or equal to 0?  Defaults to TRUE.
#' @param maxValue The maximum valye by which the vector will be standardized.  Defaults to
#' the vector's maximum.
#'
#' @export
#' @return Returns a standardized vector.
#'


# standardizeFunction standardizes a function so that it's values are between 0 and 1
# if the value of a function is <0, assumes that that is the lowest level possible
standardizeUnitScale <- function(afun, min0 = TRUE, maxValue = max(afun, na.rm = TRUE)) {

  # add back minimum of negative values in function so you can rescale properly
  if (min0 && min(afun, na.rm = TRUE) < 0) afun <- afun + abs(min(afun, na.rm = TRUE))

  afun / maxValue
}
