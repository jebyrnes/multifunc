#' @title getFuncsMaxed
#'
#' @description
#' \code{getFuncsMaxed} the number of functions greater than or equal to a wide variety of thresholds in each experimental unit
#'
#' @details Create a data frame that has the value of number or proportion of functions
#' greater than a threshold for several different thresholds at the plot.
#'
#' @author Jarrett Byrnes.
#' @param adf A data frame with functions.
#' @param vars The column names of the functions to be assessed.
#' @param threshmin The lowest threshold value to assess.
#' @param threshmax The highest threshold value to assess
#' @param threshstep The incremental steps between lowest and highest thresholds to be assessed.  See \code{seq}.
#' @param proportion Whether the output will be returned as a porportion of all functions.  Defaults to \code{FALSE}.
#' @param prepend Additional columns that will be imported from the data for the returned data frame.
#' @param maxN As a 'maximum' value can be subject to outliers, etc., what number of the highest data points
#' for a function will be used to calculate the value against which thresholds will be judged.  E.g., if maxN=1
#' then all thresholds are porportions of the largest value measured for a function.  If maxN=8, then it's the
#' porportion of the mean of the highest 8 measurements.
#'
#'
#' @export
#' @return Returns a data frame of number or fraction of functions greater than or equal to the selected thresholds in each plot over all thresholds within the relevant range.
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
#' germany$N.Soil <- -1 * germany$N.Soil + 
#'                   max(germany$N.Soil, na.rm = TRUE)
#'
#' germanyThresh <- getFuncsMaxed(germany, vars,
#'   threshmin = 0.50,
#'   threshmax = 0.60, prepend = c("plot", "Diversity"), maxN = 7
#' )
#'
#'
####
# Create a data frame that has the value of proportion of functions 
# greather than a threshold for several different thresholds at the plot 
# later
####
getFuncsMaxed <- function(adf, vars = NA,
                          threshmin = 0.05, threshmax = 0.99,
                          threshstep = 0.01, proportion = FALSE,
                          prepend = "Diversity", maxN = 1) {
  thresh_vec <- seq(threshmin, threshmax, threshstep)


  ret <- purrr::map_dfr(
    thresh_vec,
    ~ getFuncMaxed(adf,
      vars = vars,
      thresh = .x, # the threshold
      proportion = proportion,
      maxN = maxN,
      prepend = c(prepend)
    )
  )

  ret
}
