#' @title getFuncMaxed
#'
#' @description
#' \code{getFuncMaxed} the number of functions greater than or equal to a single threshold in one experimental unit
#'
#' @details Create a data frame that has the value of number or proportion of functions
#' greater than a single threshold.
#'
#' @author Jarrett Byrnes.
#' @param adf A data frame with functions.
#' @param vars The column names of the functions to be assessed.
#' @param thresh The threshold value to assess.
#' @param proportion Whether the output will be returned as a proportion of all functions.  Defaults to \code{FALSE}.
#' @param prepend Additional columns that will be imported from the data for the returned data frame.
#' @param maxN As a 'maximum' value can be subject to outliers, etc., what number of the highest data points
#' for a function will be used to calculate the value against which thresholds will be judged.  E.g., if maxN=1
#' then all thresholds are proportions of the largest value measured for a function.  If maxN=8, then it's the
#' proportion of the mean of the highest 8 measurements.
#'
#' @export
#' @return Returns a data frame of number or fraction of functions greater than or equal to the selected thresholds in each plot.
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
#' germanyThresh <- getFuncMaxed(germany, vars, 
#'                               thresh = 0.5, 
#'                               prepend = c("plot", "Diversity"), 
#'                               maxN = 7)
#'
#'
#' # A function that will return a data frame with the first several columns
#' # being information the user wants for identification purposes (prepend)
#' # which defaults to Diversity and the final column the number of columns
#' # which pass a predefined threshold, defined as some proportion of the maximim
#' # observed for each column.  vars=the names of the vars being specified
#' # thresh is the threshold, between 0 and 1, of proportion of the max that needs
#' # to be passed to be counted.
#'
#' # changelog
#' # 2014-03-24 Fixed -1 error in getMaxValue
#' # 2015-06-24 Fixed column name from prepend error https://github.com/jebyrnes/multifunc/issues/1
#' # 2022-04-14 Updated to use dplyr
getFuncMaxed <- function(adf, vars = NA, thresh = 0.7, proportion = FALSE, 
                         prepend = "Diversity", maxN = 1) {
  if (is.na(vars)[1]) stop("You need to specify some response variable names")

  # which are the relevant variables
  vars <- whichVars(adf, vars)

  # scan across all functions, see which are >= a threshold
  # funcMaxed<-rowSums(colwise(function(x) x >= (thresh*max(x, na.rm = TRUE)))(adf[,which(names(adf)%in%vars)]))
  getMaxValue <- function(x) {
    l <- length(x)
    mean(sort(x, na.last = FALSE)[l:(l - maxN + 1)], na.rm = TRUE)
  }

  # old
  # funcMaxed <- rowSums(colwise(function(x) x >= thresh*getMaxValue(x))(adf[,which(names(adf)%in%vars)]))

  ret <- adf %>%
    # get functions over threshold
    dplyr::mutate(
      thresholds = thresh,
      dplyr::across(vars,
        ~ .x >= thresh * max(.x, na.rm = TRUE),
        .names = "{.col}_OVER_THRESH"
      ),
      nFunc = length(vars)
    ) %>%
    # sum over functions
    dplyr::rowwise() %>%
    dplyr::mutate(
      funcMaxed = sum(dplyr::c_across(paste0(vars, "_OVER_THRESH")))
    ) %>%
    dplyr::ungroup() %>%
    # cleanup
    dplyr::select(-dplyr::contains("_OVER_THRESH"))

  if (proportion) {
    ret <- ret %>% dplyr::mutate(funcMaxed = funcMaxed / length(vars))
  }

  ret
}
