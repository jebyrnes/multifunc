#' @title getCoefTab
#'
#' @description
#' \code{getCoefTab} extract the effect of diversity on number of functions greater than
#' a threshold
#'
#' @details getCoefTab Takes a statistical model and plot level data with the number of functions
#' greater than a threshold at multiple different thresholds and returns the coefficient for the
#' effect of diversity at each threshold
#'
#' @author Jarrett Byrnes.
#' @param eqn The model to be fit at each threshold.
#' @param fun The fitting function.  Defaults to \code{glm}.
#' @param data A data frame containing the variables in the model to be fit.
#' @param groupVar Grouping variable.  Defaults to "thresholds" to fit the model at different
#' thresholds, but, other types of grouping are possible.
#' @param coefVar The name of the variable from the model whose coefficient we'll be extracting.
#' @param ... Other arguments to be supplied to the fitting function
#'
#'
#' @export
#' @return Returns a data frame of thresholds, coefficients, and their statistical properties.
#'
#' @examples
#'\donttest{
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
#' germanyThresh <- getFuncsMaxed(germany, vars,
#'   threshmin = 0.05,
#'   threshmax = 0.99, prepend = c("plot", "Diversity"), maxN = 7
#' )
#'
#' germanyLinearSlopes <- getCoefTab(funcMaxed ~ Diversity,
#'   data = germanyThresh, coefVar = "Diversity", family = quasipoisson(link = "identity")
#' )
#'
#'}


####
# A function that will take a data frame with a lot of different thresholds
# and at each threshold, fit a user specified statistical model, then pull out
# a coefficient of interest (e.g., Diversity) and it's information from a summar
# table.  This function can be used for glm and lm models, and potentially others
# with the proper specification.
#####
getCoefTab <- function(eqn,
                       fun = stats::glm,
                       data,
                       groupVar = "thresholds",
                       coefVar = NULL, ...) {


  get_model <- function(one_dat) {
    try(fun(eqn, data = one_dat, ...))
  }

  ret <- data %>%
    dplyr::group_by(dplyr::across(groupVar)) %>%
    tidyr::nest() %>%
    dplyr::mutate(mod = purrr::map(data, get_model)) %>%
    # get info on if things failed
    dplyr::mutate(
      err = purrr::map_lgl(mod, ~ "try-error" %in% class(.x)),
      err_conv = purrr::map_lgl(mod, ~ ifelse("glm" %in% class(.x),
        !.x$converged,
        FALSE
      )),
      mod = ifelse(err + err_conv > 0, NA, mod)
    )

  # get coefs
  ret <- ret %>%
    dplyr::mutate(coefs = purrr::map(mod, broom::tidy)) %>%
    dplyr::select(-data, -mod) %>%
    tidyr::unnest(coefs) %>%
    dplyr::ungroup()

  # filter out fit errors
  ret <- ret %>%
    dplyr::mutate(dplyr::across(estimate:p.value, ~ ifelse(err | err_conv, NA, .x))) %>%
    dplyr::select(-err, -err_conv)

  if (!is.null(coefVar)) {
    ret <- ret %>% dplyr::filter(term %in% coefVar)
  }


  return(ret)
}
