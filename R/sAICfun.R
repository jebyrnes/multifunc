#' @title sAICfun
#'
#' @description
#' \code{sAICfun} examines which species have an effect on which function using a stepwise AIC approach
#'
#' @details \code{sAICfun} takes a dataset, response, and function, and then uses a stepAIC approach
#' to determine the best model.  From that it extracts the species with a positive,
#' negative, and neutral effect on that function.
#'
#' @author Jarrett Byrnes.
#' @param response Name of the response column
#' @param species Vector of column names of species
#' @param data data frame with species presence/abscence of values of functions
#' @param positive.desired Is a positive effect the desired sign.  Defaults to TRUE
#' @param method Fitting function for statistical models.  Defaults to \code{lm}.
#' @param combine How are species combined in the model? Defaults to "+" for additive combinations.
#' @param ... Other arguments to be supplied to fitting function.
#'
#'
#' @export
#' @return Returns list of species with positive
#' negative or neutral contributions, the
#' relevant coefficient and effect matrices, and response name
#'
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
#' spList <- sAICfun("biomassY3", species, germany)
#' # " spList
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#'
#'
#' #########
#' # sAICfun takes a dataset, response, and function, and then uses a stepAIC approach
#' # to determine the best model.  From that it extracts the species with a positive,
#' # negative, and neutral effect on that function
#' #########
sAICfun <- function(response, species, data, positive.desired = TRUE, 
                    method = "lm", combine = "+", ...) {
  # first fit the model
  obj <- sAICFit(response, species, data, method, combine, ...)

  # now extract the important information about positive, negative, etc.

  # return that info in a list
  if (positive.desired) {
    pos.sp <- names(summary(obj)[[4]][, 4][(summary(obj)[[4]][, 1] > 0) & names(summary(obj)[[4]][, 4]) != "(Intercept)"])
    neg.sp <- names(summary(obj)[[4]][, 4][(summary(obj)[[4]][, 1] < 0) & names(summary(obj)[[4]][, 4]) != "(Intercept)"])
  } else {
    pos.sp <- names(summary(obj)[[4]][, 4][(summary(obj)[[4]][, 1] < 0) & names(summary(obj)[[4]][, 4]) != "(Intercept)"])
    neg.sp <- names(summary(obj)[[4]][, 4][(summary(obj)[[4]][, 1] > 0) & names(summary(obj)[[4]][, 4]) != "(Intercept)"])
  }
  neu.sp <- species[!(species %in% pos.sp) & !(species %in% neg.sp)]

  # make a vector of 1s and 0s
  effects <- rep(0, length(species))
  names(effects) <- species
  effects[which(names(effects) %in% pos.sp)] <- 1
  effects[which(names(effects) %in% neg.sp)] <- -1

  coefs <- c(effects, 0)
  names(coefs)[length(coefs)] <- "(Intercept)"
  coefs[match(names(stats::coef(obj)), names(coefs))] <- stats::coef(obj)

  return(list(pos.sp = pos.sp, neg.sp = neg.sp, neu.sp = neu.sp, functions = response, coefs = coefs, effects = effects))
}

#########
# sAICFit does the business of fitting a model using a stepAIC approach
#########
sAICFit <- function(response, species, data, method = "lm", combine = "+", ...) {
  f <- stats::as.formula(paste(response, "~", paste(species, collapse = "+")))
  fit <- eval(substitute(stats::lm(f, data = data, ...)))
  obj <- MASS::stepAIC(fit, trace = 0)
  obj
}
