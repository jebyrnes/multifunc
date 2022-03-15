#' @title getRedundancy
#'
#' @description
#' \code{getRedundancy} examines which species have an effect on which function
#'
#' @details getRedundancy takes a matrix of 1s,0s, and -1s, and depending on whether we're
#' interested in positive, negative, or both types of interactions looks for the
#' m-wise overlap between species and returns the overlap index for each combination. For
#' species whose effect is not different from 0 at the alpha=0.05 level, a 0 is returned.
#'
#' @author Jarrett Byrnes.
#' @param vars Vector of column names of functions
#' @param species Vector of column names of species
#' @param data data frame with species presence/absence of values of functions
#' @param negVars Vector of names of species for which a negative coefficient is actually a positive effect.
#' @param method Fitting function for statistical models.  Defaults to \code{lm}.
#' @param combine How are species combined in the model? Defaults to "+" for additive combinations.
#' @param output Will the output be sign of effect or "coefficient".  Defaults to "effect"
#' @param ... Other arguments to be supplied to fitting function.
#'
#'
#' @export
#' @return Returns a matrix of functions and the effect of species on each. 1s, -1s, and 0s for "effect" or coefficients.
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
#' res.list <- lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list) <- vars
#'
#' getRedundancy(vars, species, germany)
#' getRedundancy(vars, species, germany, output = "coef")
#'
#'
#'
#' #########
#' # takes a vector of responses, the species that may cause them
#' # and returns a table of 1s, -1s, and 0s with regards to the kind of effect
#' # or a coefficient table, if asked for.  Arugments can take the form of the fitting function
#' # how variables are combined, and additional arguments to the fitting function
#' #########
getRedundancy <- function(vars, species, data, negVars = NA, method = "lm", combine = "+", output = "effect", ...) {
  res.list <- lapply(vars, function(x) {
    positive.desired <- TRUE
    if (x %in% negVars) positive.desired <- F
    sAICfun(response = x, species = species, data = data, positive.desired, method = method, combine = combine, ...) # used to be method[idx]
  })

  # what if they want the coefficients
  if (output == "coef") {
    # ret<-plyr::ldply(res.list, function(x) x$coefs)
    ret <- lapply(res.list, function(x) x$coefs)
    ret <- do.call(rbind, ret) %>% as.data.frame()
  } else {
    # the default of returning the 1s, -1s, and 0s
    # ret<-ldply(res.list, function(x) x$effects)
    ret <- lapply(res.list, function(x) x$effects)
    ret <- do.call(rbind, ret) %>% as.data.frame()
  }

  rownames(ret) <- vars
  return(ret)
}

#########
# sAICfun takes a dataset, response, and function, and then uses a stepAIC approach
# to determine the best model.  From that it extracts the species with a positive,
# negative, and neutral effect on that function
#########
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
  coefs[match(names(coef(obj)), names(coefs))] <- coef(obj)

  return(list(pos.sp = pos.sp, neg.sp = neg.sp, neu.sp = neu.sp, functions = response, coefs = coefs, effects = effects))
}

#########
# sAICFit does the business of fitting a model using a stepAIC approach
#########
sAICFit <- function(response, species, data, method = "lm", combine = "+", ...) {
  f <- as.formula(paste(response, "~", paste(species, collapse = "+")))
  fit <- eval(substitute(lm(f, data = data, ...)))
  obj <- MASS::stepAIC(fit, trace = 0)
  obj
}
