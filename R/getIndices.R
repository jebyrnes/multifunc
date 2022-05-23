#' @title getIndices
#'
#' @description
#' \code{getIndices} Generates a variety of indices describing multifunctionality based on
#' the number of functions greater than a threshold for many different threshold and coefficients describing
#' the relationship between diversity and number of functoins greater than a threshold.
#'
#' @details See Byrnes et al. In Review.
#'
#' @author Jarrett Byrnes.
#' @param slopedata A data frame with slopes of the relationship between diversity and number of functions greather than or equal to a threshold
#' from \code{getCoefTab}.
#' @param threshdata A data frame with the number of functions greater than a threshold for each plot at each threshold from \code{getFuncsMaxed}.
#' @param eqn The formula used for fitting the models in slopedata.
#' @param fun The function used to refit the threshold data at key points to get intercepts, etc., that are needed for the table.
#' @param divvar The name of the variable that has the measure of diversity or other driver in the threshdata data frame.
#' @param groupVar The name of a variable by which data is grouped in the threshdata data frame. Typically "thresholds" from \code{getFuncsMaxed}.
#' @param showNfunc Show the functions at Tmin, Tmax, and Tmde. Defaults to TRUE.
#'
#'
#' @export
#' @return A data frame of indices
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
#' germanyThresh <- getFuncsMaxed(germany, vars,
#'   threshmin = 0.05,
#'   threshmax = 0.99, prepend = c("plot", "Diversity"), maxN = 7
#' )
#'
#' germanyLinearSlopes <- getCoefTab(funcMaxed ~ Diversity,
#'   data = germanyThresh,
#'   coefVar = "Diversity", family = quasipoisson(link = "identity")
#' )
#'
#' getIndices(germanyLinearSlopes, germanyThresh, funcMaxed ~ Diversity)
#' 
#' }
getIndices <- function(slopedata, threshdata, eqn, fun = stats::glm,
                       divvar = "Diversity", groupVar = "thresholds",
                       showNfunc = TRUE) {
  Smax <- max(threshdata[[divvar]], na.rm = TRUE)
  Smin <- min(threshdata[[divvar]], na.rm = TRUE)
  tdata <- get_t_indices(slopedata)

  predFun <- function(thresh, S) {
    if (is.na(thresh)) {
      return(NA)
    }
    subdata <- threshdata[which(threshdata[[groupVar]] == thresh), ]
    fit <- getFit(eqn, fun, subdata)
    stats::predict(fit, newdata = subdata[which(threshdata[[divvar]] == S), ], type = "response")[1]
  }

  Mmin <- predFun(tdata$Tmin, Smax)
  Mmax <- predFun(tdata$Tmax, Smax)
  Mmde <- predFun(tdata$Tmde, Smax)
  Rmde.linear <- slopedata$estimate[which(slopedata[[groupVar]] == tdata$Tmde)]
  Pmde.linear <- slopedata$estimate[which(slopedata[[groupVar]] == tdata$Tmde)] / (threshdata$nFunc[1] / Smax)
  nFunc <- threshdata$nFunc[1]

  if (is.na(tdata$Tmde)) {
    warning("None of these coefficients are different from 0, hence indices have little meaning")
    Rmde.linear <- NA
    Pmde.linear <- NA
  }

  return(data.frame(
    Tmin = tdata$Tmin,
    Tmax = tdata$Tmax,
    Tmde = tdata$Tmde,
    Rmde.linear = Rmde.linear,
    Pmde.linear = Pmde.linear,
    Mmin = Mmin,
    Mmax = Mmax,
    Mmde = Mmde,
    nFunc = nFunc
  ))
}


getFit <- function(eqn, fun = stats::glm, adf, ...) {
  aFit <- try(fun(eqn, data = adf, ...))

  # if there was a problem, catch it and just return NAs for this coefficient
  if ("try-error" %in% class(aFit)) {
    return(NA)
  }

  if ("glm" %in% class(aFit)) {
    if (!aFit$converged) {
      return(NA)
    }
  }

  return(aFit)
}


getIndices_nothresh <- function(eqn, fun = stats::glm, threshdata, divvar = "Diversity", groupVar = "thresholds", coefVar, ...) {
  slopedata <- getCoefTab(eqn, fun = fun, threshdata, groupVar = "thresholds", coefVar, ...)
  getIndices(slopedata, threshdata, eqn, fun, divvar, groupVar)
}



get_t_indices <- function(adf) {
  minSlope <- adf$estimate - 2 * adf[["std.error"]]
  maxSlope <- adf$estimate + 2 * adf[["std.error"]]
  sloperng <- rbind(minSlope, maxSlope)

  # which thresholds have non-zero slopes
  not0 <- sort(c(which(colSums(sloperng > 0) == 2), which(colSums(sloperng > 0) == 0)))

  if (length(not0) != 0) {
    if (not0[1] - 1 == 0) {
      Tmin <- NA
    } else {
      Tmin <- adf$thresholds[not0[1] - 1]
    }

    if (range(not0)[2] + 1 > length(adf$thresholds)) {
      Tmax <- NA
    } else {
      Tmax <- adf$thresholds[range(not0)[2] + 1]
    }
  } else {
    Tmin <- NA
    Tmax <- NA
  }

  Tmde <- adf$thresholds[which(adf$estimate == max(abs(adf$estimate), na.rm = TRUE))][1] # the [1] is in case there are multiple identical peaks
  return(data.frame(Tmin = Tmin, Tmax = Tmax, Tmde = Tmde))
}
