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
#' data(all_biodepth)
#' allVars<-qw(biomassY3, root3, N.g.m2,  light3, N.Soil, wood3, cotton3)
#'
#' germany<-subset(all_biodepth, all_biodepth$location=="Germany")
#'
#' vars<-whichVars(germany, allVars)
#'
#' #re-normalize N.Soil so that everything is on the same 
#' #sign-scale (e.g. the maximum level of a function is 
#' #the "best" function)
#' germany$N.Soil<- -1*germany$N.Soil +max(germany$N.Soil, na.rm=TRUE)
#' 
#' germanyThresh<-getFuncsMaxed(germany, vars, threshmin=0.05, 
#'    threshmax=0.99, prepend=c("plot","Diversity"), maxN=7)
#' 
#' germanyLinearSlopes<-getCoefTab(funcMaxed ~ Diversity, 
#'    data=germanyThresh, coefVar="Diversity", family=quasipoisson(link="identity"))



####
# A function that will take a data frame with a lot of different thresholds
# and at each threshold, fit a user specified statistical model, then pull out
# a coefficient of interest (e.g., Diversity) and it's information from a summar
# table.  This function can be used for glm and lm models, and potentially others
# with the proper specification.
#####


getCoefTab<-function(eqn, fun=glm, data, groupVar="thresholds", coefVar, ...){
  ddply(data, .variables=groupVar, function(adf) {
    
    if(length(unique(adf[[ as.character(eqn[[2]]) ]]))==1) return(c(0,0,NA,1)) #in case all functions perform exactly the same, the slope is a flat line
    
    options(warn=2)
    
    #use try-catch in case there are errors
    aFit<-try(fun(eqn, data=adf, ...))

    options(warn=0)
    
    #if there was a problem, catch it and just return NAs for this coefficient
    if("try-error" %in% class(aFit)) return(rep(NA, 4))
    
    if("glm" %in% class(aFit)) {    if(!aFit$converged) return(rep(NA, 4))	}
    
    
    coefInfo<-summary(aFit)$coef
    
    idx<-which(rownames(coefInfo) == coefVar)
    
    return(coefInfo[idx,])  
  })  
  
}