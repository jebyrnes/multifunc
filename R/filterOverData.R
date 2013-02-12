#' @title filterOverData
#' @title filterCoefData
#'
#' @description
#' \code{filterOverData} and \code{filterCoefData} filter contributions of species
#' to function by sign.
#' 
#' @details Takes a matrix of functions and coefficients
#' for species - either 1's and -1's, or just continuous coefficients - and filters out
#' only the sign of contributions desired.  Typically used by other functions in the package.
#'
#' @author Jarrett Byrnes.
#' @param overData Matrix of functions and which species affect them from \code{getRedundancy}.
#' @param type Are the kinds of effects we're looking at "positive", "negative" or "all".
#' 
#' @export
#' @return Returns a filtered matrix.
#'
#' @examples
#' data(all_biodepth)
#' allVars<-qw(biomassY3, root3, N.g.m2,  light3, N.Soil, wood3, cotton3)
#'
#' germany<-subset(all_biodepth, all_biodepth$location=="Germany")
#'
#' vars<-whichVars(germany, allVars)
#'
#' #re-normalize N.Soil so that everything is on the same sign-scale (e.g. the maximum level of a function is the "best" function)
#' germany$N.Soil<- -1*germany$N.Soil +max(germany$N.Soil, na.rm=T)
#' 
#' res.list<-lapply(vars, function(x) sAICfun(x, species, germany))
#' names(res.list)<-vars
#' 
#' redund<-getRedundancy(vars, species, germany)
#' 
#' filterOverData(redund, type="positive")


#########
#filterOverData takes a matrix of 1s, 0s, and -1s
#and filters it so that only the positive, negative, or both contributions
#are 1 for later overlap function usage
#########
filterOverData<-function(overData, type="positive"){
  #format the data properly given the type of analysis being done
  neg<-which(overData<0, arr.ind=T)
  pos<-which(overData>0, arr.ind=T)
  
  if(type=="positive") apply(neg, 1, function(x) overData[x[1], x[2]]<<-0) #ugh, using apply and <<- because nothing else worked
  if(type=="negative") {
    apply(pos, 1, function(x) overData[x[1], x[2]]<<-0)
    apply(neg, 1, function(x) overData[x[1], x[2]]<<-1)
  }
  if(type=="all") apply(neg, 1, function(x) overData[x[1], x[2]]<<-1)
  
  overData
}


filterCoefData<-function(coefData, type="positive"){
  #format the data properly given the type of analysis being done
  neg<-which(coefData<0, arr.ind=T)
  pos<-which(coefData>0, arr.ind=T)
  
  if(type=="positive") apply(neg, 1, function(x) coefData[x[1], x[2]]<<-0) #ugh, using apply and <<- because nothing else worked
  if(type=="negative") {
    apply(pos, 1, function(x) coefData[x[1], x[2]]<<-0)
  }
  #  if(type=="all") apply(neg, 1, function(x) coefData[x[1], x[2]]<<-1)
  
  coefData
}
