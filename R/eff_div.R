#' @title funcDiv
#' @description
#' \code{funcDiv} effective number of functions
#' 
#' @details Takes a data frame, variable names, and type of index and returns
#'  the effective number of functions - i.e., the equivalent number of 
#'  functions if all were performing at the same level. See Jost 2006 and 2010.
#'  Internally, uses \code{\link[vegetarian]{d}}
#' 
#' @author Jarrett Byrnes.
#' @param data A vector of measurements of a function.
#' @param vars Name of function variables
#' @param q 	Order of the diversity measure. Defaults to the 
#' Shannon case where q = 1. For Simpson, q=2.
#' 
#' @export
#' @return Returns a vector.
#' @references Jost, L. 2006. Entropy and diversity. Oikos 113(2): 363-375.
#' 
#' Hill, M. 1973. Diversity and evenness: A unifying notation and its 
#' consequences. Ecology 54: 427-432.
#' 


funcDiv <- function(data, vars, q=1){
  #get the variable data frame
  df <- data[,which(names(data) %in% vars)]
  
  #proportion of community
  eff_div(df, q=q)
}

#' @title eff_div
#' @description
#' \code{eff_div} effective number of functions
#' 
#' @details Takes a data frame, variable names, and type of index and returns
#'  the effective number of functions - i.e., the equivalent number of 
#'  functions if all were performing at the same level. See Jost 2006 and 2010.
#'  Internally, uses \code{\link[vegetarian]{d}}
#' 
#' @author Jarrett Byrnes.
#' @param df A data frame of functions
#' @param q 	Order of the diversity measure. Defaults to the 
#' Shannon case where q = 1. For Simpson, q=2.
#' 
#' @export
#' @return Returns a vector.
#'
eff_div <- function(df, q=1){
  #loop over the whole data frame, row by row
  sapply(1:nrow(df), function(x){
    vegetarian::d(df[x,], q=q)
  })
}


