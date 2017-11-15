#' @title funcEven
#' @description
#' \code{funcEven} evenness factor across functions
#' 
#' @details Takes a data frame, variable names, and type of index and gets 
#' the evenness factor for the collection of values supplied as defined in 
#' Jost  2010
#' @author Jarrett Byrnes.
#' @param data A vector of measurements of a function.
#' @param vars Name of function variables
#' @param q 	Order of the diversity measure. Defaults to the 
#' Shannon case where q = 1. For Simpson, q=2.
#'  
#' @export
#' @return Returns a vector.
#' @references Jost, L. 2006. Entropy and diversity. Oikos 113(2): 363-375.

funcEven <- function(data, vars, q=1){
  #get a df of just the vars
  df <- data[,which(names(data) %in% vars)]
  #get the evenness
  even_fact(df, q = q)
  
}

#' @title even_fact
#' @description
#' \code{even_fact} evenness factor across functions
#' 
#' @details Takes a data frame, variable names, and type of index and gets 
#' the evenness factor for the collection of values supplied as defined in 
#' Jost 2010
#' @author Jarrett Byrnes.
#' @param df A data frame of functions
#' @param q 	Order of the diversity measure. Defaults to the 
#' Shannon case where q = 1. For Simpson, q=2.
#'  
#' @export
#' @return Returns a vector.
#'
even_fact <- function(df, q=1){
  eff_div(df,q=q)/ncol(df)
}

