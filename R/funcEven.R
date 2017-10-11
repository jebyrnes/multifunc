#' @title funcEven
#' @description
#' \code{funcEven} evenness factor across functions
#' 
#' @details Takes a data frame, variable names, and type of index and gets 
#' the evenness factor for the collection of values supplied as defined in 
#' Jost 2010
#' @author Jarrett Byrnes.
#' @param data A vector of measurements of a function.
#' @param vars Name of function variables
#' @param type Diversity index to be used. Options are "Simpson" and "Shannon"
#' 
#' @export
#' @return Returns a vector.
#'

funcEven <- function(data, vars, type="Simpson"){
  #get a df of just the vars
  df <- data[,which(names(data) %in% vars)]
  #get the evenness
  even_fact(df, type)
  
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
#' @param type Diversity index to be used. Options are "Simpson" and "Shannon"
#' 
#' @export
#' @return Returns a vector.
#'
even_fact <- function(df, type="Simpson"){
  eff_div(df,type)/ncol(df)
}

