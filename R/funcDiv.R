#' @title funcDiv
#' @description
#' \code{funcDiv} effective number of functions
#' 
#' @details Takes a data frame, variable names, and type of index and returns
#'  the effective number of functions - i.e., the equivalent number of 
#'  functions if all were performing at the same level. See Jost 2006 and 2010.
#' 
#' @author Jarrett Byrnes.
#' @param data A vector of measurements of a function.
#' @param vars Name of function variables
#' @param type Diversity index to be used. Options are "Simpson" and "Shannon"
#' 
#' @export
#' @return Returns a vector.
#'

funcDiv <- function(data, vars, type="Simpson"){
  #get the variable data frame
  df <- data[,which(names(data) %in% vars)]
  
  #proportion of community
  eff_div(df, type)
}

#' @title eff_div
#' @description
#' \code{eff_div} effective number of functions
#' 
#' @details Takes a data frame, variable names, and type of index and returns
#'  the effective number of functions - i.e., the equivalent number of 
#'  functions if all were performing at the same level. See Jost 2006 and 2010.
#' 
#' @author Jarrett Byrnes.
#' @param df A data frame of functions
#' @param type Diversity index to be used. Options are "Simpson" and "Shannon"
#' 
#' @export
#' @return Returns a vector.
#'
eff_div <- function(df, type="Simpson"){
  #proportion of community
  p_df <- df/rowSums(df)
  ln_p_df <- log(p_df)
  ln_p_df[ln_p_df==-Inf] <- 0
  ln_p_df[ln_p_df==NaN] <- 0
  
  #effective number via Simpson's diversity'
  div <- 1/rowSums(p_df^2)
  if(type=="Shannon") div <-exp(-1*rowSums(p_df*ln_p_df))
  
  #NaN = 0
  div[is.nan(div)] <- 0
  
  div
}


