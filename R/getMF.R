#' @title getMF
#' \code{getMF} get multifunctionality index defined by function and evenness
#' 
#' @details Takes a data frame, variable names, a standardizing function, and a type of diversity index and returns an index of multifunctionality. Here MF = MF_a * MF_e where MF_a is the average level of functionality across functions sampled and MF_e is the evenness factor from \code{funcEven}.
#' @author Jarrett Byrnes.
#' @param data A vector of measurements of a function.
#' @param vars Name of function variables
#' @param standardizeFunction A function to standardize each individual 
#' function to the same scale, such as \code{standardizeUnitScale} or 
#' \code{standardizeZScore}
#' @param type Diversity index to be used. Options are "Simpson" and "Shannon"
#' 
#' @export
#' @return Returns a vector.
#'
#'
getMF <- function(data, vars, standardizeFunction=standardizeUnitScale, type="Simpson"){
  
  #get standardized functions
  std_funcs <- getStdAndMeanFunctions(data, vars, standardizeFunction)
  funcs <- std_funcs[,-ncol(std_funcs)]
  
  #get the average functional level
  mf_a <- std_funcs$meanFunction
  
  #get evenness
  func_even <- even_fact(funcs, type)
  
  #mf = average * evenness
  mf_a*func_even

}