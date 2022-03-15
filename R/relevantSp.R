#' @title relevantSp
#'
#' @description
#' \code{relevantSp} Which species are being used in this analysis.
#'
#' @details Which columns have values that are greater than zero.
#'
#' @author Jarrett Byrnes.
#' @param data A data frame with presence/abscence of different species.
#' @param colnums Column numbers that will be assessed.
#'
#' @export
#' @return A vector of columns names.
#'


# relevantSp takes a set of columns of species presence and abscence
# and then examines which were actually included in an experiment
# and returns their names
relevantSp <- function(data, colnums = 26:128) {
  names(which(colSums(data[, colnums]) > 0))
}
