#' @title Quote Words
#'
#' @description
#' \code{qw} Takes an unquoted vector and adds quotes to it like the qw function in perl.
#'
#' @details This is a helper function for data processing. Honestly, I use qw all the time
#' in other languages, and wanted a version for R.
#'
#' @author Jarrett Byrnes.
#' @param ... Any unquoted strings
#'
#'
#' @export
#' @return A vector
#'
#' @examples
#' c("a", "b")
#'
#' qw(a, b)
#'
#'
#'
#' # qw - a helper function that we
#' # will use later to deal with strings
#' # analagous to qw in PERL
qw <- function(...) {
  sapply(match.call()[-1], deparse)
}
