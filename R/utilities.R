#' Checks whether a vector has more than two unique values
#'
#' @param x a vector
#' @keywords internal

check_for_two_levels <- function(x){
  xunique <- sort(unique(x))
  if(length(xunique) == 2 && all(xunique != c(0, 1))){
    warning(sprintf("x has two levels, but they are not code as 0/1. SMD is being computed based on the values %s.", paste(xunique, collapse = ",")))
  }
}

#' Apply a function pairwise along a list
#'
#' Applies a function \code{f(ref, y)} where \code{y} is an element of a list and
#'  \code{ref} is a reference element.
#'
#' @param x a list
#' @param f the function to apply
#' @param ref the index of the reference element
#' @keywords internal

lapplyFUNpairwise <- function(x, f, ref){
  if(sum(is.na(x)) >= 1) stop("x contians NA value")
  lapply(x[-ref], FUN = function(y) { f(x[[ref]], y) })
}

