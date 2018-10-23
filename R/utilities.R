#' Checks whether a vector has more than two unique values
#'
#' @param x a vector

check_for_two_levels <- function(x){
  xunique <- sort(unique(x))
  if(length(xunique) == 2 && xunique != c(0, 1)){
    warning(sprintf("x has two levels, but they are not code as 0/1. SMD is being computed based on the values %s.", paste(xunique, collapse = ",")))
  }
}
