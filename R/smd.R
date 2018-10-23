#' Compute Standardized Mean Difference
#'
#' @name smd
#' @param x a vector of values
#' @param g a vector
#' @param method not yet implemented
#' @importFrom fastmatch ctapply
#' @return the standardized mean differences between levels of \code{g}
#' for values of \code{x}
#' @export

setGeneric("smd", function(x, g, method = NA_character_) standardGeneric("smd"))

#' @rdname smd
#' @export

setMethod(
  f          = "smd",
  signature  = "numeric",
  definition = function(x, g, method){

    hold <- fastmatch::ctapply(X  = x, INDEX = g,
                               FUN   = mean_var,
                               MERGE = function(out) do.call("rbind", args = out))

    diff(hold[, 1])/mean(hold[ , 2])
  }
)

#' @rdname smd
#' @export

setMethod(
  f          = "smd",
  signature  = "integer",
  definition = function(x, g, method){
    check_for_two_levels(x)
    smd(x = as.numeric(x), g = g, method = method)
  }
)

#' @rdname smd
#' @export

setMethod(
  f          = "smd",
  signature  = "logical",
  definition = function(x, g, method){
    smd(x = as.numeric(x), g = g, method = method)
  }
)


#' @rdname smd
#' @importFrom purrr map
#' @importFrom MASS ginv
#' @export

setMethod(
  f          = "smd",
  signature  = "factor",
  definition = function(x, g, method){
    U <- fastmatch::ctapply(X = x, INDEX = g, FUN = mean_var, MERGE = list)
    #TODO: is a list the best representation? Because I end up with the following
    D <- Reduce("-", purrr::map(U, ~ .x$mean))
    S <- Reduce("+", purrr::map(U, ~.x$var))/length(U)

    t(D) %*% MASS::ginv(S) %*% D
  }
)

#' Computes smd variance
#' @export

smd_var <- function(d, n){
  nn <- sum(n)
  nn/prod(n) + (d^2)/(2*nn)
}
