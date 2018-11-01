#' Variance computations
#'
#' @name variance_computations

NULL

#' @rdname variance_computations
#' @param p a vector of proportions corresponding to the proportion in each group
#' @return a covariance matrix

multinom_var <- function(p){
  diag(p) - outer(p, p)
}

#' Compute n, mean and variance
#'
#' Available for \code{numeric}, \code{integer}, and \code{factor} objects.
#' \code{character} objects are handled by first converting to a factor.
#'
#' @name n_mean_var
#' @param x a vector of values
#' @importFrom stats var
#' @return a list containing \code{mean} and \code{var}

setGeneric("n_mean_var", def = function(x) {
  standardGeneric("n_mean_var")
})

#' @rdname n_mean_var

setMethod(
  f          = "n_mean_var",
  signature  = "numeric",
  definition = function(x){
    list(n = length(x), mean = mean(x), var = stats::var(x))
} )

setMethod(
  f          = "n_mean_var",
  signature  = "integer",
  definition = function(x){

    check_for_two_levels(x)
    list(n = length(x), mean = mean(x), var = stats::var(x))
} )

#' @rdname n_mean_var

setMethod(
  f          = "n_mean_var",
  signature  = "logical",
  definition = function(x){
    list(n = length(x), mean = mean(x), var = stats::var(x))
} )

#' @rdname n_mean_var

setMethod(
  f          = "n_mean_var",
  signature  = "factor",
  definition = function(x){
    p <- prop.table(table(x))
    list(n = length(x), mean = p, var = multinom_var(p))
  })

#' @rdname n_mean_var

setMethod(
  f          = "n_mean_var",
  signature  = "character",
  definition = function(x){
    x <- as.factor(x)

    if(nlevels(x) > 50){
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    n_mean_var(x)
})




