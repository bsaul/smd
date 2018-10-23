#' Computes proportions in each factor level
#'
#' @param x a factor variable. It must be \code{sort}ed first!
#'
setGeneric("proportions", function(x) standardGeneric("proportions"))

#' @importFrom fastmatch ctapply

setMethod(
  f          = "proportions",
  signature  = "factor",
  definition = function(x){
    n <- length(x)
    # This is basically equivalent to table() but a touch faster
    fastmatch::ctapply(X = x, INDEX = x, FUN = function(g) length(g)/n)
  }
)

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


#' Compute mean and variance
#'
#' Available for \code{numeric}, \code{integer}, and \code{factor} objects.
#' \code{character} objects are handled by first converting to a factor.
#'
#' @name mean_var
#' @param x a vector of values
#' @return a list containing \code{mean} and \code{var}

setGeneric("mean_var", def = function(x) {
  standardGeneric("mean_var")
})

#' @rdname mean_var

setMethod(
  f          = "mean_var",
  signature  = "numeric",
  definition = function(x){
    list(mean = mean(x), var = var(x))
} )

setMethod(
  f          = "mean_var",
  signature  = "integer",
  definition = function(x){

    check_for_two_levels(x)
    list(mean = mean(x), var = var(x))
} )

#' @rdname mean_var

setMethod(
  f          = "mean_var",
  signature  = "logical",
  definition = function(x){
    list(mean = mean(x), var = var(x))
} )

#' @rdname mean_var

setMethod(
  f          = "mean_var",
  signature  = "factor",
  definition = function(x){
    p <- proportions(x)
    list(mean = p, var = multinom_var(p))
  })

#' @rdname mean_var

setMethod(
  f          = "mean_var",
  signature  = "character",
  definition = function(x){
    x <- as.factor(x)

    if(nlevels(x) > 50){
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    mean_var(x)
})




