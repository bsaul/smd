#' Variance computations
#'
#' @name variance_computations
#' @keywords internal
NULL

#' @rdname variance_computations
#' @param p a vector of proportions corresponding to the proportion in each group
#' @return a covariance matrix
#' @keywords internal
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
#' @param w an optional vector of \code{numeric} weights
#' @param na.rm passed to \code{sum}
#' @importFrom stats var
#' @return a list containing \code{mean} and \code{var}
#' @keywords internal
setGeneric("n_mean_var", def = function(x, w = NULL, na.rm = FALSE){
  standardGeneric("n_mean_var")

})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("numeric", "missing"),
  definition = function(x, w, na.rm = FALSE){

    if(na.rm == TRUE){
      x <- stats::na.omit(x)
    }

    n    <- length(x)
    mean <- sum(x)/n

    list(
      n    = n,
      mean = mean,
      var  = sum((x - mean)^2)/n
    )
} )

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("numeric", "numeric"),
  definition = function(x, w, na.rm = FALSE){

    if(na.rm == TRUE){
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    if(length(x) != length(w)){
      stop("x and w must have same length")
    }

    xw   <- x * w
    n    <- sum(w)
    # Handle case were sum of weights is 0
    mean <- if(n == 0) 0 else sum(xw)/n

    list(
      n    = n,
      mean = mean,
      var  = if(n == 0) 0 else sum(w*(x - mean)^2)/n
    )
})


#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("integer", "missing"),
  definition = function(x, w, na.rm = FALSE){

    check_for_two_levels(x)
    n_mean_var(x = as.numeric(x), na.rm = na.rm)
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("integer", "numeric"),
  definition = function(x, w, na.rm = FALSE){

    check_for_two_levels(x)
    n_mean_var(x = as.numeric(x), w = w, na.rm = na.rm)
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("logical", "missing"),
  definition = function(x, na.rm = FALSE){
    n_mean_var(x = as.numeric(x), na.rm = na.rm)
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("logical", "numeric"),
  definition = function(x, w, na.rm = FALSE){
    n_mean_var(x = as.numeric(x), w = w, na.rm = na.rm)
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("factor", "missing"),
  definition = function(x, w, na.rm = FALSE){

    if(na.rm == TRUE){
      x <- stats::na.omit(x)
    }

    p <- prop.table(table(x))
    list(n = length(x), mean = p, var = multinom_var(p))
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("factor", "numeric"),
  definition = function(x, w, na.rm = FALSE){

    if(na.rm == TRUE){
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    n <- sum(w)
    p <- tapply(w, x, function(r) if(n == 0) 0 else sum(r)/n)
    list(n = n, mean = p, var = multinom_var(p))
  })

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("character", "missing"),
  definition = function(x, w, na.rm = FALSE){

    if(na.rm == TRUE){
      x <- stats::na.omit(x)
    }

    x <- as.factor(x)

    if(nlevels(x) > 50){
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    n_mean_var(x)
})

#' @rdname n_mean_var
setMethod(
  f          = "n_mean_var",
  signature  = c("character", "numeric"),
  definition = function(x, w, na.rm = TRUE){

    if(na.rm == TRUE){
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    x <- as.factor(x)

    if(nlevels(x) > 50){
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    n_mean_var(x, w)
})



