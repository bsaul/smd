#' Variance computations
#'
#' @name variance_computations
#' @keywords internal
NULL

#' @rdname variance_computations
#' @param p a vector of proportions corresponding to the proportion in each group
#' @return a covariance matrix
#' @keywords internal
multinom_var <- function(p) {
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
#' @param unwgt.var Use unweighted or weighted covariance matrix
#' @importFrom stats var
#' @importFrom methods setGeneric setMethod
#' @return a list containing \code{mean} and \code{var}
#' @keywords internal
setGeneric("n_mean_var", def = function(x, w = NULL, na.rm = FALSE, unwgt.var = TRUE) {
  standardGeneric("n_mean_var")
})

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("numeric", "missing"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      x <- stats::na.omit(x)
    }

    n <- length(x)
    mean <- sum(x) / n

    list(
      n    = n,
      mean = mean,
      var  = sum((x - mean)^2) / n
    )
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("numeric", "numeric"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    if (length(x) != length(w)) {
      stop("x and w must have same length")
    }

    xw <- x * w
    n <- sum(w)

    # Handle case were sum of weights is 0
    if (n == 0) {
      mean <- 0
      var <- 0
    } else if (unwgt.var == TRUE) {
      mean <- sum(xw) / n
      unwgt_n <- length(x)
      unwgt_mean <- sum(x) / unwgt_n
      var <- sum((x - unwgt_mean)^2) / unwgt_n
    } else {
      mean <- sum(xw) / n
      var <- sum(w * (x - mean)^2) / n
    }

    list(
      n    = n,
      mean = mean,
      var  = var
    )
  }
)


#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("integer", "missing"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    check_for_two_levels(x)
    n_mean_var(x = as.numeric(x), na.rm = na.rm, unwgt.var = unwgt.var)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("integer", "numeric"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    check_for_two_levels(x)
    n_mean_var(x = as.numeric(x), w = w, na.rm = na.rm, unwgt.var = unwgt.var)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("logical", "missing"),
  definition = function(x, na.rm = FALSE, unwgt.var = TRUE) {
    n_mean_var(x = as.numeric(x), na.rm = na.rm, unwgt.var = unwgt.var)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("logical", "numeric"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    n_mean_var(x = as.numeric(x), w = w, na.rm = na.rm, unwgt.var = unwgt.var)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("factor", "missing"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      x <- stats::na.omit(x)
    }

    p <- prop.table(table(x))
    list(n = length(x), mean = p, var = multinom_var(p))
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("factor", "numeric"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    n <- sum(w)
    p <- tapply(w, x, function(r) if (n == 0) 0 else sum(r) / n, default = 0)
    if (unwgt.var == TRUE) {
      unwt_p <- prop.table(table(x)) # for unweighted variance
      var <- multinom_var(unwt_p)
    } else {
      var <- multinom_var(p)
    }

    list(n = n, mean = p, var = var)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("character", "missing"),
  definition = function(x, w, na.rm = FALSE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      x <- stats::na.omit(x)
    }

    x <- as.factor(x)

    if (nlevels(x) > 50) {
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    n_mean_var(x)
  }
)

#' @rdname n_mean_var
setMethod(
  f = "n_mean_var",
  signature = c("character", "numeric"),
  definition = function(x, w, na.rm = TRUE, unwgt.var = TRUE) {
    if (na.rm == TRUE) {
      kp <- !is.na(x)
      w <- w[kp]
      x <- x[kp]
    }

    x <- as.factor(x)

    if (nlevels(x) > 50) {
      warning("x has more than 50 levels. Are you sure you meant for this?")
    }

    n_mean_var(x, w, unwgt.var = unwgt.var)
  }
)
