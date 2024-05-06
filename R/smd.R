#' Compute Standardized Mean Difference
#'
#' @description
#' Computes the standardized mean differnce (SMD) between two groups.
#'
#' \deqn{
#'  d = \sqrt{D' S^{-1} D}
#' }
#'
#' where \eqn{D} is a vector of differences between group 1 and 2 and \eqn{S} is
#' the covariance matrix of these differences. If \eqn{D} is length 1, the result
#' is multplied by \eqn{sign(D)}.
#'
#' In the case of a \code{numeric} or \code{integer} variable, this is equivalent
#' to:
#'
#' \deqn{
#'   d = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{(s^2_1 + s^2_2)/2}}
#' }
#' where \eqn{\bar{x}_g} is the sample mean for group \eqn{g} and \eqn{s^2_g} is
#' the sample variance.
#'
#' For a \code{logical} or \code{factor} with only two levels, the equation above is
#' \eqn{\bar{x}_g = \hat{p}_g}, i.e. the sample proportion and \eqn{s^2_g = \hat{p}_g(1 - \hat{p}_g)}.
#'
#' When using the SMD to evaluate the effectiveness of weighting in achieving
#' covariate balance, it is important to isolate the change in SMD before and
#' after weighting to the change in mean difference, so the denominator (covariance matrix)
#' must be held constant (Stuart 2008, \doi{doi:10.1002/sim.3207}).
#' By default, the unweighted covariance matrix is used to compute SMD in both
#' the unweighted and weighted case. If the weights are not being used to adjust
#' for covariate imbalance (e.g. case weights), the \code{unwgt.var} argument
#' can be set to \code{FALSE} to use the weighted covariance matrix as the denominator.
#'
#' @name smd
#'
#' @param x a \code{vector} or \code{matrix} of values
#' @param g a vector of at least 2 groups to compare. This should coercable to a
#'    \code{factor}.
#' @param w a vector of \code{numeric} weights (optional)
#' @param std.error Logical indicator for computing standard errors using
#'    \code{\link{compute_smd_var}}. Defaults to \code{FALSE}.
#' @param na.rm Remove \code{NA} values from \code{x}? Defaults to \code{FALSE}.
#' @param gref an integer indicating which level of \code{g} to use as the reference
#'     group. Defaults to \code{1}.
#' @param unwgt.var Use unweighted or weighted covariance matrix. Defaults to \code{TRUE}
#' @importFrom methods setGeneric setMethod
#' @return a \code{data.frame} containing standardized mean differences between
#'    levels of \code{g} for values of \code{x}. The \code{data.frame} contains
#'    the columns:
#'    \itemize{
#'      \item \code{term}: the level being comparing to the reference level
#'      \item \code{estimate}: SMD estimates
#'      \item \code{std.error}: (if \code{std.error = TRUE}) SMD standard error estimates
#'    }
#' @export
#' @examples
#' x <- rnorm(100)
#' g <- rep(1:2, each = 50)
#' smd(x, g)
setGeneric(
  "smd",
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    if (gref < 1 || gref > length(unique(g))) {
      stop(sprintf("gref must be an integer within %s", length(unique(g))))
    }

    parts <- compute_smd_parts(.x = x, .g = g, .w = w, .na = na.rm, .ref = gref, .unwgt = unwgt.var)
    d <- compute_smd_pairwise(parts)
    out <- list(term = names(d), estimate = unname(d))

    if (std.error) {
      ste <- unlist(Map(compute_smd_var, d = d, smd_parts = parts))
      out <- append(out, list(std.error = unname(sqrt(ste))))
    }

    tidy_smd_singlevar(out)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("character", "ANY", "missing"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    smd(x = as.factor(x), g = g, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("character", "ANY", "numeric"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    smd(x = as.factor(x), g = g, w = w, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("logical", "ANY", "missing"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    smd(x = as.numeric(x), g = g, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("logical", "ANY", "numeric"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    smd(x = as.numeric(x), g = g, w = w, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("matrix", "ANY", "missing"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    if (std.error) {
      stop("smd is not set up to compute std.error on a matrix")
    }

    apply(x, 2, function(j) {
      simplify2array(smd(x = j, g = g, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)$estimate)
    })
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("matrix", "ANY", "numeric"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    if (std.error) {
      stop("smd is not set up to compute std.error on a matrix")
    }

    apply(x, 2, function(j) {
      simplify2array(smd(
        x = j, w = w, g = g,
        std.error = std.error,
        na.rm = na.rm, gref = gref,
        unwgt.var = unwgt.var
      )$estimate)
    })
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("list", "ANY", "missing"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    tidy_smd_multiplevar(lapply(x, function(z) {
      smd(x = z, g = g, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
    }))
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("list", "ANY", "numeric"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    tidy_smd_multiplevar(lapply(x, function(z) {
      smd(x = z, g = g, w = w, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
    }))
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("data.frame", "ANY", "missing"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    tidy_smd_multiplevar(lapply(x, function(z) {
      smd(x = z, g = g, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
    }))
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = c("data.frame", "ANY", "numeric"),
  def = function(x, g, w, std.error = FALSE, na.rm = FALSE, gref = 1L, unwgt.var = TRUE) {
    tidy_smd_multiplevar(lapply(x, function(z) {
      smd(x = z, g = g, w = w, std.error = std.error, na.rm = na.rm, gref = gref, unwgt.var = unwgt.var)
    }))
  }
)

#' Compute the standardized mean difference
#'
#' This function is for internal package use only. See \code{\link{smd}} for usage.
#'
#' @details
#' Computes:
#'
#' \deqn{
#'  d = \sqrt{D' S^{-1} D}
#' }
#'
#' where \eqn{D} is a vector of differences between group 1 and 2 and \eqn{S} is
#' the covariance matrix of these differences. If \eqn{D} is length 1, the result
#' is multplied by \eqn{sign(D)}.
#'
#' In the case of a \code{numeric} or \code{integer} variable, this is equivalent
#'  to:
#'
#' \deqn{
#'   d = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{(s^2_1 + s^2_2)/2}}
#' }
#' where \eqn{\bar{x}_g} is the sample mean for group \eqn{g} and \eqn{s^2_g}
#' is the sample variance.
#'
#' For a \code{logical} or \code{factor} with only two levels, the equation above is
#' \eqn{\bar{x}_g = \hat{p}_g}, i.e. the sample proportion and \eqn{s^2_g = \hat{p}_g(1 - \hat{p}_g)}
#' (NOTE: interally \code{smd} uses the \code{\link[stats]{var}} function, which
#' uses \eqn{n-1} as the denominator. Hence, in small samples, \eqn{s^2_g} will
#' not be precisely \eqn{\hat{p}_g(1 - \hat{p}_g)}).
#'
#' @name compute_smd
#' @param smd_parts a \code{list} of components for from \code{\link{compute_smd_parts}}
#' computing standardized mean differences
#' @param D vector of differences for each level of a factor (will be length 1 for numeric values)
#' @param S the covariance matrix
#' @importFrom MASS ginv
#' @return a single numeric value
#' @references Yang, D., & Dalton, J. E. (2012, April). A unified approach to measuring
#' the effect size between two groups using SAS®. In SAS Global Forum (Vol. 335, pp. 1-6)
#' @seealso \code{\link{smd}}
#' @keywords internal

compute_smd_pairwise <- function(smd_parts) {
  simplify2array(lapply(smd_parts, function(x) compute_smd(x$D, x$S)))
}

#' @rdname compute_smd

compute_smd <- function(D, S) {
  out <- sqrt(t(D) %*% (MASS::ginv(S) %*% D))
  if (length(D) == 1) {
    out <- drop(out) * sign(D)
  }
  out
}

#' Computes SMD variance
#'
#' Calculates the variance of a standardized mean difference using the method of
#' Hedges and Olkin (1985):
#'
#' \deqn{
#' \sqrt{\frac{n_1 + n_2}{n_1n_2} + \frac{d^2}{2(n_1 + n_2)}}
#' }
#'
#' @param d an SMD value
#' @inheritParams compute_smd
#' @keywords internal
compute_smd_var <- function(d, smd_parts) {
  N <- smd_parts$N
  sn <- sum(N)
  sn / prod(N) + (d^2) / (2 * sn)
}

#' Compute components of SMD
#'
#' Computes \code{D} and \code{S} for use within \link{compute_smd}.
#'
#' @param .x a vector of values
#' @param .w a vector of weights (optional)
#' @param .g a vector of groupings to compare
#' @param .na \code{TRUE/FALSE}. \code{NA} handling
#' @param .ref integer position of the reference group
#' @param .unwgt Use unweighted or weighted covariance.
#' @param applyFUN the \code{FUN} used to compute the SMD parts. Defaults to
#' \code{\link{n_mean_var}}
#' @keywords internal
compute_smd_parts <- function(.x, .g, .w, .na, .ref, .unwgt,
                              applyFUN = n_mean_var) {
  # Checks
  if (length(.x) != length(.g)) {
    stop("Length of x and g must match")
  }

  if (anyNA(.x) && !.na) {
    stop("x contains NA values. Do you need to set na.rm = TRUE?")
  }

  ng <- length(unique(.g))

  if (ng < 2) {
    stop("g must contain at least two levels.")
  }

  # 20181130 - These next few lines (creating the object U) are critical and
  # surprisingly challenging to get right in a nice way. But what needs to happen:
  # * x needs to get split by g
  # * if w exists it also needs to get split by g
  # * the vectors x and w may be different types so that can't be cbind()ed
  #   before splitting without unintended coersion.
  # So my current tack is to combine them into a data.frame first.
  dd <- data.frame(x = .x)
  dd$w <- if (missing(.w)) NULL else .w
  ll <- split.data.frame(dd, f = .g)
  U <- simplify2array(lapply(ll, function(M) {
    do.call(applyFUN, args = append(M, list(na.rm = .na, unwgt.var = .unwgt)))
  }))

  # Create pairwise components
  N <- lapplyFUNpairwise(U["n", ], c, .ref)
  D <- lapplyFUNpairwise(U["mean", ], `-`, .ref)
  S <- lapplyFUNpairwise(U["var", ], function(x, y) (x + y) / 2, .ref)

  Map(list, N = N, D = D, S = S)
}

#' Helper to clean up smd output
#'
#' @name smd_tidier
#' @param smd_res a result of \link{smd}
#' @return a \code{data.frame}
#' @keywords internal
tidy_smd_singlevar <- function(smd_res) {
  data.frame(smd_res, stringsAsFactors = FALSE)
}

#' @rdname smd_tidier
tidy_smd_multiplevar <- function(smd_res) {
  if (length(names(smd_res)) == length(smd_res)) {
    hold <- lapply(seq_along(smd_res), function(i) {
      data.frame(
        variable = names(smd_res)[i], smd_res[[i]],
        stringsAsFactors = FALSE
      )
    })
  } else {
    hold <- lapply(smd_res, function(x) {
      data.frame(x, stringsAsFactors = FALSE)
    })
  }

  do.call("rbind", hold)
}
