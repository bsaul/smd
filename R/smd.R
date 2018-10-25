#' Compute Standardized Mean Difference
#'
#' Computes the standardized mean differnce (SMD) between two groups.
#'
#' @name smd
#' @param x a vector of values
#' @param g a vector of groupings to compare
#' @param method not yet implemented
#' @return the standardized mean differences between levels of \code{g}
#' for values of \code{x}
#' @seealso \code{\link{compute_smd}} for mathematical details
#' @export
#' @examples
#' x <- rnorm(100)
#' g <- rep(1:2, each = 50)
#' smd(x, g)

setGeneric(
  "smd",
  def = function(x, g, method = NA_character_){
    parts <- compute_smd_parts(x, g)
    compute_smd(parts$D, parts$S)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = "logical",
  def = function(x, g, method = NA_character_){
    smd(as.numeric(x), g = g, method = method)
  }
)

#' @rdname smd
#' @export

setMethod(
  "smd",
  signature = "matrix",
  def = function(x, g, method = NA_character_){
    apply(x, 2, function(j) smd(x = j, g = g, method = method))
  }
)

#' @rdname smd
#' @importFrom purrr map_dbl
#' @export

setMethod(
  "smd",
  signature = "list",
  def = function(x, g, method = NA_character_){
    purrr::map_dbl(x, ~ smd(x = .x, g = g, method = method))
  }
)

#' @rdname smd
#' @importFrom purrr map_dbl
#' @export

setMethod(
  "smd",
  signature = "data.frame",
  def = function(x, g, method = NA_character_){
    purrr::map_dbl(x, ~ smd(x = .x, g = g, method = method))
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
#' where \eqn{D} is a vector of differences between group 1 and 2 and \eqn{S} is the
#' covariance matrix of these differences. If \eqn{D} is length 1, the result is
#' multplied by \eqn{sign(D)}.
#'
#' In the case of a \code{numeric} or \code{integer} variable, this is equivalent to:
#'
#' \deqn{
#'   d = \frac{\bar{x}_1 - \bar{x}_2}{\sqrt{(s^2_1 + s^2_2)/2}}
#' }
#' where \eqn{\bar{x}_g} is the sample mean for group \eqn{g} and \eqn{s^2_g} is the sample variance.
#'
#' For a \code{logical} or \code{factor} with only two levels, the equation above is
#' \eqn{\bar{x}_g = \hat{p}_g}, i.e. the sample proportion and \eqn{s^2_g = \hat{p}_g(1 - \hat{p}_g)}
#' (NOTE: interally \code{smd} uses the \code{\link[stats]{var}} function, which uses \eqn{n-1} as the
#' denominator. Hence, in small samples, \eqn{s^2_g} will not be precisely
#' \eqn{\hat{p}_g(1 - \hat{p}_g)})
#'
#' @param D vector of differences for each level of a factor (will be length 1 for numeric values)
#' @param S the covariance matrix
#' @importFrom MASS ginv
#' @return a single numeric value
#' @references Yang, D., & Dalton, J. E. (2012, April). A unified approach to measuring
#' the effect size between two groups using SAS®. In SAS Global Forum (Vol. 335, pp. 1-6)
#' @seealso \code{\link{smd}}

compute_smd <- function(D, S){
  out <- sqrt(t(D) %*% MASS::ginv(S) %*% D)
  if(length(D) == 1){
    out <- out * sign(D)
  }
  drop(out)
}

#' Compute components of SMD
#'
#' Computes \code{D} and \code{S} for use within \link{compute_smd}.
#'
#' @inheritParams smd
#' @param tapplyMethod either \code{tapply} or \code{\link[fastmatch]{ctapply}}.
#' Defaults to \code{\link[fastmatch]{ctapply}}.
#' @param tapplyFUN the \code{FUN} argument passed to \code{tapplyMethod}
#' @param tapplyArgs a \code{list} of arguments passed to \code{tapplyMethod}
#' @importFrom fastmatch ctapply
#' @importFrom purrr map

compute_smd_parts <- function(x, g,
                              tapplyMethod = fastmatch::ctapply,
                              tapplyFUN    = mean_var,
                              tapplyArgs   = list(MERGE = list)){

  # Checks
  if(length(x) != length(g)){
    stop("Length of x and g must match")
  }

  if(length(unique(g)) != 2){
    stop("g must have 2 unique values. smd() is currently only implemented for two groups.")
  }

  args <- append(list(X = x, INDEX = g, FUN = tapplyFUN), tapplyArgs)
  U    <- do.call(tapplyMethod, args = args)

  #TODO: is a list the best representation for mean_var?
  D <- Reduce("-", purrr::map(U, ~ .x$mean))
  S <- Reduce("+", purrr::map(U, ~.x$var))/length(U)

  list(D = D, S = S)
}

#' Computes SMD variance
#'
#' Not yet implemented
#' @param d an SMD value
#' @param n vector of group sizes
#'

smd_var <- function(d, n){
  nn <- sum(n)
  nn/prod(n) + (d^2)/(2*nn)
}
