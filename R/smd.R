#' Computes the standardized mean difference
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
#'
#' @param D vector of differences for each level of a factor (will be length 1 for numeric values)
#' @param S the covariance matrix
#' @importFrom MASS ginv
#' @return a single numeric value
#' @seealso \code{\link{smd}}

compute_smd <- function(D, S){
  out <- sqrt(t(D) %*% MASS::ginv(S) %*% D)
  if(length(D) == 1){
    out <- out * sign(D)
  }
  drop(out)
}

#' Compute Standardized Mean Difference
#'
#' Computes the standardized mean differnce (SMD) between two groups.
#'
#' @name smd
#' @param x a vector of values
#' @param g a vector
#' @param method not yet implemented
#' @importFrom fastmatch ctapply
#' @importFrom purrr map
#' @return the standardized mean differences between levels of \code{g}
#' for values of \code{x}
#' @seealso \code{\link{compute_smd}} for mathematical details
#' @export

setGeneric(
  "smd",
  def = function(x, g, method = NA_character_){
      U <- fastmatch::ctapply(X     = x,
                              INDEX = g,
                              FUN   = mean_var,
                              MERGE = list)
      #TODO: is a list the best representation? Because I end up with the following
      D <- Reduce("-", purrr::map(U, ~ .x$mean))
      S <- Reduce("+", purrr::map(U, ~.x$var))/length(U)

      compute_smd(D, S)
})


#' Computes smd variance
#' @export

smd_var <- function(d, n){
  nn <- sum(n)
  nn/prod(n) + (d^2)/(2*nn)
}
