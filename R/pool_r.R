#' Pool Pearson correlation coefficients
#'
#' @param rVec A vector of correlation coefficients
#' @param N The number of imputations
#' @return A vector of pooled r's
#' @importFrom mice pool.scalar
#' @importFrom tibble tibble
#' @export pool.r
pool.r <- function (rVec = NULL, N = NULL)
{
  # https://yihui.org/en/2014/07/library-vs-require/
  rVec_len <- length(rVec)

  if(rVec_len < 2) {
    stop("Error: rVec must have a length >= 2.")
  }

  r <- tibble::tibble(
    r = rVec,
    Z = fisherT(r),
    se = 1 / (N - 3)
  )

  poolObj <- mice::pool.scalar(Q = r$Z, U = r$se, n = N)

  # Component t is the total variance of the pooled estimated,
  # formula (3.1.5) Rubin (1987)
  tibble::tibble(
    rPool = backFisherT(poolObj$qbar),
    lo95 = backFisherT(poolObj$qbar - 1.96 * sqrt(poolObj$t)),
    hi95 = backFisherT(poolObj$qbar + 1.96 * sqrt(poolObj$t)),
    fmi = poolObj$fmi
  )
}

fisherT <- function(r = NULL) {
  # Equivalent to: 0.5 * log((1 + x) / (1 - x))
  atanh(r)
}

backFisherT <- function(z = NULL) {
  # Equivalent to: (exp(2 * ft) - 1)/(exp(2 * ft) + 1)
  tanh(z)
}
