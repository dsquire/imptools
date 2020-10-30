#' @export pool.r
pool.r <- function (rVec, N)
{
  fisherT <- function(x) 0.5 * log((1 + x) / (1 - x))
  backFisherT <- function(ft) (exp(2 * ft) - 1)/(exp(2 * ft) + 1)

  if ((m <- length(rVec)) < 2)
    stop("At least two imputations are needed for pooling.\n")

  r <- matrix(NA,
              nrow = m,
              ncol = 3,
              dimnames = list(seq_len(m), c("r", "Z", "se")))

  r[,"r"] <- rVec
  r[,"Z"] <- fisherT(rVec)
  r[,"se"] <- 1/(N - 3)

  poolObj <- mice::pool.scalar(Q=r[,"Z"], U=r[,"se"], n=N)

  zPool <- poolObj$qbar

  table <- backFisherT(zPool)

  # Component t is the total variance of the pooled estimated, formula (3.1.5) Rubin (1987)
  table[2] <- backFisherT( zPool - 1.96 * sqrt(poolObj$t) )
  table[3] <- backFisherT( zPool + 1.96 * sqrt(poolObj$t) )
  table[4] <- poolObj$f

  names(table) <- c("rPool","lo95", "hi95", "fmi")

  return(table)
} # end pool.r
