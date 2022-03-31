
## File: gbm.R
## Students : Lia Furtado and Hugo Vinson
## Projet Parallel Computing 2021 - Code improvements
# stock prices - Capther 22 (22.6 Stock-Prices)

## Description : Improved code with a apply neighborhood solution

## Date : 21 February 2022

#-------------- Functions ----------------------

#' @title GBM: Geometric Brownian Motion 
#' @description compute Geometric browian movement
#' @param t : time of prediction (integer)
#' @param mu : drift (float)
#' @param sigma : volatility (float)
#' @param S0 : value of stock at the end of the zero day (float)
#'
#' @return res: vector: all value from 0 to t of GBM prediction 
#' @export
#'
#' @examples
#' res <- gbm(100, 0.01, 0.0025, 1)
gbm <- function(t, mu, sigma, S0){
  res <- numeric(t)
  res[1] <- S0
  z <- rnorm(t,0,1)
  for (i in seq(from=2, to=t, by=1)){
    res[i] <- res[i-1] * exp((mu - 0.5*sigma) + sqrt(sigma)* z[i])
  }
  return(res)
}


