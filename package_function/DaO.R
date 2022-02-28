
## File: DaO.R
## Students : Lia Furtado and Hugo Vinson
## Projet Parallel Computing 2021 - Code improvements
# stock prices - Capther 22 (22.6 Stock-Prices)

## Description : Improved code with a apply neighborhood solution

## Date : 21 February 2022

#-------------- Functions ----------------------

#' DaO-calloption: Down and Out Call Option
#' Compute the value of the option at maturity 
#' @param t time to maturity in day (integer)
#' @param mu drift (float)
#' @param sigma volatility (float)
#' @param K strike price (float)
#' @param B barrier price
#' @param S0 value of stock at the end of the zero day (float)
#'
#' @return v: float: value of option at maturity 
#' @export
#'
#' @examples
#' res <- DaO_calloption(100, 0.05, 0.005, 2, 0.2)
DaO_calloption <- function(t, mu, sigma, K, B,  S0 = 1){

  v <- numeric(10000)
  for (i in seq(10000)){
    res <- gbm(t, mu, sigma, S0)
    s   <- res
    s_t <- s[length(s)]
    if (s_t >= K && min(s) > B){
      v[i] <- s_t - K 
    } else {
      v[i] <- 0
    }
  }
  return(v)
}
