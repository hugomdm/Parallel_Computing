
## File: estimate_Es.R
## Students : Lia Furtado and Hugo Vinson
## Projet Parallel Computing 2021 - Code improvements
# stock prices - Capther 22 (22.6 Stock-Prices)

## Description : Improved code with a apply neighborhood solution

## Date : 21 February 2022

#-------------- Functions ----------------------

# @title  estimate_Es: Estimation of the Esperance of GBM result
# @description  Give an estimation of the Esperance of GBM with confidance interval of 95%
# Give an estimation of the Probabilty of GBM result to be > 1 with confidance interval of 95%
# @param mu drift (float)
# @param sigma volatility (float)
# @param t time of prediction (integer)
#
# @return
# res: list with different estimation 
# $estimate_E: float: estimation of esperance
# $ci_E_inf: float:  lower bound
# $ci_E_sup: float: upper bound 
# $p: float: probability of St > 1
# $ci_p_inf: float:  lower bound
# $ci_p_sup: float:  upper bound
# @export
#
# @examples
# list_res <- estimate_Es(0.05, 0.0025, 100)
estimate_Es <- function(mu, sigma, t){
  n <- 10000
  vec_st <- numeric(n)
  for (i in seq(n)){
    res  <- gbm(t, mu, sigma, 1)
    vec_st[i] <- res[length(res)]
  }
  estimate_E <- mean(vec_st)
  S <- var(vec_st)
  ci_E_inf <- estimate_E - 1.96*(S/sqrt(n))
  ci_E_sup <- estimate_E + 1.96*(S/sqrt(n))
  p  <- length(which(vec_st>1))/n
  ci_p_inf <- p - 1.96*sqrt( p*(1-p) / n)
  ci_p_sup <- p + 1.96*sqrt( p*(1-p) / n)
  res <- list("estimate_E" = estimate_E, "ci_E_inf" = ci_E_inf, "ci_E_sup" = ci_E_sup, "p" = p, "ci_p_inf" = ci_p_inf, "ci_p_sup" = ci_p_sup)
  return(res)
} 