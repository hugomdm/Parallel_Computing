gbm <- function(t, mu, sigma, S0){
  #compute Geometric browian movement
  #Parameters: 
  #t: time of prediction
  #mu: drift
  #sigma: volatilty
  #S0: value of stock at the end of the zero day
  #return: 
  #res: vector: all value from 0 to t of GBM prediction 
  res <- numeric(t)
  res[1] <- S0
  z <- rnorm(t,0,1)
  for (i in seq(from=2, to=t, by=1)){
    set.seed(10)
    res[i] <- res[i-1] * exp((mu - 0.5*sigma) + sqrt(sigma)* z[i])
  }
  return(res)
}

simul_s <- function(mu, sigma, t){
  #Estimate esperance of S and Probability which St will be > 1 
  #Result give with an confidence intrvall of 95%
  #Parameters:
  #mu: float: drift
  #sigma: float: volatility 
  #t: int: time 
  #Returns:
  #list:
  #$estimate_E: float: estimation of esperance
  #$ci_E_inf: float:  lower bound
  #$ci_E_sup: float: upper bound 
  #$p: float: probability of St > 1
  #$ci_p_inf: float:  lower bound
  #$ci_p_sup: float:  upper bound
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
  return(list("estimate_E" = estimate_E, "ci_E_inf" = ci_E_inf, "ci_E_sup" = ci_E_sup, "p" = p, "ci_p_inf" = ci_p_inf, "ci_p_sup" = ci_p_sup))
  
} 


V_t <- function(t, mu, sigma, K, B,  S0 = 1){
  #Parameters: 
  #t: time of prediction
  #mu: float: drift
  #sigma: float: volatilty
  #S0: flaot: value of stock at the end of the zero day
  #K: float: strike price
  #B: float:  barrier price
  #return: 
  #v: float: value of option at maturity 
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
