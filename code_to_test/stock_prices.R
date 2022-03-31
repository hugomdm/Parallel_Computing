library(ggplot2)
gbm <- function(t, mu, sigma, S0){
  #comute Geometric browian movement
  #Parameters: 
  #x: time of prediction
  #mu: drift
  #sigma: volatilty
  #S0: value of stock at the end of the zero day
  #return: 
  #res: vector: all value from 0 to t of GBM prediction 
  res <- numeric(t)
  res[1] <- S0
  z <- rnorm(t,0,1)
  for (i in seq(from=2, to=t, by=1)){
    res[i] <- res[i-1] * exp((mu - 0.5*sigma) + sqrt(sigma)* z[i])
  }
  return(res)
}
res <- gbm(50, 0.1, 0.3, 1)
plot(res, type="l")

#22.6.1 Simulating S 
st <- numeric(1000)
for (i in seq(1000)){
  res <- gbm(1000, 0.1, 0.5, 1)
  st[i] <- res[1000]
}
lst = unlist(lapply(st, log))

#histogram
hist(lst)

#test normalité
shapiro.test(lst)
# p-valye = 0.6 -> valeur non significative -> On ne peut pas rejet la normalit? 
quantile = quantile(lst,probs = seq(0.01,0.99,0.01))
plot(quantile)

#calcule param?tre de la loi Normal ont utilise la moyenne et la variance corrig? 
#estimateur qui maximise la vraisemblance
estimation_alpha = mean(lst)
estimation_beta = var(lst)

#comparaison lst et N(alpha, beta)
p1 <- hist(lst)
p2 <- hist(rnorm(1000,estimation_alpha,sqrt(estimation_beta)))
plot( p1, col="blue")
plot( p2, col="red", add=T)


#test find alpha and beta 
vec_mu <- c(0.1, 0.2, 0.5, 0.8)
vec_sigma <- c(0.05, 0.1, 0.2, 0.4)
data <- expand.grid(vec_mu, vec_sigma)
count <- 1
for (mu in vec_mu){
  for (sigma in vec_sigma){
    st = numeric(100)
    for (i in seq(1000)){
      res = gbm(100, mu, sigma, 1)
      st[i] = res[100]
    }
    lst = unlist(lapply(st, log))
    data[count, 3] <- mean(lst)
    data[count,4]  <- var(lst) 
    count = count + 1
  }
}
colnames(data) <- c('mu', 'sigma', 'mean_sample', 'var_sample')

# On peut appercevoir que lorsque sigma (volatility) augmente la moyenne 
# de l'échantillon augmente et lorsque mu (drift) augmente la moyenne décroit
# on peut constater que la volatiliy donne l'ordre de grandeur. 
# Pour la variance elle a tendance à croitre lorsque le drift augmente et la volatility 
# semble n'avoir aucun impat sur cette dernière. 

#c22.6.2 Estimating ES(t)

simul_s <- function(mu, sigma, t){
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


vec_mu <- c(0.05, 0.01, 0.01)
vec_sigma <- c(0.0025, 0.0025, 0.01)
est_E    <- numeric(3)
ci_E_inf <- numeric(3)
ci_E_sup <- numeric(3)
est_p    <- numeric(3)
ci_p_inf <- numeric(3)
ci_p_sup <- numeric(3)
for (i in seq(3)){
  res         <- simul_s(vec_mu[i], vec_sigma[i], 100)
  est_E[i]    <- res["estimate_E"]
  ci_E_inf[i] <- res["ci_E_inf"]
  ci_E_sup[i] <- res["ci_E_sup"]
  est_p[i]    <- res["p"]
  ci_p        <- res["ci_p"]
  ci_p_inf[i] <- res["ci_p_inf"]
  ci_p_sup[i] <- res["ci_p_sup"]
}


data2 <- data.frame("mu" = vec_mu,
                    "sigma"= vec_sigma,
                    "est_E"= unlist(est_E),
                    "ci_E_inf" = unlist(ci_E_inf),
                    "ci_E_sup" = unlist(ci_E_sup),
                    "est_p"= unlist(est_p),
                    "ci_p_inf" = unlist(ci_p_inf),
                    "ci_p_sup" = unlist(ci_p_sup)
                    )
                    


#22.6.3 Down-and-out-call-option
V_t <- function(t, mu, sigma, K, B,  S0 = 1){
  v <- numeric(10000)
  for (i in seq(10000)){
    res <- gbm(t, mu, sigma, S0)
    s_t <- res[length(res)]
    if (s_t >= K && min(res) > B){
      v[i] <- s_t - K 
    } else {
      v[i] <- 0
    }
  }
  return(v)
}



mu    <-  0.01
vec_sigma <- c(0.0025, 0.005, 0.01)
K <- 2 
t <- 100 
B <- 0.2
p_v <- numeric(3)
vec_v <- list()
c <- 1
for (sigma in vec_sigma){
  v <- V_t(t, mu, sigma, K, B)
  vec_v[[c]] <- v
  p_v[c] <- length(which(v>0))/10000
  c <- c + 1 
}

cdf_1 <- ecdf(vec_v[[1]])
cdf_2 <- ecdf(vec_v[[2]])
cdf_3 <- ecdf(vec_v[[3]])

attach(mtcars)
par(mfrow=c(2,2))
plot(cdf_1, main='cfd with sigma = 0.0025')
plot(cdf_2, main='cfd with sigma = 0.005')
plot(cdf_3, main='cfd with sigma = 0.01')

#Quand Sigma augmente la probabilité d'être égale à 0 augmente. 