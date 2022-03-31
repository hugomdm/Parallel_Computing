library(ggplot2)
library(furvin)

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

vec_mu <- c(0.05, 0.01, 0.01)
vec_sigma <- c(0.0025, 0.0025, 0.01)
est_E    <- numeric(3)
ci_E_inf <- numeric(3)
ci_E_sup <- numeric(3)
est_p    <- numeric(3)
ci_p_inf <- numeric(3)
ci_p_sup <- numeric(3)
for (i in seq(3)){
  res         <- estimate_Es(vec_mu[i], vec_sigma[i], 100)
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
mu<-  0.01
vec_sigma <- c(0.0025, 0.005, 0.01)
K <- 2 
t <- 100 
B <- 0.2
p_v <- numeric(3)
vec_v <- list()
c <- 1
for (sigma in vec_sigma){
  v <- DaO_calloption(t, mu, sigma, K, B)
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