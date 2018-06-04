
library(ggplot2)

# Metropolis - Hastings Normal

MetropolisNr <- function(n,x0){
  x = c()
  x[1] <- x0
  for (ii in 2:n){
    xt <- x[ii-1]
    yt <- rnorm(1,xt,1)
    num <- dcauchy(yt,0,1)* dnorm(xt,yt,1)
    den <- dcauchy(xt,0,1)* dnorm(yt,xt,1)
    u <- runif(1)
    if (u <= num/den){
      x[ii] <- yt
    }else{
      x[ii] <- xt
    }
  }
  return(x)
}

X_Construida <- MetropolisNr(100,0)
X_teorica    <- rcauchy(100,0,1)

X <-c(X_Construida,X_teorica)
Y <- rep(c("Metropolis Normal","teorica"),c(100,100))

Data1 <-   data.frame(X,Y)
ggplot(Data1, aes(X, fill = Y, colour = Y)) +
  geom_density(alpha = 0.1) +
  xlim(-12, 12)

# Metropolis - Hastings Independiente

n = 100
x0 = 0

MetropolisInd <- function(n,x0){
x = c(x0)
for(i in 1:n){
  X_t = x[i]
  Y_t = rnorm(1,0,1)
  U = runif(1,0,1)
  Comparacion = (dcauchy(Y_t,0,1)*dnorm(X_t,0,1))/(dcauchy(X_t,0,1)*dnorm(Y_t,0,1))
  if(U <= Comparacion){
    x[i+1] = Y_t
  }else{
    x[i+1] = X_t
  }
}
return(x[-1])
}

MetropolisInd <-  MetropolisInd(100,0)
X_Construida <- MetropolisNr(100,0)
X_teorica    <- rcauchy(100,0,1)

X <-c(X_Construida,X_teorica,MetropolisInd)
Y <- rep(c("Metropolis Normal","teorica","Metropolis Independiente"),c(100,100,100))

Data1 <-   data.frame(X,Y)
ggplot(Data1, aes(X, fill = Y, colour = Y)) +
  geom_density(alpha = 0.1) +
  xlim(-12, 12)


# Metropolis-Hastings Random Walk

rw.Metropolis <- function(n,x0, k, lambda) {
  x <- numeric(n)
  x[1] <- x0
  u <- runif(n)
  w <- 0
  for (i in 2:n) {
    y <- rgamma(1, k, x[i-1])
    if (u[i] <= (dexp(y,rate = lambda) /dexp(x[i-1],rate = lambda))){
      x[i] <- y
    }else{
      x[i] <- x[i-1]
      w <- w + 1
    }
  }
  return(w)
}

k = 1

Estudio_Simulacion <-function(k){

  a =rw.Metropolis(n=1000,x0= 3.5,k,lambda = 3.5)

  return(a[2][1])

}

X <- seq(0,100,0.01)
Y <- c()

for(i in 2:1001){
  Y_other <- Estudio_Simulacion(X[i])
  Y <- c(Y,Y_other)
}

plot(X[-1],Y)