#### BONDAD DE AJUSTE
# Prueba de Bonda de Ajuste Chi Cuadrado

k = 10
ChiCuadrado  <- function(Y,k){
  
  n = length(Y)
  particiones <- cut(Y, breaks = k,by = 1/k,labels = 1:k)
  dat <- data.frame(Y,intervalo = particiones)
  fi <- 0
  for(i in 1:k){
    fi[i] = nrow(subset(dat, intervalo == i))
  }
  chi <- sum(((fi-n/k)^2)/(n/k))
  pvalue = pchisq(chi,df = k - 1)
  
  cat("La probabilidad de que los datos tengan una distribucion
       uniforme en el intervalo (0,1) es",pvalue*100,"%.")
  
}

ChiCuadrado(Y,10)

# Kolmogorov Smirnov
x <- rnorm(50)
ks.test()