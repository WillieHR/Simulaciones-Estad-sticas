### Tarea Convulsion
### Willie Hernandez

rm(list = ls())
cat("\014")

n <- 100
nu <- 20
X <- matrix(rgeom(n*nu,0.3),n,nu)
y_construida <- rowSums(X)

y_teorica    <- rnbinom(100,20,0.3)

X <-c(y_construida,y_teorica)
Y <- rep(c("Metodo Convulsion","Distribucion teorica"),c(100,100))

Data1 <-   data.frame(X,Y)
ggplot(Data1, aes(X, fill = Y, colour = Y)) +
  geom_density(alpha = 0.1) 


