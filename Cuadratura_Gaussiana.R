U=function(vector)
{
  w1=vector[1]
  w2=vector[2]
  x1=vector[3]
  x2=vector[4]
  c(w1+w2-2,w1*x1+w2*x2,w1*x1^2+w2*x2^2-2/3,
    w1*x1^3+w2*x2^3)
}

H=function(vector)
{
  w1=vector[1]
  w2=vector[2]
  x1=vector[3]
  x2=vector[4]
  matrix(c(1,1,0,0,
           x1,x2,w1,w2,
           x1^2,x2^2,2*w1*x1,2*w2*x2,
           x1^3,x2^3,3*w1*x1^2,3*w2*x2^2),
         byrow=T,ncol=4)
}

Newton.Multivariado=function(ini,U,H,eps)
{
  x=matrix(NA,ncol=1,nrow=length(ini))
  x[,1]=ini
  i=1
  w=0
  while(w==0)
  {
    aux=x[,i]-solve(H(x[,i]))%*%U(x[,i])
    x=cbind(x,aux)
    if(sqrt(sum((x[,i+1]-x[,i])^2))<eps)
    {
      w=1
    } else{
      i=i+1
    }
  }
  cat("El algoritmo terminó en", ncol(x), "iteraciones\n")
  x[,ncol(x)]
}

Pesos.Cuadratura=Newton.Multivariado(c(0.5,0.5,-0.2,0.4),U,H,1e-4)

Cuadratura.Gauss=function(f)
{
  w1=Pesos.Cuadratura[1]
  w2=Pesos.Cuadratura[2]
  x1=Pesos.Cuadratura[3]
  x2=Pesos.Cuadratura[4]
  w1*f(x1)+w2*f(x2)
}

f=function(x)
{
  exp(x)
}

Cuadratura.Gauss(f)

f2=function(x)
{
  1/sqrt(2*pi)*exp(-0.5*(3*x)^2)*3
}

Cuadratura.Gauss(f2)




