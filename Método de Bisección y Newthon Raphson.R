f=function(x)
{
  log(x)-sin(x^2)
}

x=seq(0.1,3,0.01)
y=c()

for(i in seq_along(x))
{
  y[i]=f(x[i])
}

plot(x,y,type="l")
abline(h=0,col="red")

curve(f,1,2.5)

Biseccion=function(a,b,f,eps)
{
  x=c()
  x[1]=(a+b)/2
  i=1
  w=0
  while (w==0) {
    aux1=f(a)
    aux2=f(x[i])
    if(aux1*aux2<0)
    {
      b=x[i]
    } else {
      a=x[i]
    }
    x[i+1]=(a+b)/2
    if(abs(x[i+1]-x[i])<eps)
    {
      w=1
    } else{
      i=i+1
    }
  }
  cat("El algoritmo terminó en",  length(x), "iteraciones\n")
  x[length(x)]
}

Biseccion(1,2.5,f,1e-15)

Newton=function(ini,f,fprima,eps)
{
  x=c()
  x[1]=ini
  i=1
  w=0
  while (w==0) {
    x[i+1]=x[i]-f(x[i])/fprima(x[i])
    if(abs(x[i+1]-x[i])<eps)
    {
      w=1
    } else{
      i=i+1
    }
  }
  cat("El algoritmo terminó en",  length(x), "iteraciones\n")
  x[length(x)]
}

D(expression(log(x)-sin(x^2)),"x")


fprima=function(x)
{
  1/x-cos(x^2)*(2*x)
}

Newton(2,f,fprima,1e-5)

#####

system.time(Biseccion(1,2.5,f,1e-10))
system.time(Newton(1.6,f,fprima,1e-10))











