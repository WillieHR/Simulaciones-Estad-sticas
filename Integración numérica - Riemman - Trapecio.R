Interpolacion=function(x,y,x0)
{
  n=length(x)
  m=length(y)
  if(n!=m){
    stop("Las longitudes de los vectores deben ser iguales")
  } else{
    p=c()
    for(i in 1:n)
    {
      p[i]=prod((x0-x[-i])/(x[i]-x[-i]))
    }
    sum(y*p)
  }
}

x=c(1,3,4)
y=c(2,1,7)

x0=seq(1,4,0.01)
y0=c()

for(i in seq_along(x0))
{
  y0[i]=Interpolacion(x,y,x0[i])
}

windows()
plot(x0,y0,type = "l",col="red",ylim=c(min(y0),max(y0)))
par(new=T)
plot(x,y,pch=7,col="blue",ylim=c(min(y0),max(y0)))

#############################################

Riemann=function(f,a,b,n)
{
  x=seq(a,b,length.out = (n+1))
  r=c()
  for(i in 1:n)
  {
    r[i]=(b-a)/n * f(x[i])
  }
  sum(r)
}

f=function(x)
{
  x^2
}

Riemann(f,0,1,10)

Trapecio=function(f,a,b)
{
  (b-a)/2 * (f(a)+f(b))
}

Trapecio_Compuesta=function(f,a,b,n)
{
  x=seq(a,b,length.out = (n+1))
  r=c()
  for(i in 1:n)
  {
    r[i]=Trapecio(f,x[i],x[i+1])
  }
  sum(r)
}

Trapecio_Compuesta(f,0,1,10)









