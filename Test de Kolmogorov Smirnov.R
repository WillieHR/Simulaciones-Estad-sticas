X <- rnorm(1000,mean=0,sd =1)

Xord = sort(X)
Media = mean(X)
Desviacion = sd(X)
n.x = length(X)
Ones = rep(1,n.x)
cumX = cumsum(Ones)
PcumX = cumX / n.x
Z_score = (Xord - Media)/Desviacion
F_x = pnorm(Z_score)
Diff = F_x - PcumX
Statistic = max(Diff)

X <- c(1,2,2,3,3,3,3,4,5,6)

miu = 10 
sigma = 4

Xord = sort(X)
Media = mean(X)
Desviacion = sd(X)
n.x = length(X)
Ones = rep(1,n.x)
cumX = cumsum(Ones)
PcumX = cumX / n.x
Z_score = (Xord - Media)/Desviacion
F_x = pnorm(Z_score,mean = miu, sd = sigma)
Diff = F_x - PcumX
Statistic = max(Diff)