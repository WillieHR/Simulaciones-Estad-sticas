
### Bootstrap

data("faithful")

duracion <- faithful$eruptions
waiting <- faithful$waiting

list_duracion = c()
for(i in 1:500){
  list_duracion[i] = list(sample(duracion,length(duracion),replace = TRUE))
}

list_waiting = c()
for(i in 1:500){
  list_waiting[i] = list(sample(waiting,length(waiting),replace = TRUE))
}

L_duracion_mean =  unlist(lapply(list_duracion,mean),recursive=F)

mean(L_duracion_mean)
var(L_duracion_mean)

L_duracion_var = unlist(lapply(list_duracion,var),recursive=F)

mean(L_duracion_var)
var(L_duracion_var)


### Jackniffe
  
  list_duracion = c()
  for(i in 1:length(duracion)){
    list_duracion[i] = list(duracion[-i])
  }
  
  list_waiting = c()
  for(i in 1:length(waiting)){
    list_waiting[i] = list(waiting[-i])
  }
  
  L_duracion_mean =  unlist(lapply(list_duracion,mean),recursive=F)
  
  mean(L_duracion_mean)
  var(L_duracion_mean)
  
  L_duracion_var = unlist(lapply(list_duracion,var),recursive=F)
  
  mean(L_duracion_var)
  var(L_duracion_var)