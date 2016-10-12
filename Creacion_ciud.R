Pos_recursos_crear<-function(Distritos,max_numero_unidades){
#max_numero_unidades=4
#Distritos=6
papa<-vector(length = Distritos)

#max_numero_unidades=max_numero_unidades

papa.frame<-data.frame()
i=1
Posibilidades=(max_numero_unidades+1)^Distritos
cuenta=0


progreso <- txtProgressBar(0,Posibilidades,char= '#',style=3)

while (i <=length(papa)) {
  if (papa[i]==max_numero_unidades) {
    if (papa[i+1]==max_numero_unidades) {
      i=i+1
    } else { 
        papa[i+1]=papa[i+1]+1
        if (sum(papa)<=max_numero_unidades) {papa.frame<-rbind(papa.frame,papa)}
        for (j in 0:i) {
          papa[j]=0
          i=1
        }
      }
  } else {
      if (sum(papa)<=max_numero_unidades) {papa.frame<-rbind(papa.frame,papa)}
      papa[i]=papa[i]+1
      }
  if (i==length(papa) & papa[i]==max_numero_unidades) {
    if (sum(papa)<=max_numero_unidades) {papa.frame<-rbind(papa.frame,papa)}
    cuenta=cuenta+1
    setTxtProgressBar(progreso, cuenta)
    break()
  }
  cuenta=cuenta+1
  setTxtProgressBar(progreso, cuenta)
}

close(progreso)
return(papa.frame)
}
#write.csv2(papa.frame, file = "papa1.csv", fileEncoding = "UTF-16LE")
#Prueba<-Pos_recursos_crear(6, 5)