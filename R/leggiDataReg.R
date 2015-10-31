#funzione per leggere data registrazione
leggiDataReg <- function(percorso){
  
  perDati=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perDati)
  
  data=.getValore(pg,"//tr[th/text()='Data di registrazione']/td/text()")
  x=1
  if(data=="NULL"){
    data=.getValore(pg,"//tr[th/text()='Registration Date']/td/text()")
    x=2
  }#INGLESE
  if(data=="NULL"){
    data=.getValore(pg,"//tr[th/text()='Fecha de registro']/td/text()")
    x=3
  }#SPAGNOLO
  
  if(x==1){data=inDataIT(data)}
  else if(x==2){data=inDataGB(data)}#INGLESE
  else{data=inDataE(data)
  }#SPAGNOLO
  
  return(data)
}
