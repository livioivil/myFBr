##' Funzione che dato il percorso dei dati del profilo facebook ritorna la data di registrazione
##' 
##' @title data di registrazione del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return data: data di registrazione del profilo 
##' 
##' @author Davide Meneghetti

#funzione per conoscere la data di registrazione
getDataReg <- function(percorso){
  perDati=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perDati) #lettura intero file
  
  data=.getValore(pg,"//tr[th/text()='Data di registrazione']/td/text()")
 
  return(inDataIT(data))
}