##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna la data di download dei dati
##' 
##' @title data di registrazione del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return data: data di registrazione del profilo 
##' @export getDataDownload
##' @author Livio Finos

#funzione per conoscere la data di registrazione
getDataDownload <- function(percorso){
  percorso=.fixPercorso(percorso)
  perDati=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perDati) #lettura intero file
  
  dataDWN=.getDataDownload(pg)
  return(dataDWN)
}

.getDataDownload <- function(pg){
  dataDWN=getNodeSet(pg,"//div[@class='footer']/text()")
  dataDWN=.estraielemento(dataDWN[[1]])
  dataDWN=gsub("Scaricato da .+ il ","",dataDWN)
  dataDWN=inDataIT(dataDWN)
  dataDWN
}