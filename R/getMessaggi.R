##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di messaggi privati
##'
##' @title conoscere il numero di messaggi privati
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return numero di messaggi privati
##' 
##' @author Davide Meneghetti

getMessaggi <- function(percorso){
  perM=paste(percorso,"/html/messages.htm", sep="")
  pg=htmlParse(perM)
  
  messaggi=length(getNodeSet(pg,"//div/span[@class='meta']/text()"))
  
  return(messaggi)
}

