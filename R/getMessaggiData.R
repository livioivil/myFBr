##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di messaggi privati dopo una data di riferimento
##'
##' @title conoscere il numero di messaggi privati dopo una data
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRef data di riferimento
##' @return numero di messaggi privati successi alla data di riferimento indicata
##' 
##' @author Davide Meneghetti

getMessaggiData <- function(percorso,dataRef){
  perM=paste(percorso,"/html/messages.htm", sep="")
  #lettura intero file
  pg=htmlParse(perM)
  #lettura nodi file
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  
  n=length(meta)
  messaggi=NA
  for(i in 1:n){
    temp=inDataIT(as.character(.estraielemento(meta[[i]])))
    if(!is.na(temp) && temp >= dataRef){
      if(length(messaggi)<1){
        messaggi=temp
      }else{
        messaggi=cbind(messaggi,as.Date(temp))
      }
    }
  }
  return(length(messaggi))
}

