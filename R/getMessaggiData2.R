##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di messaggi privati compresi fra 2 date
##'
##' @title conoscere il numero di messaggi privati dopo una data
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di messaggi privati compresi fra 2 date
##' 
##' @author Davide Meneghetti

getMessaggiData2 <- function(percorso,dataI,dataF){
  perM=paste(percorso,"/html/messages.htm", sep="")
  #lettura intero file
  pg=htmlParse(perM)
  #lettura nodi file
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  
  n=length(meta)
  messaggi=NA
  for(i in 1:n){
    temp=inDataIT(as.character(.estraielemento(meta[[i]])))
    if(!is.na(temp) && temp >= dataI && temp <= dataF){
      if(length(messaggi)<1){
        messaggi=temp
      }else{
        messaggi=cbind(messaggi,as.Date(temp))
      }
    }
  }
  return(length(messaggi))
}
