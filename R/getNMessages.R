##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di messaggi privati compresi fra 2 date
##'
##' @title conoscere il numero di messaggi privati dopo una data
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di messaggi privati compresi fra 2 date
##' @export
##' @title getNMessages
##' 
##' @author Davide Meneghetti

getNMessages <- function(percorso,dataI,dataF){
  percorso=.fixPercorso(percorso)
  perM=paste(percorso,"/html/messages.htm", sep="")
  #lettura intero file
  pg=htmlParse(perM)
  #lettura nodi file
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  
  
  meta=sapply(meta,.estraielemento)
  
  nmess=length(.which.within.date(inDataIT(meta),dataI, dataF))
  return(nmess)
}
