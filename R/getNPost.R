##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di post sulla bacheca compresi fra 2 date
##' 
##' @title conoscere il numero di post
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di post scritti sulla bacheca compresi fra 2 date
##' @export
##' @title getNPost

##' @author Davide Meneghetti

getNPost <- function(percorso,dataI,dataF){
  perW=paste(percorso,"/html/wall.htm", sep="")
  pg=htmlParse(perW)
  atti=getNodeSet(pg,"//div[@class='meta']/text()")
  if(length(atti)==0) return(NA)
  temp=sapply(atti,.estraielemento)
  temp=inDataIT(temp)
  nposts=length(.which.within.date(temp,dataI,dataF))
  return(nposts)
}