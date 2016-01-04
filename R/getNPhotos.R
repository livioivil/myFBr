##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di foto comprese fra 2 date
##'
##' @title conoscere il numero di foto
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di foto pubblicate dal profilo dopo la data di riferimento comprese fra 2 date
##' @export
##' @title getNPhotos
##' @author Davide Meneghetti

getNPhotos <- function(percorso,dataI, dataF){
  percorso=.fixPercorso(percorso)
  #numero di foto
  perF=paste(percorso,"/html/photos.htm", sep="")
  
  #lettura intero file
  pg=htmlParse(perF)
  dataL=getNodeSet(pg,"//div[@class='contents']/div[@class='block']/div/div[@class='meta']/text()") #data foto

  dataL=sapply(dataL,.estraielemento)
  
  nfoto=length(.which.within.date(inDataIT(dataL),dataI, dataF))
  return(nfoto)
}
