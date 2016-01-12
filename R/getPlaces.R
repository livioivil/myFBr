##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di posti in cui si ? stati compresi fra 2 date
##'
##' @title conoscere il numero di posti
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di posti dove ci si ? segnalati dopo la data di riferimento compresi fra 2 date
##' @export
##' @title getPlaces
##' @author Davide Meneghetti, Livio Finos

getPlaces <-function(percorso,dataI,dataF){
  percorso=fixPath(percorso)
  perA=paste(percorso,"/html/places.htm", sep="")
  if(!("places.htm"%in%dir(paste(percorso,"/html", sep=""))))
    return(NA)
  #lettura intero file
  pg=htmlParse(perA)
  
  place=getNodeSet(pg,"//div[@class='contents']/div/div/ul/li/div[@class='meta']/text()")
  n=length(place)
  nPosti=0
  if(n>0){
    for(i in 1:n){
      temp=inDataIT(as.character(.estraielemento(place[[i]])))
      if(temp >= dataI && temp <= dataF){
        nPosti=nPosti+1
      }
    }
  }
  return(nPosti)
}
