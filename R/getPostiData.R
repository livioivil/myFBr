##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di posti in cui si è stati dopo una certa data
##'
##' @title conoscere il numero di posti
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRef data di riferimento
##' @return numero di posti dove ci si è segnalati dopo la data di riferimento
##' 
##' @author Davide Meneghetti

getPostiData<-function(percorso,dataRef){
  perA=paste(percorso,"/html/places.htm", sep="")
  #lettura intero file
  pg=htmlParse(perA)
  
  place=getNodeSet(pg,"//div[@class='contents']/div/div/ul/li/div[@class='meta']/text()")
  n=length(place)
  nPosti=0
  if(n>0){
    for(i in 1:n){
      temp=inDataIT(as.character(.estraielemento(place[[i]])))
      if(temp >= dataRef){
        nPosti=nPosti+1
      }
    }
  }
  return(nPosti)
}

