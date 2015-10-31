##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di posti in cui si è stati
##'
##' @title conoscere il numero di posti
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return numero di posti dove ci si è segnalati
##' 
##' @author Davide Meneghetti

getPosti<-function(percorso){
  perA=paste(percorso,"/html/places.htm", sep="")
  #lettura intero file
  pg=htmlParse(perA)
  nPosti=length(getNodeSet(pg,"//div[@class='contents']/div/div/ul/li/div[@class='meta']/text()"))
  return(nPosti) 
}

