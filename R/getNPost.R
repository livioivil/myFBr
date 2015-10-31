##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di post sulla bacheca
##' 
##' @title conoscere il numero di post
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return numero di post scritti sulla bacheca
##'
##' @author Davide Meneghetti

getNPost <- function(percorso){
  perW=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perW)
  atti=length(getNodeSet(pg,"//div[@class='meta']")); #Numero attivita sulla bacheca
  return(atti)
}