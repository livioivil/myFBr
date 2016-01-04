##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di foto
##'
##' @title conoscere il numero di foto
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return numero di foto pubblicate dal profilo
##' 
##' @author Davide Meneghetti

getNFoto <- function(percorso){
  percorso=.fixPercorso(percorso)
  #numero di foto
  perF=paste(percorso,"/html/photos.htm", sep="")
  #lettura intero file
  pg=htmlParse(perF)
  foto=length(getNodeSet(pg,"//div[@class='contents']/div[@class='block']")); #Numero foto
  return(foto)
}