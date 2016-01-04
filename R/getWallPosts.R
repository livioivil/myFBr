##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna i post pubblicati sulla bacheca
##'
##' @title Post scritti sul wall
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return vettore di testi
##' @export
##' @title getWallPosts
##' 
##' @author Davide Meneghetti, Livio Finos

#funzione per leggere tutte le attivita' del wall
getWallPosts <- function(percorso){
  percorso=.fixPercorso(percorso)
  
  
  perW=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perW)  
  #lettura sezione file
  wall=getNodeSet(pg,"//div/div[@class='comment']/text()")
  wall=sapply(wall,.estraielemento)
  return(wall)  
}
