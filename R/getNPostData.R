##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di post sulla bacheca dopo una certa data
##' 
##' @title conoscere il numero di post
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRef data di riferimento
##' @return numero di post scritti sulla bacheca dopo la data di riferimento
##'
##' @author Davide Meneghetti

getNPostData <- function(percorso,dataRef){
  perW=paste(percorso,"/html/wall.htm", sep="")
  pg=htmlParse(perW)
  atti=getNodeSet(pg,"//div[@class='meta']/text()")
  n=length(atti)
  
  post=0
  for(i in 1:n){
    temp=inDataIT(as.character(.estraielemento(atti[[i]])))
    if(temp >= dataRef){
      post=post+1
    }
  }
  
  return(post)
}