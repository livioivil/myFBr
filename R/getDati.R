##' Funzione che dato il percorso dei dati del profilo facebook restituisce una serie di informazioni sul profilo
##' 
##' @title Dati di un profilo facebook
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return diverse informazioni
##' 
##' @author Davide Meneghetti



getDati <- function(percorso){
  a=getAnagrafica(percorso)
  per=paste(percorso,"/html/messages.htm", sep="")
  m=getMessaggi(per)
  return(c(a,m))
}
