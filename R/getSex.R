##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna l'indicazione del sesso
##' ##'
##' @title indicazione del sesso del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return F,M
##' @export
##' @title getSex
##' 
##' @author Davide Meneghetti, Livio Finos

getSex <- function(percorso){
  percorso=.fixPercorso(percorso)
  
  perE=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perE)#lettura intero file
  sesso=getNodeSet(pg,"//tr[th[text()='Sesso']]/td/text()");
  sesso = .cleanSex(sesso)
  return(sesso)
}#getSesso
