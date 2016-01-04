##' DA RIVEDERE
##' Funzione che dato il percorso dei dati del profilo facebook ritorna il dataset dei messaggi privati inviati e ricevuti
##' comprendendo 3 colonne: la data, l'utente e il testo del messaggio
##'
##' @title daset dei messaggi privati
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset dei messaggi composto da data, utente autore e testo dei messaggi
##' @export 
##' @title getMessagesTexts
##' @author Davide Meneghetti, Livio Finos

getMessagesTexts <- function(percorso){
  percorso=.fixPercorso(percorso)
  #lettura intero file
  perM=paste(percorso,"/html/messages.htm", sep="")
  pg=htmlParse(perM)
  #lettura nodi file
  mess=getNodeSet(pg,"//p/text()")
  mess=sapply(mess,.estraielemento)
  return(mess)
}
