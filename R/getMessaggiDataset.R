##' Funzione che dato il percorso dei dati del profilo facebook ritorna il dataset dei messaggi privati inviati e ricevuti
##' comprendendo 3 colonne: la data, l'utente e il testo del messaggio
##'
##' @title daset dei messaggi privati
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset dei messaggi composto da data, utente autore e testo dei messaggi
##' 
##' @author Davide Meneghetti

getMessaggiDataset <- function(percorso){
  #lettura intero file
  pg=htmlParse(percorso)
  #lettura nodi file
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  user=getNodeSet(pg,"//div/span[@class='user']/text()")
  msgs=getNodeSet(pg,"///div/p/text()")
  n=length(msgs)
  
  getInfo <- function(i){
    #data e ora
    data=.estraielemento(meta[[i]])
    #utente
    uten=.estraielemento(user[[i]])
    #messaggio
    mess=.estraielemento(msgs[[i]])
    c(data=data,uten=uten,mess=mess)
  }
  
  #estrazione dati da lista
  rr=t(sapply(1:n, getInfo))
    #creazione dataset
  "messaggi" <- structure(.Data = list(rr[,"data"],rr[,"uten"],rr[,"mess"]),
                          names = c("data", "utente", "messaggio"),
                          row.names = c(1:n),
                          class = "data.frame")
  return(messaggi)
}
