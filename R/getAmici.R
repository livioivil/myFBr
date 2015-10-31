##' Funzione che dato il percorso dei dati del profilo facebook il numero di amici(per tipologia)
##' 
##' @title amici di un profilo facebook
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset interi(1x4): numero di amici accettati,
##'                              numero di richieste di amicizia effettuate ma non risposte,
##'                              numero di richieste di amicizia ricevute ma non risposte,
##'                              numero di amici rimossi
##' 
##' @author Davide Meneghetti

getAmici <- function(percorso){
  perA=paste(percorso,"/html/friends.htm", sep="")
  pg=htmlParse(perA) #lettura intero file
  
  accet=length(getNodeSet(pg,"//div[@class='contents']/div/ul[1]/li/text()")) #Numero Amici Accettati
  richi=length(getNodeSet(pg,"//div[@class='contents']/div/ul[2]/li/text()")) #Numero Richieste di Amicizia Effettuate
  ricev=length(getNodeSet(pg,"//div[@class='contents']/div/ul[3]/li/text()")) #Numero Richieste di Amicizia Ricevute 
  rimos=length(getNodeSet(pg,"//div[@class='contents']/div/ul[4]/li/text()")) #Numero Amici Rimossi
  
  #creazione dataset
  "amici" <- structure(.Data = list(accet,richi,ricev,rimos),
                          names = c("accettati", "richEff", "richRic","rimossi"),
                          row.names = c(1:1),
                          class = "data.frame")
  return(amici)
}