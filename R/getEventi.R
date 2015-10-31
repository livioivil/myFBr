##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di eventi di vario tipo
##' tipi di evento: confermato, forse, rifiutato, non risposto.
##' Questa fuzione è parzialmente funzionante anche con i profili in lingua inglese e spagnola
##' 
##' @title conoscere il numero di eventi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset (1x4) contenente il numero dei vari tipi di eventi
##'
##' @author Davide Meneghetti

getEventi <- function(percorso){
  #numero eventi
  perE=paste(percorso,"/html/events.htm", sep="")
  #lettura intero file
  pg=htmlParse(perE)
  eventC=length(getNodeSet(pg,"//ul/li/p[text()='Partecipazione confermata']"))
  eventF=length(getNodeSet(pg,"//ul/li/p[text()='Forse']"))
  eventR=length(getNodeSet(pg,"//ul/li/p[text()='Partecipazione rifiutata']"))
  eventN=length(getNodeSet(pg,"//ul/li/p[text()='Nessuna risposta']"))
 
  #INGLESE
  if(eventC==0){eventC=length(getNodeSet(pg,"//ul/li/p[text()='Confirmed']"))}
  #if(eventF==0){eventF=length(getNodeSet(pg,"//ul/li/p[text()='Perhaps']"))}
  if(eventR==0){eventR=length(getNodeSet(pg,"//ul/li/p[text()='Declined']"))}
  if(eventN==0){eventN=length(getNodeSet(pg,"//ul/li/p[text()='No reply']"))}
  
  #SPAGNOLO
  if(eventC==0){eventC=length(getNodeSet(pg,"//ul/li/p[text()='Asistir']"))}
  if(eventF==0){eventF=length(getNodeSet(pg,"//ul/li/p[text()='Tal vez asista']"))}
  if(eventR==0){eventR=length(getNodeSet(pg,"//ul/li/p[text()='Invitacin rechazada']"))}
  if(eventN==0){eventN=length(getNodeSet(pg,"//ul/li/p[text()='Sin respuesta']"))}
  
  #creazione dataset
  "eventi" <- structure(.Data = list(eventC,eventF,eventR,eventN),
                        names = c("eventiConf", "eventiForse", "eventiRif","eventiNoRisp"),
                        row.names = c(1:1),
                        class = "data.frame")
  return(eventi)
}