##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di eventi di vario tipo dopo una certa data
##' tipi di evento: confermato, forse, rifiutato, non risposto.
##' 
##' @title conoscere il numero di eventi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRef data di riferimento
##' @return dataset (1x4) contenente il numero dei vari tipi di eventi dopo la data di riferimento
##'
##' @author Davide Meneghetti

getEventiData <- function(percorso,dataRef){
  #numero eventi
  perE=paste(percorso,"/html/events.htm", sep="")
  #lettura intero file
  pg=htmlParse(perE)
  
  eve=getNodeSet(pg,"//ul/li/p[text()='Partecipazione confermata']/text()")
  n=length(eve)
  
  temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[1]]))))  
  eventC=temp
  for(i in 2:n){
    temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[i]]))))
    if(!is.na(temp) && temp >= dataRef){
      eventC=cbind(eventC,as.Date(temp))
    }
  }
  
  eve=getNodeSet(pg,"//ul/li/p[text()='Forse']/text()")
  n=length(eve)
  
  temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[1]]))))  
  eventF=temp
  for(i in 2:n){
    temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[i]]))))
    if(!is.na(temp) && temp >= dataRef){
      eventF=cbind(eventF,as.Date(temp))
    }
  }
  
  eve=getNodeSet(pg,"//ul/li/p[text()='Partecipazione rifiutata']/text()")
  n=length(eve)
  
  temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[1]]))))  
  eventR=temp
  for(i in 2:n){
    temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[i]]))))
    if(!is.na(temp)){
      eventR=cbind(eventR,as.Date(temp))
    }
  }
  
  eve=getNodeSet(pg,"//ul/li/p[text()='Nessuna risposta']/text()")
  n=length(eve)
  
  temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[1]]))))  
  eventN=temp
  for(i in 2:n){
    temp=inDataIT(gsub("[^\\w]* - ","", as.character(.estraielemento(eve[[i]]))))
    if(!is.na(temp) && temp >= dataRef){
      eventN=cbind(eventN,as.Date(temp))
    }
  }
   
  #creazione dataset
  "eventi" <- structure(.Data = list(length(eventC),length(eventF),length(eventR),length(eventN)),
                        names = c("eventiConf", "eventiForse", "eventiRif","eventiNoRisp"),
                        row.names = c(1:1),
                        class = "data.frame")
  return(eventi)
}