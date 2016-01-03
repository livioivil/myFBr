##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di eventi di vario tipo compresi fra 2 date
##' tipi di evento: confermato, forse, rifiutato, non risposto.
##' 
##' @title conoscere il numero di eventi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return dataset (1x4) contenente il numero dei vari tipi di eventi dopo la data di riferimento
##' @export
##' @title getEvents

##' @author Davide Meneghetti

getEvents <- function(percorso, dataI, dataF){
  #numero eventi
  perE=paste(percorso,"/html/events.htm", sep="")
  #lettura intero file
  pg=htmlParse(perE)
  
  eve=getNodeSet(pg,"//ul/li/p")
  eve=sapply(eve,.estraielemento)
  eve=sapply(eve,function(txt) {
    res=strsplit(txt," - ")
    res[[1]][length(res[[1]])]})
  attributes(eve)<-NULL
  
  keep=.which.within.date(inDataIT(eve),dataI, dataF) 
  eve=eve[keep]
  
  
  ans.type=c('Partecipazione confermata',
  'Forse','Partecipazione rifiutata','Nessuna risposta')
  
  res=sapply(ans.type, function(txt) length(grep(txt,eve)))
  
  #creazione dataset
  "eventi" <- structure(.Data = as.list(res),
                        names = c("eventiConf", "eventiForse", "eventiRif","eventiNoRisp"),
                        row.names = c(1:1),
                        class = "data.frame")
  return(eventi)
}