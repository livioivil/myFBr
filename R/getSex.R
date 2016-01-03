##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna l'indicazione del sesso
##' ##'
##' @title indicazione del sesso del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return F,M
##' @export
##' @title getSex
##' 
##' @author Davide Meneghetti

getSex <- function(percorso){
  perE=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perE)#lettura intero file
  sesso=getNodeSet(pg,"//tr[th[text()='Sesso']]/td/text()");
  if(is.null(sesso)) return(NA)
  #if (length(sesso)==0){    #INGLESE
  #  sesso=getNodeSet(pg,"//tr[th[text()='Gender']]/td/text()");
  #}
  #if (length(sesso)==0){    #SPAGNOLO
  #  sesso=getNodeSet(pg,"//tr[th[text()='Sexo']]/td/text()");
  #}
  ses=.estraielemento(sesso[[1]])
  ses=gsub(" ","",ses)
  if(ses=="Uomo")
  {ses="M"}
  if(ses=="Donna")
  {ses="F"}
  return(ses)
}#getSesso