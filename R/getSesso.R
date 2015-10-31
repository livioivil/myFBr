##' Funzione che dato il percorso dei dati del profilo facebook ritorna l'indicazione del sesso
##' ##'
##' @title indicazione del sesso del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return numero: 0 se femmina, 1 se maschio
##' 
##' @author Davide Meneghetti

getSesso <- function(percorso){
  perE=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perE)#lettura intero file
  sesso=getNodeSet(pg,"//tr[th[text()='Sesso']]/td/text()");
  #if (length(sesso)==0){    #INGLESE
  #  sesso=getNodeSet(pg,"//tr[th[text()='Gender']]/td/text()");
  #}
  #if (length(sesso)==0){    #SPAGNOLO
  #  sesso=getNodeSet(pg,"//tr[th[text()='Sexo']]/td/text()");
  #}
  ses=.estraielemento(sesso[[1]])
  ses=gsub(" ","",ses)
  return(ses)
}#getSesso