##' Funzione che dato il percorso dei dati del profilo facebook ritorna l'insieme dei dati di un profilo in un'unica riga di dataset
##' dopo una certa data
##' in particolare restituisce: sesso, n. di sessioni e di accessi, n. di amici di varia categoria, n. di post,
##' n. di attività del wall delle varie categorie, n. di eventi con varia risposta, n. di foto, n. di posti e n. di messaggi privati.
##' 
##' @title conoscere le informazioni del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param data data di riferimento
##' @return dataset (1x20) contenente le informazioni del profilo dopo la data di rifermento
##'
##' @author Davide Meneghetti

getAnagraficaData <- function(percorso,data){
  sesso=getSesso(percorso)
  if(sesso=="Uomo")
  {sesso=1}
  accessi=getNAccessiData(percorso,data)#
  amici=getAmici(percorso)#
  nPost=getNPostData(percorso,data)#
  nEventi=getEventiData(percorso,data)#
  nFoto=getNFotoData(percorso,data)#
  mess=getMessaggiData(percorso,data)# 
  wall=getWallData(percorso,data)#
  nPosti=getPostiData(percorso,data)#
  dataReg=getDataReg(percorso)
  dati<-cbind(sesso,accessi,amici,nPost,wall,nEventi,nFoto,nPosti,mess,dataReg)
  return(dati)
}


