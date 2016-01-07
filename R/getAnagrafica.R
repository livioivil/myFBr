##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna l'insieme dei dati di un profilo in un'unica riga di dataset
##' compresi fra 2 date in particolare restituisce: sesso, n. di sessioni e di accessi, n. di amici di varia categoria, n. di post,
##' n. di attivit? del wall delle varie categorie, n. di eventi con varia risposta, n. di foto, n. di posti e n. di messaggi privati.
##' 
##' @title conoscere le informazioni del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return dataset (1x20) contenente le informazioni del profilo compresi fra 2 date
##' @export
##' @title getAnagrafica
##' 
##' @author Davide Meneghetti

getAnagrafica <- function(percorso,dataI=-Inf,dataF=+Inf){
  percorso=.fixPercorso(percorso)
  
  info=getInfo(percorso)
  #RIVEDERE:
#   accessi=getNAccessi(percorso,dataI,dataF)#
  amici=getFriends(percorso)#
  nEventi=getEvents(percorso,dataI,dataF)#
  nFoto=getNPhotos(percorso,dataI,dataF)#
  mess=getNMessages(percorso,dataI,dataF)# 
  wall=getWall(percorso,dataI,dataF)#
  nPosti=getPlaces(percorso,dataI,dataF)#
  dati<-cbind(info,#accessi,
              amici,wall,nEventi,nFoto,nPosti,mess)
  rownames(dati)=percorso
  return(dati)
}