##' Funzione che dato il percorso dei dati del profilo facebook ritorna l'insieme dei dati di un profilo in un'unica riga di dataset
##' compresi fra 2 date
##' in particolare restituisce: sesso, n. di sessioni e di accessi, n. di amici di varia categoria, n. di post,
##' n. di attività del wall delle varie categorie, n. di eventi con varia risposta, n. di foto, n. di posti e n. di messaggi privati.
##' 
##' @title conoscere le informazioni del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return dataset (1x20) contenente le informazioni del profilo compresi fra 2 date
##' 
##' @author Davide Meneghetti

getAnagraficaData2 <- function(percorso,dataI,dataF){
  sesso=getSesso(percorso)
  if(sesso=="Uomo")
    {sesso=1}
  accessi=getNAccessiData2(percorso,dataI,dataF)#
  amici=getAmici(percorso)#
  nPost=getNPostData2(percorso,dataI,dataF)#
  nEventi=getEventiData2(percorso,dataI,dataF)#
  nFoto=getNFotoData2(percorso,dataI,dataF)#
  mess=getMessaggiData2(percorso,dataI,dataF)# 
  wall=getWallData2(percorso,dataI,dataF)#
  nPosti=getPostiData2(percorso,dataI,dataF)#
  dataReg=getDataReg(percorso)
  dati<-cbind(sesso,accessi,amici,nPost,wall,nEventi,nFoto,nPosti,mess,dataReg)
  return(dati)
}

