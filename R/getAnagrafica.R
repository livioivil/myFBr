##' Funzione che dato il percorso dei dati del profilo facebook ritorna l'insieme dei dati dell'utente in un'unica riga di dataset
##' in particolare restituisce: sesso, n. di sessioni e di accessi, n. di amici di varia categoria, n. di post,
##' n. di attività del wall delle varie categorie, n. di eventi con varia risposta, n. di foto, n. di posti e n. di messaggi privati.
##' 
##' @title conoscere le informazioni del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset (1x20) contenente le informazioni del profilo
##'
##' @author Davide Meneghetti

getAnagrafica <- function(percorso){
  sesso=getSesso(percorso)
  if(sesso=="Uomo")
    {sesso=1}
  else{sesso=0}
  accessi=getNAccessi(percorso)
  amici=getAmici(percorso)
  nPost=getNPost(percorso)
  nEventi=getEventi(percorso)
  nFoto=getNFoto(percorso)
  mess=getMessaggi(percorso)
  wall=getWall(percorso)
  nPosti=getPosti(percorso)
  dataReg=getDataReg(percorso)  
   
  dati<-cbind(sesso,accessi,amici,nPost,wall,nEventi,nFoto,nPosti,mess,dataReg)
  return(dati)
}



