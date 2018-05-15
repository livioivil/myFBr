##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero delle differenti attivit? che compaiono sul wall
##' tra cui amicizie, stati, like, piace, condivisioni, link e giocato dopo una certa data.
##' Funzione NON valida per profili scritti in lingua inglese o spagnola, facile da aggiornare
##'
##' @title conoscere il numero delle varie attivit? sul wall
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di Inizio (-Inf by default)
##' @param dataF data di Fine (+Inf by default)
##' @param wall result of a call of \code{getWall}.
##' @return getWall un dataset a con tre colonne: time, user, text
##' getWall_summary: dataset (1x7) contenente le informazioni delle attivit? sul Wall dopo la data di riferimento
##' @export getWall getWall_summary
##' @title getWall
##' @aliases getWall_summary
##' 
##' @author Davide Meneghetti, Livio Finos

#funzione per leggere tutte le attivita' del wall
getWall_summary <- function(wall){
  cerca.testo=c(w_amicizia="hanno stretto amicizia",
                w_stato="ha aggiornato il suo stato",
                w_piace =  " piace ",
                w_condiviso= "ha condiviso la foto",
                w_linkAltri= "ha pubblicato un link sul tuo diario.",
                w_linkTuo="ha condiviso un link.",
                w_partecipato="ha partecipato a",
                w_postInBacheca="scritto sul tuo diario",
                w_aggiornImgProf="aggiornato la sua immagine del profilo",
                w_nuovaFoto="aggiunto una nuova foto all\'album")
  
  n=nrow(wall)
  
  res=sapply(cerca.testo, function(txt) length(grep(txt,wall$action)))
  res=c(res,w_postTotali=n)
  
  res=c(list(azioni=res),summary_freq_events_week_hour(wall))
  #creazione dataset
  
  return(res)  
}

######################
getWall <- function(percorso=".",dataI=NULL, dataF=NULL){
  percorso=myFBr:::.fixPercorso(percorso)
  percorso=.getWallPath(percorso)
  #lettura intero file
  dumFun <- function(x){
    # xname <- xmlName(x)
    # xattrs <- xmlAttrs(x)
    sapply(xmlChildren(x), xmlValue)
  }
  dum <- XML::xmlParse(percorso)
  out=xpathSApply(dum, "//div[@class='contents']/div/p", dumFun)
  if(length(out)==0){ 
    temp=data.frame(time=as.POSIXct("2015-10-11 22:10:00"),action="",text="")
    return(temp[-1,])
  } else{
    res=sapply(out,
               function(x) c(time=x[[1]],action=ifelse(length(x)>1,x[[2]],NA),text=ifelse(length(x)>2,x[[3]],NA)))
    res=data.frame(t(res),stringsAsFactors = FALSE)
    
    res$time=inDataIT(res$time)
    keep=.which.within.date(res$time,dataI, dataF)
    res=res[keep,]
    return(res) 
  } 
}
