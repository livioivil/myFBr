##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di messaggi privati compresi fra 2 date
##'
##' @title estre (il numero d)i messaggi privati
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di messaggi privati compresi fra 2 date
##' @export getMessages getNMessages
##' @name getMessages
##' @aliases getNMessages 
##' 
##' @author Davide Meneghetti, Livio Finos


getMessages <- function(percorso,dataI=NULL,dataF=NULL){
  percorso =fixPath(percorso)
  #lettura intero file
  perM=paste(percorso,"/html/messages.htm", sep="")
  pg=htmlParse(perM)
  #lettura nodi file
  #   getNodeSet(pg,"//p/text()")
  #   getNodeSet(pg,"//div[@class='message']")
  #il testo è coontenuto in <p>, esternamente a div message
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  meta=.estraielementi(meta)
  meta=inDataIT(meta)
  id.select=.which.within.date.null(messaggi$time,dataI, dataF)
  if(!is.null(id.select)) meta=meta[id.select]
  
  user=getNodeSet(pg,"//div/span[@class='user']/text()")
  if(!is.null(id.select)) user=user[id.select]
  
  msgs=getNodeSet(pg,"///div/p")
  if(!is.null(id.select)) msgs=msgs[id.select]
  
  nmsgs=length(msgs)
  nuser=length(user)
  nmeta=length(meta)
  if(!((nmsgs==nuser)&&(nmeta==nuser)))
    warning("Something went wrong in the reading. number of messages, users and times is different!")

  meta=data.frame(time=meta)
  meta$user=.estraielementi(user)
  meta$user=gsub(" $","",meta$user)
  
  meta$text=.estraielementi(msgs)
  meta$text=gsub("^<p>","",meta$text)
  meta$text=gsub("</p> $","",meta$text)
  return(meta)
}


getNMessages <- function(percorso,dataI=NULL,dataF=NULL){
  percorso=.fixPercorso(percorso)
  perM=paste(percorso,"/html/messages.htm", sep="")
  #lettura intero file
  pg=htmlParse(perM)
  #lettura nodi file
  meta=getNodeSet(pg,"//div/span[@class='meta']/text()")
  
  
  meta=.estraielementi(meta)
  
  nmess=length(.which.within.date(inDataIT(meta),dataI, dataF))
  return(nmess)
}
