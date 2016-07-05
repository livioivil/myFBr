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

# getMessages2 <- function(percorso=".",dataI=NULL, dataF=NULL){
#     percorso=myFBr:::.fixPercorso(percorso)
#     
#     percorsoMess=paste(percorso,"/html/messages.htm", sep="")
#     #lettura intero file
#     dumFun <- function(x){
#       # xname <- xmlName(x)
#       # xattrs <- xmlAttrs(x)
#       sapply(xmlChildren(x), xmlValue)
#       
#       c(sapply(xmlChildren(x), function(xx){browser();  getNodeSet(xx,"div[@class='message_header']/span/text()") else xmlValue}))
#     }
#     dum <- XML::xmlParse(percorsoMess)
#     out=xpathSApply(dum, "//div[@class='thread']", dumFun)
#     
#     header=sapply(getNodeSet(dum,"//div[@class='message_header']/span/text()"),xmlValue)
#     length(header)/2
#     
#     text=sapply(getNodeSet(dum,"//p/text()"),xmlValue)
#     str(text)
#     
#     res=sapply(out,
#                function(x) c(time=x[[1]],action=x[[2]],text=x[[3]]))
#     res=data.frame(t(res),stringsAsFactors = FALSE)
#     
#     res$time=inDataIT(res$time)
#     keep=.which.within.date(res$time,dataI, dataF)
#     res=res[keep,]
#     res$text[res$text=="p"]=NA
#     return(res)  
#   }
  
 
getMessages <- function (percorso, dataI = NULL, dataF = NULL)
{
  percorso = fixPath(percorso)
  perM = paste(percorso, "/html/messages.htm", sep = "")
  if (!("messages.htm" %in% dir(paste(percorso, "/html", sep = ""))))
    return(data.frame(time = NA, user = NA, text = NA)[-1,])
  
  pg = htmlParse(perM)
  
  threads=getNodeSet(pg, "//div/div[@class='thread']")
  threads_people=getNodeSet(pg, "//div/div[@class='thread']/text()")
  msg= plyr::llply(threads, function(el) {getNodeSet(el, "div/div/span")})
  msg_lengths=sapply(msg,length)/2
  
  msg=plyr::llply(msg,function(one_msg) plyr::llply(one_msg, function(el) {
    getNodeSet(el,"text()")}))
  msg=matrix((unlist(msg)),ncol=2,byrow = TRUE)
  # head(msg)
  
  text=plyr::llply(threads, function(el) getNodeSet(el, "p"))
  
  tempo=myFBr:::inDataIT(myFBr:::.estraielementi(msg[,2]))
  id.select = myFBr:::.which.within.date.null(tempo, dataI, dataF)
  
  
  if (!is.null(id.select)){
    tempo = tempo[id.select]
    user = myFBr:::.estraielementi(msg[id.select,1])
    rm(msg)
    text=unlist(text)[id.select]
    # text=myFBr:::.estraielementi(text)
    thread=rep(threads_people,msg_lengths)[id.select]
    # thread=myFBr:::.estraielementi(thread)
  } else{
    user = myFBr:::.estraielementi(msg[,1])
    rm(msg)
    text=unlist(text)
    # text=myFBr:::.estraielementi(text)
    thread=rep(threads_people,msg_lengths)
  }
  ntext = length(text)
  nuser = length(user)
  ntempo = length(tempo)
  nthread = length(thread)
  if (!((ntext == nuser) &&(ntext == nthread) && (ntempo == nuser)))
    warning("Something went wrong in the reading. number of messages,
            users and times is different!")
  
  user = unlist(user)
  meta = data.frame(user = gsub(" $", "", user),time=tempo)
  meta$text = myFBr:::.estraielementi(text)
  meta$text = gsub("^<p>", "", meta$text)
  meta$text = gsub("</p> $", "", meta$text)
  meta$text[meta$text=="<p/> "]=""
  meta$thread = factor(unlist(myFBr:::.estraielementi(thread)))
  return(meta)
}



getNMessages <- function(percorso,dataI=NULL,dataF=NULL){
  nrow(getMessages(percorso,dataI=NULL,dataF=NULL))
}
