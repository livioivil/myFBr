##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero delle differenti attivit? che compaiono sul wall
##' tra cui amicizie, stati, like, piace, condivisioni, link e giocato dopo una certa data.
##' Funzione NON valida per profili scritti in lingua inglese o spagnola, facile da aggiornare
##'
##' @title conoscere il numero delle varie attivit? sul wall
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di Inizio (-Inf by default)
##' @param dataF data di Fine (+Inf by default)
##' @return getWall un dataset a con tre colonne: time, user, text
##' getWall_summary: dataset (1x7) contenente le informazioni delle attivit? sul Wall dopo la data di riferimento
##' @export getWall getWall_summary
##' @title getWall
##' @aliases getWall_summary
##' 
##' @author Davide Meneghetti, Livio Finos

#funzione per leggere tutte le attivita' del wall
getWall_summary <- function(percorso, dataI=NULL, dataF=NULL){
  percorso=.fixPercorso(percorso)
  
  cerca.testo=c(amicizia="hanno stretto amicizia",
                stato="ha aggiornato il suo stato",
                piace =  " piace ",
                condiviso= "ha condiviso la foto",
                linkAltri= "ha pubblicato un link sul tuo diario.",
                linkTuo="ha condiviso un link.",
                partecipato="ha partecipato a",
                postInBacheca="scritto sul tuo diario",
                aggiornImgProf="aggiornato la sua immagine del profilo",
                nuovaFoto="aggiunto una nuova foto all\'album")

  perW=paste(percorso,"/html/wall.htm", sep="")
  if(!("wall.htm"%in%dir(paste(percorso,"/html", sep="")))){
    return(.make.empty.Wall_symmary(cerca.testo))}
  #lettura intero file
  pg=htmlParse(perW)  
  #lettura sezione file
  wall=getNodeSet(pg,"//div[@class='contents']/div/text()")
  if(length(wall)==0) {
    return(.make.empty.Wall_symmary(cerca.testo))}
  wall=sapply(wall,.estraielemento)
  #wall
#   n=length(wall)
  data=getNodeSet(pg,"//div[@class='contents']/div/div[@class='meta']/text()")
  #data
  data=sapply(data,.estraielemento)
  data=inDataIT(data)
  keep=.which.within.date(data,dataI, dataF) 
#   data=data[keep]
  wall=wall[keep]
  n=length(keep)

#   sum(res)
  res=sapply(cerca.testo, function(txt) length(grep(txt,wall)))
  res=c(res,postTotali=n)
#creazione dataset
"nWall" <- structure(.Data = as.list(res),
                       names = names(res) ,
                       row.names = c(1:1),
                       class = "data.frame")
  
#   if(nWall$amicizia==0){  #INGLESE
#     ans.type2=c("friends","stato","likes","like","shared","link","played","wall")
#     
#     res=sapply(ans.type2, function(txt) length(grep(txt,wall)))
#     #creazione dataset
#     "nWall" <- structure(.Data = as.list(res),
#                          names = ans.type,
#                          row.names = c(1:1),
#                          class = "data.frame")
#   }
#   
#   if(nWall$amicizia==0){ #SPAGNOLO
#     ans.type2=c("amigos","estado","piace","like","compartido","link","jugado","diario")
#     
#     res=sapply(ans.type2, function(txt) length(grep(txt,wall)))
#     #creazione dataset
#     "nWall" <- structure(.Data = as.list(res),
#                          names = ans.type,
#                          row.names = c(1:1),
#                          class = "data.frame")
# 
#   }
#   
  return(nWall)  
}

######################
getWall <- function(percorso, dataI, dataF){
  percorso=.fixPercorso(percorso)
  
  perW=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perW)  
  #lettura sezione file
  wall=getNodeSet(pg,"//div[@class='contents']/div/text()")
  if(length(wall)==0) {
    "nWall" <- structure(.Data = as.list(rep(NA,length(cerca.testo)+1)),
                         names = c(names(cerca.testo),"postTotali"),
                         row.names = c(1:1),
                         class = "data.frame")
    return(nWall)}
  wall=sapply(wall,.estraielemento)
  #wall
  #   n=length(wall)
  data=getNodeSet(pg,"//div[@class='contents']/div/div[@class='meta']/text()")
  #data
  data=sapply(data,.estraielemento)
  data=inDataIT(data)
  keep=.which.within.date(data,dataI, dataF) 
  #   data=data[keep]
  wall=wall[keep]
  n=length(keep)
  
  #   sum(res)
  res=sapply(cerca.testo, function(txt) length(grep(txt,wall)))
  res=c(res,postTotali=n)
  #creazione dataset
  "nWall" <- structure(.Data = as.list(res),
                       names = names(res) ,
                       row.names = c(1:1),
                       class = "data.frame")
  return(nWall)  
}
