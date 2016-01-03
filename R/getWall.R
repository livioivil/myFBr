##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero delle differenti attivit? che compaiono sul wall
##' tra cui amicizie, stati, like, piace, condivisioni, link e giocato dopo una certa data.
##' Funzione valida anche per profili scritti in lingua inglese o spagnola.
##'
##' @title conoscere il numero delle varie attivit? sul wall
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di Inizio
##' @param dataF data di Fine
##' @return dataset (1x7) contenente le informazioni delle attivit? sul Wall dopo la data di riferimento
##' @export
##' @title getWall
##' 
##' @author Davide Meneghetti

#funzione per leggere tutte le attivita' del wall
getWall <- function(percorso, dataI, dataF){
  
  perW=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perW)  
  #lettura sezione file
  wall=getNodeSet(pg,"//div[@class='contents']/div/text()")
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

ans.type=c("amicizia","stato","piace","like","condiviso","link","giocato")

res=sapply(ans.type, function(txt) length(grep(txt,wall)))
#creazione dataset
"nWall" <- structure(.Data = as.list(res),
                       names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                       row.names = c(1:1),
                       class = "data.frame")
  
  if(nWall$amicizia==0){  #INGLESE
    ans.type=c("friends","stato","likes","like","shared","link","played")
    
    res=sapply(ans.type, function(txt) length(grep(txt,wall)))
    #creazione dataset
    "nWall" <- structure(.Data = as.list(res),
                         names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                         row.names = c(1:1),
                         class = "data.frame")
  }
  
  if(nWall$amicizia==0){ #SPAGNOLO
    ans.type=c("amigos","estado","piace","like","compartido","link","jugado")
    
    res=sapply(ans.type, function(txt) length(grep(txt,wall)))
    #creazione dataset
    "nWall" <- structure(.Data = as.list(res),
                         names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                         row.names = c(1:1),
                         class = "data.frame")

  }
  
  return(nWall)  
}
