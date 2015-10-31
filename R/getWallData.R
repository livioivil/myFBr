##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero delle differenti attività che compaiono sul wall
##' tra cui amicizie, stati, like, piace, condivisioni, link e giocato dopo una certa data.
##' Funzione valida anche per profili scritti in lingua inglese o spagnola.
##'
##' @title conoscere il numero delle varie attività sul wall
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRif data di riferimento
##' @return dataset (1x7) contenente le informazioni delle attività sul Wall dopo la data di riferimento
##' 
##' @author Davide Meneghetti

#funzione per leggere tutte le attivita' del wall
getWallData <- function(percorso,dataRif){
  
  perW=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perW)  
  #lettura sezione file
  wall=getNodeSet(pg,"//div[@class='contents']/div/text()")
  #wall
  n=length(wall)
  data=getNodeSet(pg,"//div[@class='contents']/div/div[@class='meta']/text()")
  #data
  n=length(data)
  
  getAtti <- function(i){
    #attivita wall
    atti=.estraielemento(wall[[i]])
    #data dell'attivita
    data=.estraielemento(data[[i]])
    c(data=data,atti=atti)
  }
  
  n=length(wall)
  #estrazione dati da lista
  rr=t(sapply(1:n, getAtti))
  
  #creazione dataset
  "datiWall" <- structure(.Data = list(rr[,"atti"],rr[,"data"]),
                          names = c("attivita", "data"),
                          row.names = c(1:n),
                          class = "data.frame")
  
  cercaNStringhe <- function(stringa){
    stati=rep(0,n)
    for(i in 1:n){
      if( inDataIT(datiWall[i,"data"])>=dataRif){
        k=grep(stringa, datiWall[i,"attivita"])
        if(length(k) != 0L) stati[i]=k
      }
    }
    length(stati[stati>0])
  }
  
  #creazione dataset
  "nWall" <- structure(.Data = list(cercaNStringhe("amicizia"),
                                    cercaNStringhe("stato"),
                                    cercaNStringhe("piace"),
                                    cercaNStringhe("like"),
                                    cercaNStringhe("condiviso"),
                                    cercaNStringhe("link"),
                                    cercaNStringhe("giocato")),
                       names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                       row.names = c(1:1),
                       class = "data.frame")
  
  if(nWall$amicizia==0){  #INGLESE
    #creazione dataset
    "nWall" <- structure(.Data = list(cercaNStringhe("friends"),
                                      cercaNStringhe("stato"),
                                      cercaNStringhe("likes"),
                                      cercaNStringhe("like"),
                                      cercaNStringhe("shared"),
                                      cercaNStringhe("link"),
                                      cercaNStringhe("played")),
                         names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                         row.names = c(1:1),
                         class = "data.frame")
  }    
  
  if(nWall$amicizia==0){ #SPAGNOLO
    #creazione dataset
    "nWall" <- structure(.Data = list(cercaNStringhe("amigos"),
                                      cercaNStringhe("estado"),
                                      cercaNStringhe("piace"),
                                      cercaNStringhe("like"),
                                      cercaNStringhe("compartido"),
                                      cercaNStringhe("link"),
                                      cercaNStringhe("jugado")),
                         names = c("amicizia", "stato","piace","like","condiviso","link","giocato"),
                         row.names = c(1:1),
                         class = "data.frame")
  }
  
  return(nWall)  
}
