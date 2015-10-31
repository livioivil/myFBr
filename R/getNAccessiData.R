##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero sessioni aperte e di accessi dopo una certa data
##'
##' @title conoscere il numero sessioni aperte e di accessi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataRef data di riferimento
##' @return dataset (1x2) contenente:numero di sessioni aperte e di accessi dopo la data di riferimento
##' 
##' @author Davide Meneghetti

#funzione per conoscere il numero di accessi di un profilo dato il percorso di una cartella
getNAccessiData <- function(percorso,dataRef){
  perA=paste(percorso,"/html/security.htm", sep="")
  #lettura intero file
  pg=htmlParse(perA)
  sess=getNodeSet(pg,"//div[@class='contents']/div/ul[1]/li/p/text()");#numero di sessioni attive
  acce=getNodeSet(pg,"//div[@class='contents']/div/ul[2]/li/p/text()");#numero di accessi all'account
  
  n=length(sess)  
  temp=inDataIT(gsub("Create: ","", as.character(.estraielemento(sess[[1]]))))  
  sessioni=temp
  for(i in 2:n){
    temp=inDataIT(gsub("Create: ","", as.character(.estraielemento(sess[[i]]))))
    if(!is.na(temp)){
      if(temp > dataRef){sessioni=cbind(sessioni,as.Date(temp))}
    }
  }
  sessioni=length(sessioni)
  
  n=length(acce)
  temp=inDataIT(as.character(.estraielemento(acce[[1]])))  
  accessi=temp
  for(i in 2:n){
    temp=inDataIT(as.character(.estraielemento(acce[[i]])))
    if(!is.na(temp)){
      if(temp > dataRef){accessi=cbind(accessi,as.Date(temp))}
    }
  }  
  accessi=length(accessi)
  
  #creazione dataset
  "acces" <- structure(.Data = list(sessioni,accessi),
                       names = c("nSessioni", "nAccessi"),
                       row.names = c(1:1),
                       class = "data.frame")
  return(acces)
}