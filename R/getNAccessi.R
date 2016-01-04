##' DA RIVEDERE
##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero sessioni aperte e di accessi compresi fra 2 date
##'
##' @title conoscere il numero sessioni aperte e di accessi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return dataset (1x2) contenente:numero di sessioni aperte e di accessi compresi fra 2 date
##' @export
##' @title getNAccessi
##' 
##' @author Davide Meneghetti

#funzione per conoscere il numero di accessi di un profilo dato il percorso di una cartella
getNAccessi <- function(percorso,dataI,dataF){
  perA=paste(percorso,"/html/security.htm", sep="")
  #lettura intero file
  pg=htmlParse(perA)
#   getNodeSet(pg,"//h2")
  sess=getNodeSet(pg,"//div[@class='contents']/div/ul[1]/li/p/text()");#numero di sessioni attive
  acce=getNodeSet(pg,"//div[@class='contents']/div/ul[2]/li/p/text()");#numero di accessi all'account
  
  n=length(sess)  
  temp=inDataIT(gsub("Create: ","", as.character(.estraielemento(sess[[1]]))))  
  sessioni=temp
  for(i in 2:n){
    temp=inDataIT(gsub("Create: ","", as.character(.estraielemento(sess[[i]]))))
    if(!is.na(temp)){
      if(temp >= dataI && temp <= dataF){sessioni=cbind(sessioni,as.Date(temp))}
    }
  }
  sessioni=length(sessioni)
  
  n=length(acce)
  temp=inDataIT(as.character(.estraielemento(acce[[1]])))  
  accessi=temp
  for(i in 2:n){
    temp=inDataIT(as.character(.estraielemento(acce[[i]])))
    if(!is.na(temp)){
      if(temp >= dataI && temp <= dataF){accessi=cbind(accessi,as.Date(temp))}
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