##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di foto dopo una certa data
##'
##' @title conoscere il numero di foto
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param data data di riferimento
##' @return numero di foto pubblicate dal profilo dopo la data di riferimento
##' 
##' @author Davide Meneghetti

getNFotoData <- function(percorso,data){
  #numero di foto
  perF=paste(percorso,"/html/photos.htm", sep="")
  
  #lettura intero file
  pg=htmlParse(perF)
  dataL=getNodeSet(pg,"//div[@class='contents']/div[@class='block']/div/div[@class='meta']/text()") #data foto
  foto=0
  for(i in 1:length(dataL)){
    dataD=as.character(.estraielemento(dataL[[i]]))
    if(inDataIT(dataD) >= data){
      foto=i;
    }
  }
  #foto=length(getNodeSet(pg,"//div[@class='contents']/div[@class='block']")) #Numero foto
  return(foto)
}
