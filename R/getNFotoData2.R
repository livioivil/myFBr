##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di foto comprese fra 2 date
##'
##' @title conoscere il numero di foto
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di foto pubblicate dal profilo dopo la data di riferimento comprese fra 2 date
##' 
##' @author Davide Meneghetti

getNFotoData2 <- function(percorso,dataI, dataF){
  #numero di foto
  perF=paste(percorso,"/html/photos.htm", sep="")
  
  #lettura intero file
  pg=htmlParse(perF)
  dataL=getNodeSet(pg,"//div[@class='contents']/div[@class='block']/div/div[@class='meta']/text()") #data foto
  foto=0
  for(i in 1:length(dataL)){
    dataD=as.character(.estraielemento(dataL[[i]]))
    if(inDataIT(dataD) >= dataI && inDataIT(dataD) <= dataF){
      foto=i;
    }
  }
  #foto=length(getNodeSet(pg,"//div[@class='contents']/div[@class='block']")) #Numero foto
  return(foto)
}
