##' @description Funzione che dato il percorso dei dati del profilo facebook 
##' ritorna il numero di foto (getNPhotos) o tutte le informazioni sulle singole foto (getPhotos)
##' comprese fra 2 date.(NON ANCORA IMPLEMENTATA)
##'
##' @title calcola il numero di foto
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @param dataI data di inizio di riferimento
##' @param dataF data di fine di riferimento 
##' @return numero di foto pubblicate dal profilo dopo la data di riferimento comprese fra 2 date
##' @export
##' @title getNPhotos
##' @author Davide Meneghetti, Livio Finos



getNPhotos <- function(percorso,dataI=NULL, dataF=NULL){
  percorso=fixPath(percorso)
  #numero di foto
  perF=paste(percorso,"/photos", sep="")
  if(!("photos"%in%dir(percorso)))
    return(NA)
  list.album = list.dirs(perF)[-1]
  #lettura intero file
#   album=list.album[2]
  ############################
  .getInfoPhoto <- function(album){
    index.path=paste(album,"/index.htm", sep="")
    if("index.htm"%in%dir(album)){
      pg=htmlParse(index.path)
      dataL=  getNodeSet(pg,"//div[@class='block']/div/div[@class='meta']/text()") #data foto
      dataL=.estraielementi(dataL)
      dataL=inDataIT(dataL)
      id.select=.which.within.date.null(dataL,dataI,dataF)
      if(is.null(id.select)) 
        return(length(dataL)) else 
          return(length(id.select))
    } else return(0)
  }
  res=(sapply(list.album,.getInfoPhoto))
   res=res[res>0]
  ############################  
  return(list(nAlbum=length(res),nPhoto=sum(res),nPhotoInAlbum=res))
}
