.fixPercorso <- function(percorso){
  dirtemp=dir(percorso)
  if("__MACOSX"%in%dirtemp){    
    percorso=paste(percorso,sep="/",setdiff(dirtemp,"__MACOSX")[1])
  }
  percorso
}

.which.within.date <- function(a,dataI,dataF){
  which((a >= dataI) & (a <= dataF))
}

.estraielemento <- function(d){
  d=toString.XMLNode(d);
  d
}

.getValore <- function (file,ric){
  v=getNodeSet(file,ric)
  v=v[[1]]
  v=toString.XMLNode(v)
  v=gsub(" $","",v)
  v
}

.cleanSex <- function(sesso){
  if(is.null(sesso)) return(NA)
  sesso=.estraielemento(sesso[[1]])
  sesso=gsub(" ","",sesso)
  if(sesso=="Uomo")
  {sesso="M"}
  if(sesso=="Donna")
  {sesso="F"}
  sesso
}
