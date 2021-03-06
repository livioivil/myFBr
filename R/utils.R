.make.empty.Wall_symmary <-function(cerca.testo){ 
  "nWall" <- structure(.Data = as.list(rep(NA,length(cerca.testo)+1)),
                       names = c(names(cerca.testo),"postTotali"),
                       row.names = c(1:1),
                       class = "data.frame")
  nWall}

#mantenuta solo per peigrizia, sostituire ovunque nelle funzioni, man mano 
.fixPercorso <- function(percorso){
  fixPath(percorso)
}

.getWallPath <- function(percorso){
  if("wall.htm"%in%dir(paste(percorso,"/html", sep=""))){
    perW=paste(percorso,"/html/wall.htm", sep="")
  } else {
    perW=paste(percorso,"/html/timeline.htm", sep="")
  }
  perW
}

.which.within.date.null <- function(a,dataI,dataF){
  if(is.null(dataI)&is.null(dataF)) return(NULL)
  if(is.null(dataI)&(!is.null(dataF))) return((a <= dataF))
  if(is.null(dataF)&(!is.null(dataI))) return((a >= dataI)) else
    which((a >= dataI) &  (a <= dataF))
}

.which.within.date <- function(a,dataI,dataF){
  if(is.null(dataI)&is.null(dataF)) return(rep(TRUE,length(a)))
  if(is.null(dataI)&(!is.null(dataF))) return((a <= dataF))
  if(is.null(dataF)&(!is.null(dataI))) return((a >= dataI)) else
  which((a >= dataI) &  (a <= dataF))
}


.estraielemento <- function(d){
  d=toString.XMLNode(d);
  d
}

.estraielementi <- function(d){
  plyr::llply(d,toString.XMLNode)  
}


#######################
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


some_statistics <- function(values){
  q=quantile(values,c(0,.25,.5,.75,1))
  c(mean=mean(values),mode= mode(values),sd=sd(values),q=q)
}
