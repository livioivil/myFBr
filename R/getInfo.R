##' @description Funzione che dato il percorso dei dati del profilo facebook ritorna alcune infomrazioni personali (dalla pagina settings.html)
##' 
##' @title indicazione del sesso del profilo
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return F,M
##' @export
##' @title getInfo
##' 
##' @author Davide Meneghetti, Livio Finos

getInfo <- function(percorso){
  percorso=.fixPercorso(percorso)
  perE=paste(percorso,"/index.htm", sep="")
  pg=htmlParse(perE)#lettura intero file
  email=getNodeSet(pg,"//tr[th[text()='Email']]/td/text()");
  email = email[[1]]
  
  sesso=getNodeSet(pg,"//tr[th[text()='Sesso']]/td/text()");
  sesso = .cleanSex(sesso)
  if(is.na(sesso)){
    sesso=getNodeSet(pg,"//tr[th[text()='Genere']]/td/text()");
    sesso = .cleanSex(sesso)
  }
    
  dataReg=.getValore(pg,"//tr[th/text()='Data di registrazione']/td/text()")
  dataReg=inDataIT(dataReg)
  
  dataDown=.getDataDownload(pg)
  
  data.frame(dataReg=dataReg,
             dataDownload=dataDown,
             sesso=sesso,
             dataNascita=as.POSIXct(.getValore(pg,"//tr[th/text()='Data di nascita']/td/text()"),format ="%m/%d/%Y",tz="UTC"),
             cittaNatale=.getValore(pg,"//tr[th/text()='Città natale']/td/text()"),
             situazSentim=.getValore(pg,"//tr[th/text()='Situazione sentimentale']/td/text()")
  )
}#getSesso