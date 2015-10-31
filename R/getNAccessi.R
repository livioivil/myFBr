##' Funzione che dato il percorso dei dati del profilo facebook ritorna il numero di sessioni aperte e il numero accessi
##'
##' @title conoscere il numero di sessioni aperte e il numero accessi
##' @param percorso stringa che indica il percorso della cartella dei dati del profilo
##' @return dataset(1x2): numero di sessioni e numero di accessi 
##' 
##' @author Davide Meneghetti

getNAccessi <- function(percorso){
  perA=paste(percorso,"/html/security.htm", sep="")
  pg=htmlParse(perA)
  
  sessioni=length(getNodeSet(pg,"//div[@class='contents']/div/ul[1]/li"));#numero di sessioni attive
  accessi=length(getNodeSet(pg,"//div[@class='contents']/div/ul[2]/li"));#numero di accessi all'account
  
  #creazione dataset
  "acces" <- structure(.Data = list(sessioni,accessi),
                       names = c("nSessioni", "nAccessi"),
                       row.names = c(1:1),
                       class = "data.frame")
  return(acces)
}