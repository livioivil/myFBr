# funzione per il numero di posti in cui è stata 
getStati<-function(percorso){
  #numero stati
  perA=paste(percorso,"/html/wall.htm", sep="")
  #lettura intero file
  pg=htmlParse(perA)
  
  comm=length(getNodeSet(pg,"//div[@class='contents']/div/div[@class='comment']"))
  #creazione dataset
  "stati" <- structure(.Data = list(comm),
                       names = c("N.Stati"),
                       row.names = c(1:1),
                       class = "data.frame")
  
  
  return(stati)
  
}

