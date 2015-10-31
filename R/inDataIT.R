##' Funzione traduce una data estesa letta come stringa da facebook in italiano e la restituisce in formato Date
##'
##' @title tradurre data da stringa per esteso a formato data
##' @param data stringa data estesa letta da facebook
##' @return data in formato Date
##' 
##' @author Davide Meneghetti

inDataIT <- function(data){
  #Date italiane
  a=gsub(" alle ore","", data )
  a=gsub("^(\\w+) ","", a )
  a=gsub("gennaio","01", a )
  a=gsub("febbraio","02", a )
  a=gsub("marzo","03", a )
  a=gsub("aprile","04", a )
  a=gsub("maggio","05", a )
  a=gsub("giugno","06", a )
  a=gsub("luglio","07", a )
  a=gsub("agosto","08", a )
  a=gsub("settembre","09", a )
  a=gsub("ottobre","10", a )
  a=gsub("novembre","11", a )
  a=gsub("dicembre","12", a )
  
  a=gsub(" *UTC+02","", a )
  a=gsub(" *UTC+01","", a )
  
  lung=nchar(a)
  tz=substr(a,start=lung-5,stop=lung)
  #data=  as.Date(a,format ="%d %m %Y %H.%M",tz=tz)
  as.Date(a,format ="%d %m %Y %H.%M")
  
  return(as.Date(a,format ="%d %m %Y %H.%M"))
}