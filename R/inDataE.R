inDataE <- function(data){
  
  #Date spagnole
  a=gsub(" a la\\(s\\)","", data )
  a=gsub(",","", a )
  a=gsub(":",".", a )
  a=gsub(" de","", a )
  a=gsub("^(\\w+) ","", a )
  a=gsub("enero","01", a )
  a=gsub("febrero","02", a )
  a=gsub("marzo","03", a )
  a=gsub("abril","04", a )
  a=gsub("mayo","05", a )
  a=gsub("junio","06", a )
  a=gsub("julio","07", a )
  a=gsub("agosto","08", a )
  a=gsub("septiembre","09", a )
  a=gsub("octubre","10", a )
  a=gsub("noviembre","11", a )
  a=gsub("diciembre","12", a )
  
  
  a=gsub(" *UTC+02","", a )
  a=gsub(" *UTC+01","", a )
  
  lung=nchar(a)
  tz=substr(a,start=lung-5,stop=lung)
  #data=  as.Date(a,format ="%d %m %Y %H.%M",tz=tz)
  as.Date(a,format ="%d %m %Y %H.%M")
  
  return(as.Date(a,format ="%d %m %Y %H.%M"))
}