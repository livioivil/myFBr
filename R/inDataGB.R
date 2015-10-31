inDataGB <- function(data){
  
  #Date inglesi
  a=gsub(" at","", data )
  a=gsub(",","", a )
  a=gsub(":",".", a )
  a=gsub("^(\\w+) ","", a )
  a=gsub("January","01", a )
  a=gsub("February","02", a )
  a=gsub("March","03", a )
  a=gsub("April","04", a )
  a=gsub("May","05", a )
  a=gsub("June","06", a )
  a=gsub("July","07", a )
  a=gsub("August","08", a )
  a=gsub("September","09", a )
  a=gsub("October","10", a )
  a=gsub("November","11", a )
  a=gsub("December","12", a )
  
  
  a=gsub(" *UTC+02","", a )
  a=gsub(" *UTC+01","", a )
  
  lung=nchar(a)
  tz=substr(a,start=lung-5,stop=lung)
  #data=  as.Date(a,format ="%d %m %Y %H.%M",tz=tz)
  as.Date(a,format ="%d %m %Y %H.%M")
  
  return(as.Date(a,format ="%d %m %Y %H.%M"))
}