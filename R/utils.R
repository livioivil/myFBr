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