
kmeans4 <- function(dati,c,it){
  print("Hartigan-Wong")
  k<- kmeans(dati, centers=c, iter.max=it, algorithm="Hartigan-Wong");
  print(table(k$cluster))
  print("Lloyd")
  k<- kmeans(dati, centers=c, iter.max=it, algorithm="Lloyd");
  print(table(k$cluster))
  print("Forgy")
  k<- kmeans(dati, centers=c, iter.max=it, algorithm="Forgy");
  print(table(k$cluster))
  print("MacQueen")
  k<- kmeans(dati, centers=c, iter.max=it, algorithm= "MacQueen");
  print(table(k$cluster))
}

kmeansGrafico <- function(dati,c,it,alg){
  k<- kmeans(dati, centers=c, iter.max=it, algorithm=alg)
  grup=k$cluster
  ck=cbind(completoData,grup)
  plot(ck$nPost,ck$amicizia, type="n", xlab="nPost", ylab="Amicizia")
  for(i in 1:n){
    points(ck[ck$grup==i,]$nPost,ck[ck$grup==i,]$amicizia,col=i)
  }
}