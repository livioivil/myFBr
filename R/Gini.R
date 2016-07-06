#' @names Gini
#' @aliases Gini
#' @description It computes Gini Index
#' @title Gini 
#' @param user 
#' @param n_mess 
#' @param k ? il numero di modalit?, in questo caso il numero di amici totali che scrivono
#' @param 
#' @export


Gini=function(user,n_mess=NULL,k=NULL,adjusted=TRUE){
  tab=table(user)
  if(is.null(n_mess))
    n_mess=length(user)
  if(is.null(k))
    k=length(tab)
  
  freq_relative=tab/n_mess
  gini=1-sum((freq_relative)^2)
  if(adjusted){
    Imax=(k-1)/k
    gini=gini/Imax
  }
  if(gini=="NaN")
    gini=0
  return(gini)
}