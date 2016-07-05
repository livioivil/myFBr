#' @examples 
#' function_to_run <- function(cart) getMessages_summary(cart,dataI=dataI,dataF=dataF)
#' @name perform_over_all_folders
#' @title perform_over_all_folders
#' @author livio finos
#' @aliases perform_over_all_folders
#' @export
#' 

perform_over_all_folders <- function(dir_list,function_to_run= function(cart) getMessages_summary(cart,dataI=dataI,dataF=dataF,stringhe = stringhe),
                                     dir_repos="./temp_anna/",overwrite=FALSE){
  
single_analysis <- function(cart){
    cat("\n folder: ",cart,".. ")
    nome_file=strsplit(cart,"/")
    nome_file=nome_file[[1]][length(nome_file[[1]])]
    nome_file=gsub("zip$","Rdata",nome_file)
    if((overwrite)||!(nome_file%in%dir(dir_repos))){
      res=try(function_to_run(cart),silent = TRUE)
      if(is(res,"try-error")){ 
        cat("\n errore per ", nome_file,"!!!")
        return(res)
      } else{
        cat("done.")
        save(file = paste(dir_repos,sep="",nome_file), res)
        return(TRUE)
      }
    } else {cat(" già presente")
      return(NA)}
  }
  
  
  
  res=mclapply( dir_list, single_analysis)
  cat("\n Total number of folders:",length(res))
  cat("\n Number of with non error status:",sum(sapply(res,function(i)i),na.rm=TRUE))
  cat("\n Number of folder not processed (because already present):",sum(sapply(res,is.na)))
  cat("\n Number of encontered erros:",sum(sapply(res,function(i) is(i,"try-error"))))
  TRUE
}