##' @description Validity check (and fix) for files path
##' @title fixPath
##' @param file.path path
##' @param silent FALSE
##' @param ifEmptyTryInWD If the file.path is empty, have a try using the working directory as root.
##' @return correct file.path 
##' @export
##' @author Livio Finos

fixPath <- function(file.path,silent=FALSE,ifEmptyTryInWD=TRUE){
  dirtemp=dir(file.path)
  
  if(length(dirtemp)==0){
    if(ifEmptyTryInWD) dirtemp=dir(paste("./",file.path,sep=""),full.names = TRUE)
  }
  
  if(length(dirtemp)==0){
    warning("directory ",file.path, " is empty or does not exist! fixPath will return NA")
    return(NA)
  }
  changed=FALSE
  if("__MACOSX"%in%dirtemp){    
    file.path=paste(file.path,sep="/",setdiff(dirtemp,"__MACOSX")[1])
    dirtemp=dir(file.path)
    changed=TRUE
  }
  continue=TRUE
  while((!("index.htm"%in%dirtemp)) && continue){
    changed=TRUE
    if(!is.null(dirtemp[1])){
      file.path=paste(file.path,sep="/",dirtemp[1])
      dirtemp=dir(file.path)
    } else {
      continue=FALSE
      if(!silent) warning("The path does not contain the correct files, fixPath will return NA")
      file.path=NA
      changed=FALSE
    }
  }
  if(changed & (!silent) ) 
    warning("The path has been modified to: ",file.path)
  
  file.path
}
