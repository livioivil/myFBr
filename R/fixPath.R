##' @description Validity check (and fix) for files path
##' @title fixPath
##' @param file.path path
##' @param silent FALSE
##' @return correct file.path 
##' @export
##' @author Livio Finos

fixPath <- function(file.path,silent=FALSE){
  dirtemp=dir(file.path)
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
      if(!silent) warning("The path does not contain the correct files")
      file.path=NA
      changed=FALSE
    }
  }
  if(changed & (!silent) ) 
    warning("The path has been modified to:")
  
  file.path
}
