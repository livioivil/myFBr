##' @description Validity check (and fix) for files path
##'
##' @title fixPath
##' @param file.path
##' @return correct file.path
##' @export
##' 
##' @author Livio Finos

fixPath <- function(percorso,silent=FALSE){
  dirtemp=dir(percorso)
  changed=FALSE
  if("__MACOSX"%in%dirtemp){    
    percorso=paste(percorso,sep="/",setdiff(dirtemp,"__MACOSX")[1])
    dirtemp=dir(percorso)
    changed=TRUE
  }
  continue=TRUE
  while((!("index.htm"%in%dirtemp)) && continue){
    changed=TRUE
    if(!is.null(dirtemp[1])){
      percorso=paste(percorso,sep="/",dirtemp[1])
      dirtemp=dir(percorso)
    } else {
      continue=FALSE
      if(!silent) warning("The path does not contain the correct files")
      percorso=NA
      changed=FALSE
    }
  }
  if(changed & (!silent) ) 
    warning("The path has been modified to:")
  
  percorso
}
