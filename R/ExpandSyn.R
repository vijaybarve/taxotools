#' Expands Scientific name is the genus is used with '.'
#' @param name scientific name
#' @param syn synonym with short form genus name to expand the Genus
#' @return Synonym with Genus expanded
#' @family Name functions
#' @export
ExpandSyn <- function(name,syn){
  short <- FALSE
  for(i in 1:length(syn)){
    if(substr(syn[i],2,2)=='.'){
      if(substr(syn[i],1,1)==substr(name,1,1)){
        syn[i] <- paste(strsplit(name," ")[[1]][1],substr(syn[i],4,nchar(syn[i])))
      }
      short <- TRUE
    }
  }
  return(syn)
}
