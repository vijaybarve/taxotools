#' Expands Scientific name is the genus is used with '.'
#'
#' This function is depreciated. Please use \link{expand_name}
#'
#' @param name scientific name
#' @param syn synonym with short form genus name to expand the Genus
#' @return Synonym with Genus expanded using either name or previous names in
#' the syn list
#' @family Name functions
#' @examples
#' \dontrun{
#' ExpandSyn("Addax gibbosa", "A. mytilopes")
#' ExpandSyn("Oryx addax", "O. nasomaculatus")
#' }
#' @export
ExpandSyn <- function(name,syn){
  .Deprecated("expand_name")
  for(i in 1:length(syn)){
    if(substr(syn[i],2,2)=='.'){
      if(substr(syn[i],1,1)==substr(name,1,1)){
        syn[i] <- paste(strsplit(name," ")[[1]][1],substr(syn[i],4,nchar(syn[i])))
      } else {
        if(i > 1) {
          if( substr(syn[i],1,1)==substr(syn[i-1],1,1)){
            syn[i] <- paste(strsplit(syn[i-1]," ")[[1]][1],substr(syn[i],4,nchar(syn[i])))
          }
        }
      }
    }
  }
  return(syn)
}
