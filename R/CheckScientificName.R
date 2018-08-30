#' Parse and resolve a scientific name string
#'
#' Parse the names using GBIF parse API to make sure the name passed
#' is an scientific name
#'
#' @param name scientific name string to be checked
#' @return Resolved Canonical name and NULL is not matched
#'
#' @family Name functions
#' @importFrom taxize gbif_parse gnr_resolve
#' @examples
#' CheckScientificName("Akodon longipilis (Waterhouse, 1837)")
#' CheckScientificName("Mus longipilis Waterhouse, 1837")
#' CheckScientificName("Akodon hershkovitzi Patterson, Gallardo, and Freas, 1984")
#'
#' @export
CheckScientificName <- function(name){
  res <- gnr_resolve(name)
  if(dim(res)[1]>0){
    res1 <- tryCatch({gbif_parse(res$matched_name[1])},
                     error=function(cond) {
                       message("There was an error")
                       return(NULL)})
    return(res1$canonicalname)
  } else {
    return(NULL)
  }
}
