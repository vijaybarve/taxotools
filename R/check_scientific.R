#' Parse and resolve a scientific name string
#'
#' Parse the name using GNR and GBIF parse API to make sure the name
#' is scientific name
#'
#' @param name scientific name string to be checked
#' @return Resolved canonical name (NULL if not matched)
#'
#' @family Name functions
#' @importFrom taxize gbif_parse gnr_resolve
#' @examples
#' \dontrun{
#' check_scientific("Akodon longipilis (Waterhouse, 1837)")
#' check_scientific("Mus longipilis Waterhouse, 1837")
#' check_scientific("Akodon hershkovitzi Patterson, Gallardo, and Freas, 1984")
#' }
#' @export
check_scientific <- function(name){
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
