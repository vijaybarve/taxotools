#' @title taxo_fuzzy_match
#' @description Fuzzy matching with names
#' @param name Name to search
#' @param master List of names
#' @param dist Distance tolerance, Default: 2
#' @return Matched name. Null if not found.
#' @details Fuzzy matching with names in the master list and return best match.
#' @importFrom stringdist stringdist
#' @examples
#' \dontrun{
#'master <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Acodon hirtus",
#'                                     "Akodon longipilis apta",
#'                                     "Akodon longipilis castaneus",
#'                                     "Chroeomys jelskii",
#'                                     "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = F)
#'  taxo_fuzzy_match("Acodon hirta",master)
#' }
#' @rdname taxo_fuzzy_match
#' @export
taxo_fuzzy_match <- function(name,master,dist=2){
  ret <- master[agrep(name,master$canonical),c("canonical")]
  if(identical(ret, character(0)) ){
      ret <- NULL
  } else {
    ret <- data.frame("canonical"=ret,
                      stringsAsFactors = F)
    ret$dist <- stringdist(name,ret$canonical)
    if(min(ret$dist)>dist){
      ret <- NULL
    }else {
      ans <- NULL
      ans$canonical <- ret$canonical[which(ret$dist==min(ret$dist))]
      ans$dist <- min(ret$dist)
      ret <- ans
    }
  }
  return(ret)
}
