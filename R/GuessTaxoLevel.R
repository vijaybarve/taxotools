#' Guess the level of Scientific Name
#'
#' #' This function is depreciated. Please use \link{match_lists}
#'
#' @param name scientific name string to be checked
#' @family Discontinued functions
#' @return Guess on level of Taxon name and NULL if not sure
#' @examples
#' \dontrun{
#' GuessTaxoLevel("Akodon longipilis")
#' GuessTaxoLevel("Akodon")
#' GuessTaxoLevel("Abrocoma cinerea shistacea")
#' }
#' @export
GuessTaxoLevel <- function(name){
  .Deprecated("guess_taxo_level")
  level <- ""
  if(!(is.na(name) | is.null(name) | name =="")){
    wordcount <- length(strsplit(name," ")[[1]])
    level <- switch(wordcount,
                    "Genus or above",
                    "Species",
                    "Subspecies")
  }
  return(level)
}
