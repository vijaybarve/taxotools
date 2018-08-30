#' Guess the level of Scientific Name
#' @param name scientific name string to be checked
#' @family Name functions
#' @return Guess on level of Taxon name and NULL if not sure
#' @examples
#' GuessTaxoLevel("Akodon longipilis")
#' GuessTaxoLevel("Akodon")
#' GuessTaxoLevel("Abrocoma cinerea shistacea")
#'
#' @export
GuessTaxoLevel <- function(name){
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
