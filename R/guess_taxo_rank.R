#' Guess the taxonomic rank of Scientific Name
#'
#' Guesses the taxonomic rank i.e. Genus, Species or Subspecies based on
#' number of words
#'
#' @param name scientific name string to be checked
#' @family Name functions
#' @examples
#' guess_taxo_rank("Akodon longipilis")
#' guess_taxo_rank("Akodon")
#' guess_taxo_rank("Abrocoma cinerea shistacea")
#'
#' @export
guess_taxo_rank <- function(name){
  level <- ""
  if(!(is.na(name) | is.null(name) | name =="")){
    wordcount <- length(strsplit(name," ")[[1]])
    level <- switch(wordcount,
                    "Genus or above",
                    "Species",
                    "Subspecies")
  }
  if(is.null(level)){
    level <- "Unknown"
  }
  return(level)
}
