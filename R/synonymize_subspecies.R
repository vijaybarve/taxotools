#' @title Convert all subspecies into synonyms of the species
#' @description used in generating master lists
#' @param master List of names with a field named canonical
#' @param verbose display process messages, Default: FALSE
#' @return Same list of names with id and accid fields added (or data updated
#' the fields exists) with all subspecies linked to the species names as
#' synonyms
#' @details used in generating master lists
#' @examples
#' \dontrun{
#' newmaster <- synonymize_subspecies(master)
#' }
#' @rdname synonymize_subspecies
#' @export
synonymize_subspecies <- function(master,
                                  verbose=FALSE){
  names(master) <- tolower(names(master))
  if("id" %!in% names(master)){
    master$id <- seq.int(nrow(master))
  }
  if("accid" %!in% names(master)){
    master$accid <- 0
  }
  for(i in 1:dim(master)[1]){
    if(guess_taxo_rank(master$canonical[i])=="Subspecies"
       & master$accid[i]==0){
      spname <- paste(unlist(strsplit(master$canonical[i],
                                      split = "\\s+"))[1:2],collapse=" ")
      if(verbose){print(paste(i,master$canonical[i]))}
      master$accid[i] <- master$id[which(master$canonical==spname)]
    }
  }
  return(master)
}
