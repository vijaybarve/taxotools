#' @title Convert all subspecies into synonyms of the species
#' @description used in generating master lists
#' @param master List of names with a field named canonical
#' @param verbose display process messages, Default: FALSE
#' @return Same list of names with id and accid fields added (or data updated
#' the fields exists) with all subspecies linked to the species names as
#' synonyms
#' @details While dealing with taxonomic names only at specific level,
#' to take advantage of sub-specific names already available in the lists
#' are sometimes treated as synonyms of the names at species rank. To
#' convert all the subspecies names as synonyms this function is very handy.
#' @family List functions
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
  for(i in 1:nrow(master)){
    if(guess_taxo_rank(master$canonical[i])=="Subspecies"
       & master$accid[i]==0){
      spname <- paste(unlist(strsplit(master$canonical[i],
                                      split = "\\s+"))[1:2],collapse=" ")
      if(verbose){print(paste(i,master$canonical[i]))}
      if(spname %in% master$canonical){
        master$accid[i] <- get_accid(master,spname)
        if(nrow(master[which(master$accid==master$id[i]),])>0){
          master$accid[which(master$accid==master$id[i])] <- master$accid[i]
        }
      }
    }
  }
  remrec <- master[which(master$taxonlevel=="SUBSPECIES" & master$accid==0),]
  if(verbose){print(paste("Orphan subspecies",nrow(remrec)))}
  return(master)
}
