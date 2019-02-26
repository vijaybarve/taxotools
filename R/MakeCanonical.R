#' Construct canonical names
#'
#' Canonical names using Genus, Species and Subspecies friends
#'
#' @param dat data frame containing taxonomic list
#' @param genus field name for Genus field
#' @param species field name for Species field
#' @param subspecies field name for Subspecies field
#' @family name functions
#' @return a data frame containing Canonical names field added or repopulated using
#'     filed names for Genus, Species and Subspecies specified in parameters
#' @examples
#' \dontrun{
#' MakeCanonical(mylist,"genus","species","subspecies")
#' }
#' @export
MakeCanonical <- function(dat,genus="",species="",subspecies=""){
  newdat <- dat
  newdat$canonical <- ""
  if(genus==""){
    return(NULL)
  } else {
    newdat <- RenameColumn(newdat,genus,"genus")
  }
  if(species==""){
    return(NULL)
  } else {
    newdat <- RenameColumn(newdat,species,"species")
  }
  if(subspecies!=""){
    newdat <- RenameColumn(newdat,subspecies,"subspecies")
  }
  for(i in 1:dim(newdat)[1]){
    cano <- newdat$genus[i]
    if(!is.empty(newdat$species[i])){
      cano <- paste(cano,newdat$species[i])
    }
    if(subspecies!=""){
      if(!is.empty(newdat$subspecies[i])){
        cano <- paste(cano,newdat$subspecies[i])
      }
    }
    newdat$canonical[i] <- cano
  }
  newdat <- RenameColumn(newdat,"genus",genus)
  newdat <- RenameColumn(newdat,"species",species)
  newdat <- RenameColumn(newdat,"subspecies",subspecies)
  return(newdat)
}

is.empty <- function(val){
  if(is.null(val)){
    return(TRUE)
  }
  if(is.na(val)){
    return(TRUE)
  }

  if(val==""){
    return(TRUE)
  }
  return(FALSE)
}

RenameColumn <- function(dat,old,new){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    print("Error: Fieldname not found...")
  }
  return(dat)
}
