#' Construct canonical names
#'
#' Construct canonical names using Genus, Species and Subspecies fields. At times
#' due to spaces or NAs in the data fields, it makes it tricky to generate
#' canonical names.
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
#' cast_canonical(mylist,"genus","species","subspecies")
#' }
#' @export
cast_canonical <- function(dat,genus="",species="",subspecies=""){
  newdat <- as.data.frame(dat)
  newdat$canonical <- ""
  if(genus==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,genus,"genus")
  }
  if(species==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,species,"species")
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies")
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
  newdat <- rename_column(newdat,"genus",genus)
  newdat <- rename_column(newdat,"species",species)
  newdat <- rename_column(newdat,"subspecies",subspecies)
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

rename_column <- function(dat,old,new){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    print(paste("Error: Fieldname not found...",old))
  }
  return(dat)
}
