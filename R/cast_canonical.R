#' Construct canonical names
#'
#' Construct canonical names using Genus, Species and Subspecies fields. At times
#' due to spaces or NAs in the data fields, it makes it tricky to generate
#' canonical names.
#'
#' @param dat data frame containing taxonomic list
#' @param canonical field name for canonical names
#' @param genus field name for Genus field
#' @param species field name for Species field
#' @param subspecies field name for Subspecies field
#' @family Name functions
#' @return a data frame containing Canonical names field added or repopulated using
#'     filed names for Genus, Species and Subspecies specified in parameters
#' @examples
#' \dontrun{
#' mylist <- data.frame("genus" = c("Acodon", "Akodon", "Abrothrix", "Abeomelomys"),
#'                      "species" = c("jelskii","longipilis","longipilis", "sevia"),
#'                      "subspecies" = c("pyrrhotis","castaneus","", NA))
#' cast_canonical(mylist,"canonical","genus","species","subspecies")
#' }
#' @export
cast_canonical <- function(dat,canonical="canonical",genus="",
                           species="",subspecies=""){
  newdat <- as.data.frame(dat)
  newdat$canonical_ <- NA
  if(is.empty(canonical)){
    print("Empty")
    canonical <- "canonical"
  }
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
  } else {
    newdat$subspecies <- NA
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
    newdat$canonical_[i] <- proper(cano)
  }
  newdat <- rename_column(newdat,"genus",genus)
  newdat <- rename_column(newdat,"species",species)
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies",subspecies)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subspecies"))]
  }
  if((canonical == "canonical") & ("canonical" %in% names(newdat))){
    newdat$canonical <- newdat$canonical_
    newdat <- newdat[ , !(names(newdat) %in% c("canonical_"))]
  } else {
    if(canonical %in% names(newdat)){
      newdat <- newdat[ , !(names(newdat) %in% canonical)]
    }
    newdat <- rename_column(newdat,"canonical_",canonical)
  }
  return(newdat)
}
