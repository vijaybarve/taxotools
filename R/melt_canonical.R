#' Deconstruct canonical names
#'
#' Deconstruct canonical names into Genus, Species and Subspecies fields
#'
#' @param dat data frame containing taxonomic list
#' @param canonical field name for canonical names
#' @param genus field name for Genus
#' @param species field name for Species
#' @param subspecies field name for Subspecies
#' @family Name functions
#' @return a data frame containing Genus, Species and Subspecies fields
#'  added or repopulated using data in canonical name field.
#' @examples
#' \dontrun{
#'mylist <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Acodon hirtus",
#'                                     "Akodon longipilis apta",
#'                                     "AKODON LONGIPILIS CASTANEUS",
#'                                     "Chroeomys jelskii",
#'                                     "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = F)
#' melt_canonical(mylist,"canonical","genus","species","subspecies")
#' }
#' @export
melt_canonical <- function(dat,canonical="",genus="",species="",subspecies=""){
  newdat <- as.data.frame(dat)
  if(genus==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,genus,"genus",silent=TRUE)
    newdat$genus <- NA
  }
  if(species==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,species,"species",silent=TRUE)
    newdat$species <- NA
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies",silent=TRUE)
    newdat$subspecies <- NA
  }
  if(canonical!=""){
    newdat <- rename_column(newdat,canonical,"canonical")
  }
  pb = txtProgressBar(min = 0, max = nrow(newdat), initial = 0)
  for(i in 1:nrow(newdat)){
    if(!is.empty(newdat$canonical[i])){
      tl <- guess_taxo_level(newdat$canonical[i])
      newdat$genus[i] <- toproper(strsplit(newdat$canonical[i]," ")[[1]][1])
      if(tl=="Species" | tl=="Subspecies"){
        newdat$species[i] <- tolower(strsplit(newdat$canonical[i]," ")[[1]][2])
      }
      if(tl=="Subspecies" & subspecies!=""){
        newdat$subspecies[i] <- tolower(strsplit(newdat$canonical[i]," ")[[1]][3])
      }
    }
    setTxtProgressBar(pb,i)
  }
  newdat <- rename_column(newdat,"genus",genus)
  newdat <- rename_column(newdat,"species",species)
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies",subspecies)
  }
  newdat <- rename_column(newdat,"canonical",canonical)
  return(newdat)
}
