#' Deconstruct canonical names
#'
#' Deconstruct canonical names into Genus, Species and Subspecies fields.
#'
#' @param dat data frame containing taxonomic list
#' @param canonical field name for canonical names
#' @param genus field name for Genus
#' @param species field name for Species
#' @param subspecies field name for Subspecies
#' @family Name functions
#' @return a data frame containing Genus, Species and Subspecies fields added or or repopulated
#'  using data in canonical name field.
#' @examples
#' \dontrun{
#' melt_canonical(mylist,"canonical","genus","species","subspecies")
#' }
#' @export
melt_canonical <- function(dat,canonical="",genus="",species="",subspecies=""){
  reglst <- c("f.","sp.", "var.","ab.","spp.","cf.")
  newdat <- as.data.frame(dat)
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
  if(canonical!=""){
    newdat <- rename_column(newdat,canonical,"canonical")
  }
  newdat$canonical <- trimws(newdat$canonical)
  for(i in 1:dim(newdat)[1]){
    if(!is.empty(newdat$canonical[i])){
      tl <- guess_taxo_level(newdat$canonical[i])
      newdat$genus[i] <- strsplit(newdat$canonical[i]," ")[[1]][1]
      if(tl=="Species" | tl=="Subspecies"){
        newdat$species[i] <- strsplit(newdat$canonical[i]," ")[[1]][2]
      }
      if(tl=="Subspecies" & subspecies!=""){
        newdat$subspecies[i] <- strsplit(newdat$canonical[i]," ")[[1]][3]
      }
      if(tl=="Unknown"){
        nwords <- length(strsplit(newdat$canonical[i]," ")[[1]])
        ct <- 2
        sp <- strsplit(newdat$canonical[i]," ")[[1]][ct]
        # If the names is in (), it will be a subgenus and not species
        if(substr(sp,1,1)=="("){
          ct <- ct+1
          sp <- strsplit(newdat$canonical[i]," ")[[1]][ct]
        }
        ct <- ct+1
        spp <- strsplit(newdat$canonical[i]," ")[[1]][ct]
        if(spp=="spp."){
          ct <- ct+1
          if(nwords>=ct){
            spp <- strsplit(newdat$canonical[i]," ")[[1]][ct]
          }
        }
        # Subspecies name should not start with Capital character. That will be author
        if(isupper(substr(spp,1,1))){
          spp <- ""
        }
        # We need to remove certain strings like sp. f. etc.
        if(spp %in% reglst){
          spp <- ""
        }
        newdat$species[i] <- sp
        newdat$subspecies[i] <- spp
      }
    }
  }
  newdat <- rename_column(newdat,"genus",genus)
  newdat <- rename_column(newdat,"species",species)
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies",subspecies)
  }
  newdat <- rename_column(newdat,"canonical",canonical)
  return(newdat)
}

isupper <- function(chr){
  if(chr == toupper(chr)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
