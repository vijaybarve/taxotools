#' @title Darwin Core to Taxolist format
#' @description Converts a Darwin Core name list to taxolist format
#' @param namelist names list in Darwin Core format
#' @param statuslist vector listing taxonomicStatus to be considered in
#' the namelist. If Default value is NA, automatically uses list of
#' \itemize{\item{Accepted}\item{Synonym}\item{Valid}
#' \item{heterotypicSynonym}#' \item{homotypicSynonym}}
#' @param source source of the namelist. Default NA
#' @return names list is taxolist format
#' @details The name lists downloaded for ITIS website in Darwin Core format has
#' all the required fields. Just needs to be converted and quality checked in terms
#'  of missing linkages
#' @importFrom plyr rename
#' @importFrom stringr word
#' @examples
#' \dontrun{
#' if(interactive()){
#'  taxolist <- DwC2taxo(namelist)
#'  }
#' }
#' @rdname DwC2taxo
#' @export
DwC2taxo <- function(namelist,
                     statuslist=NA,
                     source=NA){
  if(is.na(statuslist)){
    statuslist <- c("Accepted","Synonym", "Valid",
                 "heterotypicSynonym","homotypicSynonym")
  }
  statuslist <- toupper(statuslist)
  if("taxonRank" %in% names((namelist))){
    namelist <- namelist[which(toupper(namelist$taxonRank) == "SPECIES" |
                                 toupper(namelist$taxonRank) == "SUBSPECIES"),]
  } else {
    warning("taxonRank not found.")
    return(NULL)
  }
  if("taxonomicStatus" %in% names((namelist))){
    namelist <- namelist[which(toupper(namelist$taxonomicStatus) %in% statuslist),]
  }
  if("taxonID" %in% names((namelist))){
    namelist <- rename(namelist,
                       replace = c("taxonID" = "id",
                                   "acceptedNameUsageID" = "accid",
                                   "specificEpithet" = "species",
                                   "infraspecificEpithet" = "subspecies",
                                   "taxonRank" = "taxonlevel"))
  }
  if("taxonKey" %in% names((namelist))){
    namelist <- rename(namelist,
                       replace = c("taxonKey" = "id",
                                   "acceptedTaxonKey" = "accid",
                                   "taxonRank" = "taxonlevel"))
    namelist$species <- word(namelist$species,2)
    namelist$accid[which(namelist$accid==namelist$id)] <- 0
    if("subspecies" %!in% names(namelist)){
      namelist <- melt_scientificname(namelist,
                                      sciname = "scientificName",
                                      genus = "genus_",
                                      species = "species_",
                                      subspecies = "subspecies")
    }
  }
  namelist[which(namelist$accid %in% namelist$id),]
  namelist <- cast_canonical(namelist,"canonical","genus","species","subspecies")
  namelist <- namelist[,c("id", "order", "family", "genus", "species",
                          "subspecies", "taxonlevel", "accid", "canonical")]
  namelist$source <- source
  return(namelist)
}
