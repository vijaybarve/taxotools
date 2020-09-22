#' @title Darwin Core to Taxolist format
#' @description Converts a Darwin Core name list to taxolist format
#' @param namelist names list in Darwin Core format
#' @return names list is taxolist format
#' @details The name lists downloaded for ITIS website in Darwin Core format has
#' all the required fields. Just needs to be converted and quality checked in terms
#'  of missing linkages
#' @importFrom plyr rename
#' @examples
#' \dontrun{
#' if(interactive()){
#'  taxolist <- DwC2taxo(namelist)
#'  }
#' }
#' @rdname DwC2taxo
#' @export
DwC2taxo <- function(namelist){
  namelist <- namelist[which(namelist$taxonRank == "species" |
                               namelist$taxonRank == "subspecies"),]
  namelist <- rename(namelist,
                     replace = c("taxonID" = "id",
                                 "acceptedNameUsageID" = "accid",
                                 "specificEpithet" = "species",
                                 "infraspecificEpithet" = "subspecies",
                                 "taxonRank" = "taxonlevel"))
  # melt_authyear
  namelist[which(namelist$accid %in% namelist$id),]
  namelist <- cast_canonical(namelist,"canonical","genus","species","subspecies")
  namelist <- namelist[,c("id", "order", "family", "genus", "species",
                          "subspecies", "taxonlevel", "accid", "canonical")]
  return(namelist)
}


