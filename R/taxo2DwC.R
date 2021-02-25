#' @title Taxolist to Darwin Core (DwC)
#' @description Converts a taxolist to Darwin Core format
#' @param taxolist taxolist
#' @return returns a taxonomic list in DwC format
#' @details Converts a taxolist to Darwin Core format
#' @family List functions
#' @examples
#' \dontrun{
#'mytaxo <- data.frame("id" = c(1,2,3,4,5,6,7),
#'                     "canonical" = c("Hypochlorosis ancharia",
#'                                     "Hypochlorosis tenebrosa",
#'                                     "Pseudonotis humboldti",
#'                                     "Myrina ancharia",
#'                                     "Hypochlorosis ancharia tenebrosa",
#'                                     "Hypochlorosis ancharia obiana",
#'                                     "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                     "accid" = c(0,1,1,1,0,0,0),
#'                     "source" = c("itis","itis","wiki","wiki","itis",
#'                                  "itis","itis"),
#'                     stringsAsFactors = F)
#' mysynlst <- taxo2DwC(mytaxo)
#'}
#' @rdname taxo2DwC
#' @export
#' 
taxo2DwC <- function(taxolist){
  if(is.null(taxolist)){
    return(NULL)
  }
  if(nrow(taxolist)<1){
    return(NULL)
  }
  if("taxonlevel" %!in% names(taxolist)){
    taxolist$taxonlevel <- lapply(taxolist$canonical, guess_taxo_rank)
  }
  taxolist <- rename_column(taxolist,"id","taxonKey")
  taxolist <- rename_column(taxolist,"canonical","scientificName")
  taxolist <- rename_column(taxolist,"taxonlevel","taxonRank")
  taxolist <- rename_column(taxolist,"species","specificEpithet")
  taxolist <- rename_column(taxolist,"subspecies","infraspecificEpithetProperty")
  # Accepted names
  taxo_ac <- taxolist[which(taxolist$accid==0),]
  taxo_ac$acceptedTaxonKey <- taxo_ac$taxonKey
  taxo_ac$acceptedScientificName <- taxo_ac$scientificName
  taxo_ac$taxonomicStatus <- "Valid"
  # Synonyms
  taxo_syn <- taxolist[which(taxolist$accid!=0),]
  if(nrow(taxo_syn)>0){
    taxo_syn$acceptedTaxonKey <- taxo_syn$accid
    taxo_syn$taxonomicStatus <- "Synonym"
    taxo_syn$acceptedScientificName <- taxo_ac$scientificName[match(taxo_syn$acceptedTaxonKey,taxo_ac$taxonKey)]
    taxo_ac <- rbind(taxo_ac,taxo_syn)
  }
  taxo_ac <- taxo_ac[,!names(taxo_ac) %in%c("accid")]
  return(taxo_ac)
}

