#' @title Synonym list to taxolist
#' @description Converts a Synonym list with Accepted Names and Synonym columns
#' to taxolist format
#' @param synlist Synonym list with Accepted name and Synonym columns
#' @param canonical Accepted names column name, Default: 'canonical'
#' @param synonym Synonym column name , Default: 'synonym'
#' @return returns a taxolist format list will all the names in same column and
#' accepted names linked to synonyms with id and accid fields
#' @details Converts a synonyms list to taxolist format
#' @family List functions
#' @examples
#' \dontrun{
#' if(interactive()){
#'  taxolist <- syn2taxo(synlist)
#'  }
#' }
#' @rdname syn2taxo
#' @export
syn2taxo <- function(synlist,
                     canonical="canonical",
                     synonym="synonym"){
  synlist <- rename_column(synlist,canonical,"Name_")
  synlist <- rename_column(synlist,synonym,"Syn_")
  # Accepeted Names
  synlist <- as.data.frame(synlist)
  canonical <- unique(synlist$Name_)
  Id <- seq(1:length(canonical))
  AccId <- 0
  if("source" %in% names(synlist)){
    source <- synlist$source[1]
  } else {
    source <- NA
  }
  taxo <- as.data.frame(cbind(Id,canonical,AccId,source),stringsAsFactors = F)
  for(i in 1:nrow(taxo)){
    taxo$taxonlevel[i] <-  guess_taxo_rank(taxo$canonical[i])
  }
  # Synonyms
  id <- length(canonical) + 1
  pb = txtProgressBar(min = 0, max = nrow(synlist), initial = 0)
  for(i in 1:nrow(synlist)){
    canonical <- synlist$Syn_[i]
    Id <-  id
    id <- id + 1
    AccId <- taxo[which(taxo$canonical==as.character(synlist$Name_[i])),c("Id")]
    taxolevel <- guess_taxo_rank(as.character(synlist$Syn_[i]))
    if(taxolevel=="Species" | taxolevel=="Subspecies"){
      rec <- data.frame("Id"=Id,"canonical"=canonical,
                        "AccId"=AccId,"source"=source,
                        "taxonlevel"=taxolevel)
      taxo <- rbind(taxo,rec)
    }
    setTxtProgressBar(pb,i)
  }
  cat("\n")
  taxo$order <- ""
  taxo$family <- ""
  taxo <- melt_canonical(taxo,"canonical","genus","species","subspecies")
  names(taxo) <- tolower(names(taxo))
  return(taxo)
}
