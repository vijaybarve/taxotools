#' @title Taxolist to document
#' @description Converts a taxolist to a formatted document in html, pdf or
#' word document
#' @param taxolist taxolist
#' @param genus only process for speciec genus. Default("") implying process 
#' all
#' @param source data source you want to print in output header
#' @param outformat output format one of "html_document", "word_document",
#' "odt_document", "rtf_document", "pdf_document". Default ("html_document")
#' @param outdir output directory for the document. Default (".")
#' @param outfile output file name. Dedfaout ("taxolist.html")
#' @importFrom rmarkdown render
#' @return NULL Saves a document file
#' @details Converts a taxolist to a formatted document in html, pdf or
#' word document making it easy for taxonomist to read through the data
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
#' taxo2doc(mytaxo)
#' taxo2doc(mytaxo,sourse="My list")
#'}
#' @rdname taxo2doc
#' @export
#' 
taxo2doc <- function(taxolist=NULL,genus=NA,source="",
                     outformat="html_document",
                     outdir=".",outfile="taxolist.html"){
  if(is.null(taxolist)){
    stop("No taxolist to process")
  }
  if("species" %!in% names(mytaxo)){
    taxolist <- melt_canonical(taxolist,
                               canonical="canonical",
                               genus="genus",
                               species="species",
                               subspecies="subspecies")
  }
  tfile <- tempfile("taxo_", fileext = c(".rmd"))
  con <- file(tfile)
  sink(con, append=TRUE)
  if(source==""){
    cat(paste('---\ntitle: "Taxonomic list" \n---\n\n'))
  } else {
    cat(paste('---\ntitle: "Taxonomic list: ',source,'"\n---\n\n'))
  }
  mytaxo <- taxolist
  mytaxo <- mytaxo[!duplicated(mytaxo$canonical),]
  mytaxo$family[which(is.na(mytaxo$family))] <- "-"
  if("author" %in% names(mytaxo)){
    mytaxo$author[which(is.na(mytaxo$author))] <- ""
  } else {
    mytaxo$author <- ""
  }
  mytaxo <- mytaxo[with(mytaxo, order(family,genus,species,subspecies)),]
  mytaxo <-mytaxo[which(mytaxo$species!="unidentifiable"),]
  if(!is.na(genus)){
    mytaxo <-mytaxo[which(mytaxo$genus %in% genus),]
  }
  cat(paste("  \n\n"))
  mytaxo_ac <- mytaxo[which(mytaxo$accid==0),]
  if(nrow(mytaxo_ac)==0){
    cat(paste("# ",source,"has nothing to display  \n"))
    sink() 
    return()
  }
  fam <- ""
  for(i in 1:nrow(mytaxo_ac)){
    if(mytaxo_ac$family[i]!=fam){
      cat(paste("  \n\n"))
      cat(paste("## Family: _",mytaxo_ac$family[i],"_  \n",sep=''))
      fam <- mytaxo_ac$family[i]
    }
    mytaxo_ac$author[i] <- utf2ascii(mytaxo_ac$author[i])
    cat(paste(i," _",mytaxo_ac$canonical[i],"_ ",mytaxo_ac$author[i],"  \n",sep = ''))
    if(nrow(mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),])>0){
      synlst <- mytaxo[which(mytaxo$accid==mytaxo_ac$id[i]),]
      for(j in 1:nrow(synlst)){
        synlst$author[j] <- utf2ascii(synlst$author[j])
        cat(paste("\t  = _",synlst$canonical[j],"_ " ,synlst$author[j],"  \n",sep = ''))
      }
    }
  }
  sink() 
  rmarkdown::render(input=tfile,
                    output_format=outformat,
                    output_dir = outdir,
                    output_file = outfile)
  return(NULL)
}