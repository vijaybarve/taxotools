#' Resolve canonical names against GNA
#'
#' Resolve names against Global Names Archtitecture (GNA) to make sure the name
#' esixts
#'
#' @param taxolist (data frame) taxonomic list
#' @param sciname () column name for scientific names
#' @param score_threshold (numeric) to mkae sure names match as desired. Dafault (0.98)
#' Higher value indicates best match, lower vaules would return matches at 
#' genus level 
#' @param best_match_only (logical) If TRUE, best match only returned else return
#' all records returned by GNA. Default: TRUE
#' @param add_fields (character) One of NA (default) , minimal or all. NA addts a 
#' locical column 'resolved', Minimal gives back just four fields, whereas all 
#' gives all fields back.
#' @param verbose (logical) verbose output, Default: FALSE
#' @return (data frame) names list resolves
#'
#' @family Name functions
#' @importFrom taxize gnr_resolve
#' @examples
#' \dontrun{
#' mylist <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                      "Acodon hirtus",
#'                                      "Akodon longipilis apta",
#'                                      "AKODON LONGIPILIS CASTANEUS",
#'                                      "Chroeomys jelskii",
#'                                      "Acodon jelskii pyrrhotis"),
#'                      stringsAsFactors = F)
#' test <- resolve_names(mylist)
#' test1 <- resolve_names(mylist,add_fields = "minimal")
#' test2 <- resolve_names(mylist,best_match_only = FALSE,add_fields = "minimal")
#' test3 <- resolve_names(mylist,best_match_only = FALSE,add_fields = "all")
#' }
#' @export
resolve_names <- function(taxolist,
                          sciname="canonical",
                          score_threshold=0.98,
                          best_match_only=TRUE,
                          add_fields= NA,
                          verbose=TRUE){
  taxolist <- rename_column(taxolist,sciname,"canonical__")
  taxolist$resolved <- FALSE
  taxores <- NULL
  get_fields <- ifelse(!is.na(add_fields) & add_fields=="all","all","minimal")
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(taxolist), initial = 0)}
  for(i in 1:nrow(taxolist)){
    recres <- gnr_resolve(sci  = c(taxolist$canonical__[i]),fields=get_fields)
    recres <- recres[which(recres$score>score_threshold),]
    if(nrow(recres)>0){
      # Only add a tag
      if(is.na(add_fields)){
        taxolist$resolved[i] <- TRUE
        if(verbose){setTxtProgressBar(pb,i)}
        next
      } 
      #uSe only best matching (first) result
      if(best_match_only){
        recres <- recres[1,]
      } 
      recres <- cbind(taxolist[i,], recres, row.names = NULL)
      taxores <- plyr::rbind.fill(taxores,recres)
    } 
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  if(is.na(add_fields)){
    taxolist <- rename_column(taxolist,"canonical__",sciname)
    return(taxolist) 
  }else {
    taxores <- rename_column(taxores,"canonical__",sciname)
    return(taxores)
  }
}
