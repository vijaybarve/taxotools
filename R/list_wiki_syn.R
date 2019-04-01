#' Get Wikipedia Synonyms for list of names
#'
#' Fetch Synonyms from Wikipedia and clean them for use
#'
#' @param namelist list of scientific names
#' @family Wiki functions
#' @return a data frame containing names, synonyms and Canonical synonyms matched
#'    with GBIF backbone taxonomy  \itemize{ \item{Name}  {: Scientific name}
#'    \item{OrigSyn}  {: Original synonym returned by Wikipedia}
#'  \item{Syn}  {: Synonym in canonical form, matched with GBIF}}
#' @importFrom wikitaxa wt_wikipedia
#' @importFrom taxize gbif_parse
#' @examples
#' list_wiki_syn("Abrothrix illutea")
#' #list_wiki_syn(c("Abditomys latidens", "Abeomelomys sevia", "Abrocoma schistacea"))
#'
#' @export
list_wiki_syn <- function(namelist){
  res <- NULL
  for(i in 1:length(namelist)){
    accname <- namelist[i]
    cat(paste("\n",i,accname," "))
    wikisyn <- wikitaxa::wt_wikipedia(accname)$synonyms
    wikiacn <- wikitaxa::wt_wikipedia(accname)$classification[which(wt_wikipedia(namelist[i])$classification$rank=="binomial"),]$name
    if(!is.null(wikiacn) & !identical(wikiacn, character(0)) ){
      if(accname!= wikiacn){
        wikisyn <- c(wikisyn,wikiacn)
      }
    }
    if(length(wikisyn)>0){
      wikisyn <- expand_name(accname,wikisyn)
      synlst <- NULL
      for(j in 1:length(wikisyn)){
        rec <- taxize::gbif_parse(wikisyn[j])
        syn <- check_scientific(rec$canonicalname)
        if(is.null(syn)){
          syn <- ''
        }
        syn_orig <- wikisyn[j]
        synrec <- cbind(accname,syn_orig,syn)
        synlst <- rbind(synlst,synrec)
        cat("+")
      }
      recs <- synlst
    } else {
      recs <- NULL
    }
    res <- rbind(res,recs)
  }
  res <- as.data.frame(res)
  names(res) <- c("Name","OrigSyn","Syn")
  cat("\n")
  return(res)
}
