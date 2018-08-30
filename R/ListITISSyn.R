#' Get list ITIS Synonyms for a Scientific Name
#'
#' Fetch Synonyms using ITIS web service
#'
#' @param scname Scientific Name
#' @return a list containing synonyms
#' @importFrom taxize get_tsn synonyms
#' @family ITIS functions
#' @examples
#' ListITISSyn("Abrothrix longipilis")
#' ListITISSyn("Abditomys latidens")
#'
#' @export
ListITISSyn <- function(scname){
  tsn <- get_tsn(scname, rows=1)[1]
  t1 <- NULL
  if(!is.na(tsn)){
    syn <- synonyms(tsn,db="itis")
    eval(parse(text=paste("t <- (syn$'",tsn,"')",sep='')))
    if(!is.null(t)){
      eval(parse(text=paste("t <- (syn$'",tsn,"'$acc_name)",sep='')))
      eval(parse(text=paste("t1 <- (syn$'",tsn,"'$syn_name)",sep='')))
      return(unique(c(t,t1)))
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}
