#' Get ITIS Synonyms for list of names
#'
#' Fetch Synonyms from ITIS
#'
#' @param namelist list of scientific names
#' @family ITIS functions
#' @return a data frame containing names (passed) and synonyms
#' @examples
#' GetITISSyn("Abrothrix longipilis")
#' GetITISSyn(c("Abditomys latidens", "Abeomelomys sevia", "Abrothrix jelskii" ))
#'
#' @export
GetITISSyn <- function(namelist){
  retset <- NULL
  for (i in 1:length(namelist)){
    set1 <- ListITISSyn(namelist[i])
    if(!is.null(set1)){
      set1 <- cbind(namelist[i],set1)
      retset <- rbind(retset,set1)
    }
  }
  retset <- as.data.frame(retset)
  names(retset) <- c("Name","Syn")
  return(retset)
}
