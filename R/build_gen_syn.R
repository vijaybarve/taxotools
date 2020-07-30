#' @title Build genetic level synonyms
#' @description Build a genus level synonym list from master list.
#' @param dat master list
#' @return data frame with genus level synonyms with two columns viz. Valid_genus
#' and Original_Genus
#' @details This is to #'  be passed on to get_accepted_names function
#'  as a parameter
#' @examples
#' \dontrun{
#' if(interactive()){
#'  gen_syn <- build_gen_syn(master)
#'  }
#' }
#' @rdname build_gen_syn
#' @export
build_gen_syn <- function(dat){
  names(dat) <- tolower(names(dat))
  mylist <- NULL
  dat <- melt_canonical(dat,"canonical","genus","species","subspecies")
  valid_gen <- unique(dat[which(dat$accid==0),c("genus")])
  for(i in 1:length(valid_gen)){
    valid_gen_ids <- dat[which(dat$genus==valid_gen[i]),c("id")]
    syn_gen <- unique(dat[which(dat$accid %in% valid_gen_ids),c("genus")])
    if(length(syn_gen)>0){
      for(j in 1:length(syn_gen)){
        myrec <- data.frame("Valid_Genus"=valid_gen[i],
                            "Original_Genus"=syn_gen[j],
                            stringsAsFactors = F)
        mylist <- rbind(mylist,myrec)
      }
    }
  }
  mylist <- mylist[which(mylist$Valid_Genus != mylist$Original_Genu),]
  return(mylist)
}
