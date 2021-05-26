#' @title get synonyms 
#' @description get all the synonyms from the master list for all the names
#' in the checklist
#' @param master master list of names
#' @param checklist list of names to be processed
#' @param commasep return list should be comma seperated list or each synonym on
#' its own row. Default false to
#' @param verbose verbose output on the console
#' @return Data frame with names from the checklist and their synonyms present
#' in the master list
#' @importFrom plyr rbind.fill
#' @family List functions
#' @examples
#' \dontrun{
#' master <- data.frame("id" = c(1,2,3),
#'                      "canonical" = c("Hypochlorosis ancharia",
#'                                      "Hypochlorosis tenebrosa",
#'                                      "Hypochlorosis ancharia tenebrosa"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae"),
#'                      "accid" = c(0,1,0),
#'                      "source" = c("itis","itis","itis"),
#'                      stringsAsFactors = F)
#' 
#' checklist <- data.frame("id" = c(1,2,3,4,5),
#'                         "canonical" = c("Hypochlorosis ancharia",
#'                                         "Pseudonotis humboldti",
#'                                         "Myrina ancharia",
#'                                         "Hypochlorosis ancharia obiana",
#'                                         "Hypochlorosis lorquinii"),
#'                         "family" = c("Lycaenidae", "Lycaenidae", 
#'                                      "Lycaenidae", "Lycaenidae",
#'                                       "Lycaenidae"),
#'                         "accid" = c(0,1,1,0,0),
#'                         "source" = c("itis","wiki","wiki","itis",
#'                                      "itis"),
#'                         stringsAsFactors = F)
#' get_synonyms(master,checklist,commasep=FALSE)
#' get_synonyms(master,checklist,commasep=TRUE)
#' }
#' @rdname get_synonyms
#' @export
get_synonyms <- function(master = NULL,
                         checklist = NULL,
                         commasep=FALSE,
                         verbose = TRUE){
  if(is.null(master)){
    warning("master data missing")
    return(NULL)
  }
  if(is.null(checklist)){
    warning("checklist data missing")
    return(NULL)
  }
  master <- as.data.frame(master)
  checklist <- as.data.frame(checklist)
  addlist <- NULL
  names(master) <- tolower(names(master))
  names(checklist) <- tolower(names(checklist))
  # master <- compact_ids(master,"id","accid",1,verbose)
  idcount <- max(master$id) + 1
  checklist <- compact_ids(checklist,"id","accid",idcount,verbose)
  check_acc <- checklist[which(checklist$accid==0),]
  for(i in 1:nrow(check_acc)){
    if(verbose){cat(paste("\n",i))}
    recset <- get_id_recs(checklist,check_acc$id[i])
    if(!is.null(recset)){
      found <- FALSE
      found_count <- 0
      accid_set <- c()
      for(j in 1:nrow(recset)){
        if(recset$canonical[j] %in% master$canonical) {
          found <- TRUE
          set_accid <- get_accid(master,as.character(recset$canonical[j]),
                                 verbose)
          accid_set <- c(accid_set,set_accid)
          found_count <- found_count + 1
        }
      }
      if(length(unique(accid_set))==1){
        for(k in 1:dim(recset)[1]){
          if(verbose){cat("|")}
          addrec <- recset[k,]
          addrec$id <- idcount
          idcount <- idcount + 1
          addrec$accid <- set_accid
          addlist <- rbind(addlist,addrec)
          if(verbose){cat("+")}
        }
      }
    }
  }
  if(verbose){cat("\n")}
  addmast <- master[which(master$id %in% addlist$accid),]
  addlist <- rbind.fill(addlist,addmast)
  retval <- taxo2syn(addlist)
  retval <- retval[,c("canonical","synonym")]
  if(!commasep){
    retval <- melt_cs_field(retval,"synonym")
  }
  return(retval)
}
