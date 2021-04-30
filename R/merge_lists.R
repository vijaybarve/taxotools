#' @title merge two lists of names
#' @description Useful in generating a master list of names from multiple
#' sources
#' @param master master list of names
#' @param checklist list to be merged
#' @param verbose verbose output on the console
#' @return returns three components. First the names to be added, second
#' the names that could not be matched and third the names that matched
#' multiple names in master
#' @details Matches names is checklist with names on master
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
#' merged <- merge_lists(master,checklist)
#' }
#' @rdname merge_lists
#' @export
merge_lists <- function(master = NULL,
                        checklist = NULL,
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
  retval <- NULL
  addlist <- NULL
  noaddlist <- NULL
  multilist <- NULL
  names(master) <- tolower(names(master))
  names(checklist) <- tolower(names(checklist))
  master <- compact_ids(master,"id","accid",verbose)
  checklist <- compact_ids(checklist,"id","accid",verbose)
  idcount <- max(master$id) + 1
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
      if(length(unique(accid_set))==0){
        noaddlist <- rbind(noaddlist,recset)
        if(verbose){cat("-")}
        next
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
      } else {
        multilist <- rbind(multilist,recset)
        if(verbose){print(accid_set)}
        if(verbose){cat("*")}
      }
    }
  }
  retval$addlist <- addlist
  retval$noaddlist <- noaddlist
  retval$multilist <- multilist
  return(retval)
}

get_id_recs <- function(checklist,id){
  retset <- NULL
  rec <- checklist[which(checklist$id==id),]
  recs <- checklist[which(checklist$accid==id),]
  retset <- rbind(retset,rec,recs)
  if(dim(retset)[1]<1){
    return(NULL)
  } else {
    return(retset)
  }
}

get_accid <- function(master,name,verbose=FALSE){
  if(verbose){cat(".")}
  id <- 0
  mset <- master[which(master$canonical==name),]
  if(dim(mset)[1]>0){
    if(mset[1,c("accid")]==0){
      id <- mset[1,c("id")]
    } else {
      id <- mset[1,c("accid")]
    }
  }
  return(as.numeric(id))
}
