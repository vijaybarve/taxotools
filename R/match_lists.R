#' match two taxonomic lists
#'
#' match two taxonomic lists using canonical names
#'
#' @param master master taxonomic list
#' @param checklist match  taxonomic list
#' @param masterfld field name for canonical name in master list
#' @param checklistfld field name for canonical name in match list
#' @family list functions
#' @return a list with data frames containing matched records,
#' records only in master and cheklist and statistics about the
#' records including Jaccard index
#' @examples
#' \dontrun{
#' match_lists(master,checklist,"canonical","canonical")
#' }
#' @export
match_lists <- function(master,checklist,masterfld,checklistfld){
  retval <- NULL
  if(masterfld==""){
    return(NULL)
  } else {
    master <- rename_column(master,masterfld,"masterfld")
    master$masterfld <- as.character(master$masterfld)
  }
  if(checklistfld==""){
    return(NULL)
  } else {
    checklist <- rename_column(checklist,checklistfld,"checklistfld")
    checklist$checklistfld <- as.character(checklist$checklistfld)
  }
  retval$matchlist <- master[which(master$masterfld %in% checklist$checklistfld),]
  retval$onlymaster <- master[which(master$masterfld %!in% checklist$checklistfld),]
  retval$onlychecklist <-  checklist[which(checklist$checklistfld %!in% master$masterfld),]
  retval$matchlist <- rename_column(retval$matchlist,"masterfld",masterfld)
  retval$onlymaster <- rename_column(retval$onlymaster,"masterfld",masterfld)
  retval$onlychecklist <- rename_column(retval$onlychecklist,"checklistfld",checklistfld)
  retval$stat$masterrec <- dim(master)[1]
  retval$stat$checkrec <- dim(checklist)[1]
  retval$stat$match <- dim(retval$matchlist)[1]
  retval$stat$onlymaster <- dim(retval$onlymaster)[1]
  retval$stat$onlychecklist <- dim(retval$onlychecklist)[1]
  retval$stat$jacard <- dim(retval$matchlist)[1] / ( dim(retval$matchlist)[1] +
                                                       dim(retval$onlymaster)[1] +
                                                       dim(retval$onlychecklist)[1] )
  return(retval)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
