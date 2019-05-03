#' match two taxonomic lists
#'
#' match two taxonomic lists using canonical names
#'
#' @param master master taxonomic list
#' @param checklist match  taxonomic list
#' @param masterfld field name for canonical name in master list
#' @param checklistfld field name for canonical name in match list
#' @family list functions
#' @return a list with two data frames containing matched and non-matched
#' names from the master list
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
  retval$nonmatchlist <- master[which(master$masterfld %!in% checklist$checklistfld),]
  retval$matchlist <- rename_column(retval$matchlist,"masterfld",masterfld)
  retval$nonmatchlist <- rename_column(retval$nonmatchlist,"masterfld",masterfld)
  return(retval)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
