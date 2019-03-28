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
#' match_lists(master,checklist,"canonical","canonical",)
#' }
#' @export
match_lists <- function(master,checklist,masterfld,checklistfld){
  retval <- NULL
  if(masterfld==""){
    return(NULL)
  } else {
    master <- RenameColumn(master,masterfld,"masterfld")
  }
  if(checklistfld==""){
    return(NULL)
  } else {
    checklist <- RenameColumn(checklist,checklistfld,"checklistfld")
  }
  retval$matchlist <- master[which(master$masterfld %in% checklist$checklistfld),]
  retval$nonmatchlist <- master[which(master$masterfld %!in% checklist$checklistfld),]
  retval$matchlist <- RenameColumn(retval$matchlist,"masterfld",masterfld)
  retval$nonmatchlist <- RenameColumn(retval$nonmatchlist,"masterfld",masterfld)
  return(retval)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
