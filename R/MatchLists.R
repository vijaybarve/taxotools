#' match two taxonomic lists
#'
#' This function is depreciated. Please use \link{match_lists}
#'
#' match two taxonomic lists using canonical names
#'
#' @param master master taxonomic list
#' @param lst match  taxonomic list
#' @param masterfld field name for canonical name in master list
#' @param lstfld field name for canonical name in match list
#' @family Discontinued functions
#' @return a list with two data frames containing matched and non-matched names from the master list
#' @examples
#' \dontrun{
#' MatchLists(master,lst,"canonical","canonical",)
#' }
#' @export
MatchLists <- function(master,lst,masterfld,lstfld){
  .Deprecated("match_lists")
  retval <- NULL
  if(masterfld==""){
    return(NULL)
  } else {
    master <- RenameColumn(master,masterfld,"masterfld")
  }
  if(lstfld==""){
    return(NULL)
  } else {
    lst <- RenameColumn(lst,lstfld,"lstfld")
  }
  retval$matchlist <- master[which(master$masterfld %in% lst$lstfld),]
  retval$nonmatchlist <- master[which(master$masterfld %!in% lst$lstfld),]
  retval$matchlist <- RenameColumn(retval$matchlist,"masterfld",masterfld)
  retval$nonmatchlist <- RenameColumn(retval$nonmatchlist,"masterfld",masterfld)
  return(retval)
}

'%!in%' <- function(x,y)!('%in%'(x,y))
