
'%!in%' <- function(x,y)!('%in%'(x,y))

proper=function(x) paste0(toupper(substr(x, 1, 1)),
                          tolower(substring(x, 2)))

isupper <- function(chr){
  if(chr == toupper(chr)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.empty <- function(val){
  if(is.null(val)){
    return(TRUE)
  }
  if(is.na(val)){
    return(TRUE)
  }
  if(val==""){
    return(TRUE)
  }
  return(FALSE)
}

rename_column <- function(dat,old,new,silent=FALSE){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    if(!silent){
      print(paste("Fieldname not found...",old))
    }
  }
  return(dat)
}
