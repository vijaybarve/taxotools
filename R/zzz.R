
'%!in%' <- function(x,y)!('%in%'(x,y))

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

rename_column <- function(dat,old,new){
  if(old %in% colnames(dat)){
    colnames(dat)[which(names(dat) == old)] <- new
  } else {
    print(paste("Error: Fieldname not found...",old))
  }
  return(dat)
}
