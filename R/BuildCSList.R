#' Build a comma separated List
#'
#' Builds a comma separated list given a data frame with two fiends, primary
#' repeating values and secondary values to be summarized with comma separated
#' in the same field
#'
#' @param data data frame containing primary and secondary data columns
#' @param pri Primary field name (repeating values)
#' @param sec Secondary field (values would be added to same record,
#' comma separated)
#' @param duplicate If true, duplicate entries are allowed in secondary field
#' @param sepchar Character separator between the data items. Default is comma
#' @return a data frame with two fields Primary and secondary (comma
#' separated list)
#' @examples \dontrun{
#'scnames <- c("Abrothrix longipilis", "Abrothrix jelskii")
#'SynList <- GetITISSyn(scnames)
#'BuildCSList(SynList,"Name","Syn")
#'}
#'
#' @family List functions
#' @export
BuildCSList <- function(data,pri,sec,duplicate=FALSE,sepchar=","){
  tdata <- data
  colnames(tdata)[which(colnames(tdata) == pri)] <- 'pri'
  colnames(tdata)[which(colnames(tdata) == sec)] <- 'sec'
  if(!is.null(tdata)){
    tdata$pri <- as.character(tdata$pri)
    tdata$sec <- as.character(tdata$sec)
    tdata <- tdata[order(tdata$pri),]
    oldpri <- tdata$pri[1]
    oldrec <- tdata[1,]
    retdat <- NULL
    newsec <- tdata$sec[1]
    for(i in 2:dim(tdata)[1]){
      if(tdata$pri[i]==oldpri){
        newsec <- paste(newsec,sepchar," ",tdata$sec[i])
      } else {
        rec <- oldrec
        rec$sec <- newsec
        retdat <- rbind(retdat,rec)
        oldpri <- tdata$pri[i]
        oldrec <- tdata[i,]
        newsec <- tdata$sec[i]
      }
    }
    rec <- oldrec
    if(!duplicate){
      newsec <- dedup_csl(newsec,sepchar)
    }
    rec$sec <- newsec
    retdat <- rbind(retdat,rec)
    retdat <- as.data.frame(retdat)
    colnames(retdat)[which(colnames(retdat) == 'pri')] <- pri
    colnames(retdat)[which(colnames(retdat) == 'sec')] <- sec
    rownames(retdat) <- NULL
    return(retdat)
  } else {
    return(NULL)
  }
}

dedup_csl <- function(vec,sepchar){
  tmp <- strsplit(vec,sepchar)[[1]]
  tmp <- trimws(tmp)
  tmp <- unique(tmp)
  return(paste(tmp, collapse=paste(sepchar," ",sep="")))
}
