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
#' @return a data frame with two fiends Primary and secondary (comma
#' separated list)
#' @family List functions
#' @export
BuildCSList <- function(data,pri,sec){
  tdata <- data[,c(pri,sec)]
  if(!is.null(tdata)){
    names(tdata) <- c("pri","sec")
    tdata$pri <- as.character(tdata$pri)
    tdata$sec <- as.character(tdata$sec)
    tdata <- tdata[order(tdata$pri),]
    oldpri <- tdata$pri[1]
    first <- TRUE
    retdat <- NULL
    newsec <- tdata$sec[1]
    for(i in 2:dim(tdata)[1]){
      if(tdata$pri[i]==oldpri){
        newsec <- paste(newsec,", ",tdata$sec[i])
      } else {
        rec <- cbind(oldpri,newsec)
        retdat <- rbind(retdat,rec)
        oldpri <- tdata$pri[i]
        newsec <- tdata$sec[i]
      }
    }
    rec <- cbind(oldpri,newsec)
    retdat <- rbind(retdat,rec)
    retdat <- as.data.frame(retdat)
    names(retdat) <- c(pri,sec)
    return(retdat)
  } else {
    return(NULL)
  }
}
