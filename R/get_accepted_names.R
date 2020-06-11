#' @title get_accepted_name
#' @description Match namelist with master and fetch the accepted names
#' using the linkages provided within the data
#' @param master data frame with required columns id, canonical and accid.
#' Other columns like order, family are optional. Column id is typically
#' running ids for each record and accid will contain 0 if the name is
#' currently accepted name and id number of accepted name in case the name
#' is a synonym. Column canonical contains bionomial or trinomial without
#' spp. var. etc.
#' @param gen_syn data frame with columns Original_Genus and Valid_Genus
#'  where Original_genus is synonym and valid_genus is one present in the
#'  master. Default: NA when gen_syn is not used.
#' @param namelookup Lookup data frame for names where some neames might
#' need manual lookup. The columns required are binomial and validname
#' where binomial is new name and validname is present in the master.
#'  Default: NA when namelookup is not used.
#' @param namelist data frame of the list of names to be resolved. Must
#' contain either column canonical containing binomial or trinominal name
#' without spp. and var. etc. or may contain columns for genus, species
#' and subspecies (any subspecific unit) and the names of the columns are
#' passed as subsequent parameters.
#' @param canonical column containing names to be resolved to accepted names
#' , Default: NA when columns for genus and species are specified.
#' @param genus column containing genus names to be resolved to accepted
#' names and typically accompanied by species and subspecies columns, Default: NA
#' when canonical parameter is supplied.
#' @param species column containing species names to be resolved to accepted
#' names and is accompanied by genus, Default: NA
#' @param subspecies column containing species names to be resolved to accepted
#' names and is accompanied by genus and species, Default: NA
#' @param verbose display process messages, Default: TRUE
#' @return data frame containing all the original columns with following
#' additional columns:\itemize{
#' \item{accepted_name - }{Accepted name present in the master. NA is not resolved}
#' \item{method - }{method used to resolve the name. See details for explanation
#' of each method}
#' }
#' @details
#' Name resolution methods:\itemize{
#' \item{direct - }{was a direct match with name or a synonym}
#' \item{fuzzy - }{used fuzzy matching}
#' \item{gensyn - }{genus substitution with known genus level synonyms}
#' \item{lookup - }{Manual lookup in earlier processing}
#' \item{sppdrop - }{subspecies was dropped}
#' \item{sub2sp - }{subspecies elevated to species}
#' \item{NA - }{could not be resolved}
#' }
#'
#' Note: Make sure all the data frames have same character encoding to prevent
#'  errors.
#' @importFrom stringr word
#' @family Name functions
#' @examples
#' \dontrun{
#'master <- data.frame("id" = c(1,2,3,4,5,6,7),
#'                     "canonical" = c("Hypochlorosis ancharia",
#'                                     "Hypochlorosis tenebrosa",
#'                                     "Pseudonotis humboldti",
#'                                     "Myrina ancharia",
#'                                     "Hypochlorosis ancharia tenebrosa",
#'                                     "Hypochlorosis ancharia obiana",
#'                                     "Hypochlorosis lorquinii"),
#'                     "accid" = c(0,1,1,1,0,0,0),
#'                     stringsAsFactors = F)
#'
#'mylist <- data.frame("id"= c(11,12,13,14,15,16,17,18,19),
#'                     "scname" = c("Hypochlorosis ancharia",
#'                                  "Hypochlorosis ancharii",
#'                                  "Hypochlorosis tenebrosa",
#'                                  "Pseudonotis humboldtii",
#'                                  "Abrothrix longipilis",
#'                                  "Myrinana anchariana",
#'                                  "Hypochlorosis ancharia ancharia",
#'                                  "Myrina lorquinii",
#'                                  "Sithon lorquinii"),
#'                     stringsAsFactors = F)
#'
#'res <- get_accepted_names(master=master,
#'                          namelist = mylist,
#'                          canonical = "scname")
#'
#'gen_syn_list <- data.frame("Original_Genus"=c("Pseudonotis",
#'                                              "Myrina"),
#'                           "Valid_Genus"=c("Hypochlorosis",
#'                                           "Hypochlorosis"),
#'                           stringsAsFactors = F)
#'
#'res <- get_accepted_names(master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelist = mylist,
#'                          canonical = "scname")
#'
#'lookup_list <- data.frame("binomial"=c("Sithon lorquinii",
#'                                       "Hypochlorosis humboldti"),
#'                          "validname"=c("Hypochlorosis lorquinii",
#'                                        "Hypochlorosis lorquinii"),
#'                          stringsAsFactors = F)
#'
#'res <- get_accepted_names(master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelookup = lookup_list,
#'                          namelist = mylist,
#'                          canonical = "scname")
#'
#'mylist_s <- melt_canonical(mylist,canonical = "scname",
#'                           genus = "genus",
#'                           species = "species",
#'                           subspecies = "subspecies")
#'
#'res <- get_accepted_names(master=master,
#'                          gen_syn = gen_syn_list,
#'                          namelookup = lookup_list,
#'                          namelist = mylist_s,
#'                          genus = "genus",
#'                          species = "species",
#'                          subspecies = "subspecies")
#' }
#' @rdname get_accepted_names
#' @export

get_accepted_names <- function(master,gen_syn=NA,namelookup=NA,namelist,
                               canonical=NA, genus=NA,species=NA,subspecies=NA,
                               verbose=TRUE){
  # Set the data
  names(master) <- tolower(names(master))

  if(is.na(canonical)){
    if(verbose){cat("\nConstructing canonical name field")}
    namelist <- cast_canonical(namelist,"canonical",genus,species,subspecies)
    canonical="canonical"
  }
  if(!missing(gen_syn)){
    gen_synr <- data.frame(cbind(gen_syn[,2],gen_syn[,1]))
    names(gen_synr) <- names(gen_syn)
    gen_syn <- rbind(gen_syn,gen_synr)
    gen_syn <<- gen_syn[!duplicated(gen_syn),]
  }
  namelist <- rename_column(namelist,canonical,"canonical_")
  new <- namelist
  #--- Add a dummy rec
  tmprec <- new[1,]
  tmprec$canonical_ <- master$canonical[1]
  new <- rbind(new,tmprec)
  #---
  new$orig_canonical <- new$canonical_
  new$method <- NA
  new$source <- NA
  new$id_ <- 0
  new$accid_ <- 0

  # Get Name lookup table names replaced
  if(!missing(namelookup)){
    if(verbose){cat("\nUsing lookup table")}
    for(i in 1:nrow(new)){
      if(new$orig_canonical[i] %in% namelookup$binomial) {
        if(verbose){cat("+")}
        new$canonical_[i] <- namelookup$validname[which(new$orig_canonical[i]==namelookup$binomial)]
        new$method[i] <- "lookup"
      }
    }
  }

  # Remove repeating species and subspecies
  if(verbose){cat("\nRemoving repeating subspecies names")}
  for(i in 1:nrow(new)){
    if(length(strsplit(new$canonical_[i],' ')[[1]])>2){
      if(word(new$canonical_[i],2,2)==word(new$canonical_[i],3,3)){
        new$canonical_[i] <- word(new$canonical_[i],1,2)
        new$method[i] <- "repsub"
      }
    }
  }

  # Direct Matches
  if(verbose){cat("\nFetching accepted names")}
  new$id_<- master$id[match(new$canonical_, master$canonical)]
  new$accid_<- master$accid[match(new$canonical_, master$canonical)]
  new$newid_[which(new$accid_==0)] <- new$id_[which(new$accid_==0)]
  new$newid_[which(new$accid_!=0)] <- new$accid_[which(new$accid_!=0)]
  new$accepted_name <- master$canonical[match(new$newid_, master$id)]
  new$source <- master$source[match(new$id_, master$id)]
  new$method[which(is.na(new$method) & !is.na(new$accepted_name))] <- "direct"


  # Genus swap
  if(!missing(gen_syn)){
    if(verbose){cat("\nTrying Genus level synonyms\n")}
    for(i in 1:nrow(new)){
      #print(i)
      if(is.na(new$accepted_name[i])){
        curgenus <- word(new$canonical_[i],1,1)
        genlist <- as.character(gen_syn[which(gen_syn$Valid_Genus==
                                                curgenus),
                                        c("Original_Genus")])
        if(length(genlist)>0){
          for(j in 1:length(genlist)){
            combi_name <- paste(genlist[j],
                                word(new$canonical_[i],
                                     2,length(strsplit(new$canonical_[i],
                                                       ' ')[[1]])))
            name_match <- master[which(master$canonical==combi_name),]
            if(dim(name_match)[1]==1){
              if(name_match$accid[1]==0){
                match_rec <- name_match
                if(verbose){cat("+")}
              } else {
                match_rec <- master[which(master$id==name_match$accid[1]),]
                if(verbose){cat("*")}
              }
              new$accepted_name[i] <- match_rec$canonical[1]
              new$source[i] <- name_match$source[1]
              new$method[i] <- "gensyn"
            }
          }
        }
      }
    }
  }

  # Subspecies to species
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(verbose){cat("\nTrying Subspecies to species\n")}
    for(i in 1:nrow(new)){
      if(is.na(new$accepted_name[i]) &
         length(strsplit(new$canonical_[i],' ')[[1]])>2){
        swap_name <- paste(word(new$canonical_[i],1,1),word(new$canonical_[i],3,3))
        name_match <- master[which(master$canonical==swap_name),]
        if(dim(name_match)[1]==1){
          if(name_match$accid[1]==0){
            match_rec <- name_match
            if(verbose){cat("+")}
          } else {
            match_rec <- master[which(master$id==name_match$accid[1]),]
            if(verbose){cat("*")}
          }
          new$accepted_name[i] <- match_rec$canonical[1]
          new$source[i] <- name_match$source[1]
          new$canonical_[i] <- swap_name
          new$method[i] <- "sub2sp"
        }
      }
    }
  }

  # Dropping subspecies
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(verbose){cat("\nTrying dropping Subspecies\n")}
    for(i in 1:nrow(new)){
      if(is.na(new$accepted_name[i]) &
         length(strsplit(new$canonical_[i],' ')[[1]])>2){
        drop_name <- word(new$canonical_[i],1,2)
        name_match <- master[which(master$canonical==drop_name),]
        if(dim(name_match)[1]==1){
          if(name_match$accid[1]==0){
            match_rec <- name_match
            if(verbose){cat("+")}
          } else {
            match_rec <- master[which(master$id==name_match$accid[1]),]
            if(verbose){cat("*")}
          }
          new$accepted_name[i] <- match_rec$canonical[1]
          new$source[i] <- name_match$source[1]
          new$canonical_[i] <- drop_name
          new$method[i] <- "sppdrop"
        }
      }
    }
  }

  # Fuzzy matches
  if(nrow(new[which(is.na(new$accepted_name)),])>0){
    if(verbose){cat("\nTrying Fuzzy Matches\n")}
    for(i in 1:nrow(new)){
      if(is.na(new$accepted_name[i]) & !is.na(new$canonical_[i])){
        if(verbose){cat(paste("\n",new$canonical_[i]," "))}
        fres <- taxo_fuzzy_match(new$canonical_[i],master,dist=3)
        if(!is.null(fres)){
          new$canonical_[i] <- fres$canonical
          name_match <- master[which(master$canonical==fres$canonical),]
          if(verbose){cat(name_match$source[1])}
          if(dim(name_match)[1]==1){
            if(name_match$accid[1]==0){
              match_rec <- name_match
              if(verbose){cat("+")}
            } else {
              match_rec <- master[which(master$id==name_match$accid[1]),]
              if(verbose){cat("*")}
            }
            new$accepted_name[i] <- match_rec$canonical[1]
            new$source[i] <- name_match$source[1]
            new$method[i] <- "fuzzy"
          }
        }
      }
    }
  }

  # Get higher taxonomy
  new$family <- master$family[match(new$canonical_, master$canonical)]
  new$subfamily <- master$subfamily[match(new$canonical_, master$canonical)]
  new$tribe <- master$tribe[match(new$canonical_, master$canonical)]

  # Cleanup and return data
  new <- new[,-which(names(new) %in% c("id_","accid_","newid_"))]
  new <- new[-nrow(new),]
  if(!is.na(canonical)){
    new <- rename_column(new,"canonical_",canonical)
  }
  if(verbose){cat("\nDone")}
  return(new)
}
