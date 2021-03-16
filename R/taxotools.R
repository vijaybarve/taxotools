#' taxotools: Tools to handle taxonomic data
#'
#' Tools include matching and merging taxonomic lists, casting and melting 
#' scientific names, managing taxonomic lists from GBIF and ITIS, harvesting 
#' names from wikipedia and fuzzy matching.
#'
#'@section List functions:
#'\itemize{
#'\item \link{cast_cs_field}
#'\item \link{DwC2taxo}
#'\item \link{match_lists}
#'\item \link{melt_cs_field}
#'\item \link{merge_lists}
#'\item \link{syn2taxo}
#'\item \link{synonymize_subspecies}
#'\item \link{taxo2DwC}
#'\item \link{taxo2syn}
#'\item \link{wiki2taxo}
#'}
#'
#'@section Name functions:
#'\itemize{
#'\item \link{build_gen_syn}
#'\item \link{cast_canonical}
#'\item \link{cast_scientificname}
#'\item \link{check_scientific}
#'\item \link{expand_name}
#'\item \link{get_accepted_names}
#'\item \link{guess_taxo_rank}
#'\item \link{list_higher_taxo}
#'\item \link{melt_canonical}
#'\item \link{melt_scientificname}
#'\item \link{resolve_names}
#'\item \link{taxo_fuzzy_match}
#'}
#'
#'@section ITIS functions:
#'\itemize{
#'\item \link{get_itis_syn}
#'\item \link{list_itis_syn}
#'}
#'
#'
#'@section Wiki functions:
#'\itemize{
#'\item \link{list_wiki_syn}
#'}
#'
#'@section Citation:
#'\itemize{
#'\item Barve, V., (2021). taxotools: Tools to handle
#'taxonomic data (R package V 0.0.79). Retrieved from
#'https://doi.org/10.5281/zenodo.3934939
#'}
#'
#' @docType package
#' @name taxotools
NULL
