#' @title Generate URLs to search OPS biblios
#'
#' @param title - character
#' @param abstract - character
#' @param applicant  - character
#' @param inventor - character
#' @param citation - character
#' @param cpc - character
#' @param ipc - character
#' @param from - YYYY or YYYYMMDD, publication date.
#' @param to - YYYY or YYYYMMDD, publication date
#'
#' @return prints a query that is ready to be used in the Publication Data Keyword Search endpoint
#' @details This function generates a set of URLs with a maximum of 100 results per URL for searching either OPS titles, titles & abstracts, biblios (default) and/or date ranges (publication date). See Details.
#' @export
#' @examples \dontrun{urls <- get_quert(title = "pizza", applicant = "IBM", from = 2010, to = 2011)}


get_query <- function(title = NA, abstract = NA, titab = NA, applicant = NA,
                          inventor = NA, citation = NA, pub_num = NA, cpc = NA, ipc = NA, from = NA, to = NA) {

  baseurl <- "http://ops.epo.org/3.2/rest-services/published-data/search/biblio/?q="

  if(is.na(title) == FALSE) {
    title <- paste0("ti=", title)
  }
  if(is.na(abstract) == FALSE){
    abstract <- paste0("ab=", abstract)
  }
  if(is.na(titab) == FALSE){
    titab <- paste0("ta=", titab)
  }
  if(is.na(applicant) == FALSE){
    applicant <- paste0("pa=", applicant)
  }
  if(is.na(inventor) == FALSE){
    inventor <- paste0("in=", inventor)
  }
  if(is.na(citation)== FALSE){
    citation <-paste0("ct=", citation)
  }
  if(is.na(pub_num) == FALSE){
    pub_num <- paste0("pn=", pub_num)
  }
  if(is.na(cpc) == FALSE){
    cpc <- paste0("cpc=", cpc)
  }
  if(is.na(ipc) == FALSE){
    ipc <- paste0("ipc=", ipc)
  }

  if(is.na(from) == FALSE & is.na(to) == FALSE) {
    dates <- paste0("pd=%22", from, " ", to, "%22")
  }else{
    dates<-NA
  }


  query<-c(title, abstract, titab, applicant, inventor, citation, pub_num, cpc, ipc, dates)

  query<-query[!is.na(query)]

  query<-paste0(query, collapse = " and ")

  query<-gsub("=", "%3D", query)

  query<-gsub(" ", "%20", query)

  url<-paste0(baseurl, query)

  return(url)
}

