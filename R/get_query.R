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
                      inventor = NA, citation = NA, pub_num = NA, cpc = NA, ipc = NA, from = NA, to = NA,
                      merge_classification = FALSE, with_biblio = FALSE) {
  if(with_biblio == TRUE){
    baseurl <- "http://ops.epo.org/3.2/rest-services/published-data/search/biblio/?q="
  } else {
    baseurl <- "http://ops.epo.org/3.2/rest-services/published-data/search/?q="
  }
  if(is.na(title) == FALSE) {
    if(length(title)> 1){
      title <- paste(title, collapse = " or ")
      title <- paste0("(", title,")")
    }
    title <- paste0("ti=", title)

  }
  if(is.na(abstract) == FALSE){

    if(length(abstract)> 1){
    abstract <- paste(abstract, collapse = " or ")
    abstract <- paste0("(", abstract,")")
  }
    abstract <- paste0("ab=", abstract)

  }
  if(is.na(titab) == FALSE){
    if(length(titab) > 1){
      titab <- paste(titab, collapse = " or ")
      titab <- paste0("(", titab,")")
    }
      titab <- paste0("ta=", titab)

  }
  if(is.na(applicant) == FALSE){
    if(length(applicant) > 1){
      applicant <- paste(applicant, collapse = " or ")
      applicant <- paste0("(", applicant,")")
    }
    applicant <- paste0("pa=", applicant)
  }
  if(is.na(inventor) == FALSE){
    if(length(inventor) > 1){
      inventor <- paste(inventor, collapse = " or ")
      inventor <- paste0("(", inventor,")")
    }
    inventor <- paste0("in=", inventor)
  }
  if(is.na(citation)== FALSE){
    citation <-paste0("ct=", citation)
  }
  if(is.na(pub_num) == FALSE){
    if(length(pub_num) > 1){
      pub_num <- paste(pub_num, collapse = " or ")
      pub_num <- paste0("(", pub_num,")")
    }
    pub_num <- paste0("pn=", pub_num)
  }
  if(is.na(cpc) == FALSE){
    if(length(cpc) > 1){
      cpc <- paste(cpc, collapse = " or ")
      cpc <- paste0("(", cpc,")")
      cpc <- paste0("cpc=", cpc)
    } else {
      cpc <- paste0("cpc=", cpc)
    }
  }
  if(is.na(ipc) == FALSE){
    if(length(ipc) > 1){
      ipc <- paste(ipc, collapse = " or ")
      ipc <- paste0("(", ipc,")")
      ipc <- paste0("ipc=", ipc)
    } else {
      ipc <- paste0("ipc=", ipc)
    }

  }

  if(is.na(from) == FALSE & is.na(to) == FALSE) {
    dates <- paste0("pd=%22", from, " ", to, "%22")
  }else{
    dates<-NA
  }

  if(merge_classification == TRUE){
    class <- paste(cpc, ipc, sep = " or ")
    class <- paste0("(", class, ")")
    query<-c(title, abstract, titab, applicant, inventor, citation, pub_num, dates, class)
  } else {
    query<-c(title, abstract, titab, applicant, inventor, citation, pub_num, dates, cpc, ipc)
  }

  query<-query[!is.na(query)]

  query<-paste0(query, collapse = " and ")

  query<-gsub("=", "%3D", query)

  query<-gsub(" ", "%20", query)

  query <- gsub("/", "%2F", query)

  url<-paste0(baseurl, query)

  return(url)
}

