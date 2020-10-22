#' @title Generate urls to be fed into get_ops function
#'
#' @param title - Character vector containing string to be searched in the title field
#' @param abstract - Character vector containing string to be searched in the abstract field
#' @param titab - Character vector containing string to be searched in the title or abstract field
#' @param applicant  - Character vector containing string to be searched in the applicant field
#' @param inventor - Character vector containing string to be searched in the inventor field
#' @param citation - Character vector containing DOCDB identification to be searched in the citation field
#' @param pub_num - Character vector containing publication number. It can also be used to search patents granted by a specific patent authority, using their country code
#' @param cpc - Character vector containing Cooperative Patent Classification code
#' @param ipc - Character vector containing International Patent Classification code
#' @param from - YYYY or YYYYMMDD, publication date.
#' @param to - YYYY or YYYYMMDD, publication date
#' @param with_biblio - Boolean. If true, the results returns with bibliographic information attached
#' @return url
#' @details This function returns queries that can be provided as inputs to the get_ops function. Remember that the ops accepts only queries made of maximum 20 components or 10 components per field.
#' @export get_query
#' @examples \dontrun{urls <- get_query(title = "pizza", applicant = "IBM", from = 2010, to = 2011)}


get_query <- function(title = NA, abstract = NA, titab = NA, applicant = NA,
                      inventor = NA, citation = NA, pub_num = NA, cpc = NA, ipc = NA, from = NA, to = NA,
                      merge_classification = FALSE, with_biblio = FALSE) {

  # Evaluate whether to retrieve search results with bibliographic data attached
  if(with_biblio == TRUE){
    baseurl <- "http://ops.epo.org/3.2/rest-services/published-data/search/biblio/?q="
  } else {
    baseurl <- "http://ops.epo.org/3.2/rest-services/published-data/search/?q="
  }

  # Series of if clause that build the query
  if(is.na(title) == FALSE) {
    # Evaluate whether to search for multiple values
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
  # Title and abstract
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

  # Paste query
  query<-c(title, abstract, titab, applicant, inventor, citation, pub_num, dates, cpc, ipc)

  # Remove NA values
  query<-query[!is.na(query)]

  # Paste query together
  query<-paste0(query, collapse = " and ")

  # Convert special characters to CQL format
  query<-gsub("=", "%3D", query)
  query<-gsub(" ", "%20", query)
  query <- gsub("/", "%2F", query)

  # Paste query to url
  url<-paste0(baseurl, query)

  return(url)
}

