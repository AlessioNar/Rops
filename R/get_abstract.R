#' @title Get the abstract of a patent given the epodoc or docdb number
#' @description Retrieves up to 100 patent abstracts through the EPODOC or DOCDB identification number
#' @param patent_id string or character vector, identification code of the patent
#' @param type string, indicate whether to retrieve the patent publication or application (pub or app)
#' @param format string, type of the patent id supplied, it can either be epodoc or docdb
#' @param access_token access token generated using the create_access_token function
#' @return dataframe containing patent bibliographic information
#'
#' @examples \dontrun{search_patent(docdb_id, access_token))}
#'
#' @import httr
#' @import jsonlite
#' @export get_abstract
#'

get_abstract<-function(id, type, format, access_token){

  baseURL <- "http://ops.epo.org/3.2/rest-services/published-data/"

  # adds the type of the patent to the url depending on the type parameter 
  if(type == "pub"){
    url <- paste0(baseURL, "publication", "/")
  } else if(type == "app"){
    url <- paste0(baseURL, "application", "/")    
  } else {
    stop("type must be either pub or app")
  }

  # adds the format of the patent id to the url depending on the format parameter
  if(format == "epodoc"){
    url <- paste0(url, "epodoc", '/')
  } else if(format == "docdb"){
    url <- paste0(url, "docdb", '/')
  } else {
    stop("format must be either epodoc or docdb")
  }

  # If the id is a vector, it merges the vector into a string separated by commas and sends a POST request
  # If the id is a string, it sends a GET request

if(is.vector(id) == TRUE){
    # Paste ids together
    merged_id<-paste0(id, collapse = ", ") 
    # Search using the abstract endpoint
    url<-paste0(url, merged_id, "/abstract")
  } else {
    # Search using the abstract endpoint
    url<-paste0(url, id, "/abstract")
}

  # Make request
  response<-get_ops(url, access_token, raw = FALSE)

  # Store the number of documents
  documents <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]]  
  return(documents)
}
