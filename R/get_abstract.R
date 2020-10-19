#' @title Get patent abstract
#'
#' @description Retrieves up to 100 patent abstracts through the epodoc or docdb id
#'
#' @param patent_id identification code of the patent. It can either be in the epodoc or docdb format
#' @param type publication or application (pub or app)
#' @param format id type of the patent, it must be either epodoc or docdb
#' @param access_token access token generated using the create_access_token function
#' @return patent bibliographic information in json format
#'
#' @examples \dontrun{search_patent(docdb_id, access_token))}
#'
#'
#' @import httr
#' @import jsonlite
#' @export get_biblio
#'
#'

get_biblio<-function(id, type, format, access_token, fromRange = 1, toRange = 100){

  baseURL <- "http://ops.epo.org/3.2/rest-services/published-data/"
  if(type == "pub"){
    url <- paste0(baseURL, "publication/")
  }
  if(type == "app"){
    url <- paste0(baseURL, "application/")
  }
  if(format == "docdb"){
    url <- paste0(url, "docdb/")
  }
  if(format == "epodoc"){
    url <- paste0(url, "epodoc/")
  }
  if(is.vector(id) == TRUE){
  merged_id<-paste0(id, collapse = ", ")
  multiple<-TRUE
  url<-paste0(url, merged_id, "/abstract")
} else {
  multiple<-FALSE
  url<-paste0(url, id, "/abstract")
}

# Request
response<-get_ops(url, access_token, raw = TRUE, from_range = fromRange, to_range = toRange)

if(response$status_code == 200){

parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

#patent_biblio<-parse_biblio(parsed_response)

}
else{
  print(paste("Failed request, error", response$status_code))
}

if(is.null(patent_biblio) == FALSE){

return(abstract)
} else {
print("Request failed")
}

}
