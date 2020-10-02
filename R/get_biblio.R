#' @title Get patent bibliographic data
#'
#' @description search for a patent
#'
<<<<<<< HEAD
#' @param patent_id identification code of the patent. It can either be in the epodoc or docdb format
#' @param id_type id type of the patent, it must be either epodoc or docdb
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

get_biblio<-function(id, type, format, access_token, fromRange, toRange){

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
  url<-paste0(url, merged_id, "/biblio")
} else {
  multiple<-FALSE
  url<-paste0(url, id, "/biblio")
}

# Request
response<-get_ops(url, access_token, raw = TRUE, from_range = 1, to_range = 2)

if(response$status_code == 200){

parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

patent_biblio<-parse_biblio(parsed_response)

}
else{
  print(paste("Failed request, error", response$status_code))
}

if(is.null(patent_biblio) == FALSE){

return(patent_biblio)
} else {
print("Request failed")
}

}

