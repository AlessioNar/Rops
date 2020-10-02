#' @title Get patent bibliographic data
#'
#' @description search for a patent
#'
#' @param docdb_id docdb identification code of the patent
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
get_biblio<-function(docdb_id, access_token, fromRange, toRange){
if(is.vector(docdb_id) == TRUE){
  merged_docdb_id<-paste0(docdb_id, collapse = ", ")
  multiple<-TRUE
  url<-paste0("http://ops.epo.org/3.2/rest-services/published-data/publication/docdb/", merged_docdb_id, "/biblio")
} else {
  multiple<-FALSE
  url<-paste0("http://ops.epo.org/3.2/rest-services/published-data/publication/docdb/", docdb_id, "/biblio")
}
# create bearer token to associate to the POST/GET request

# Request
response<-get_ops(url, access_token, raw = TRUE, from_range = fromRange, to_range = toRange)

if(response$status_code == 200){
parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

patent_biblio<-parse_patents(parsed_response, docdb_id)

}
else{
  print(paste("Failed request, error", response$status_code))
}

if(is.null(patent_biblio) == FALSE){
  patent
return(patent_biblio)
}

}

