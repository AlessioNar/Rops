#' @title Get family members
#'
#' @description Get family members of the patent
#'
#' @param id identification code of the patent.
#' @param type type of document to be retrieved. It can either be a publication or an application (pub or app)
#' @param format format type of the patent id, it must be either epodoc or docdb
#' @param access_token access token generated using the create_access_token function
#' @return Family members of the selected patent
#'
#' @examples \dontrun{get_family(id, type = "pub", format = "epodoc ", access_token)}
#' @import httr
#' @import jsonlite
#' @export get_family
#'
get_family<- function(id, type, format, access_token){

  baseURL <- "http://ops.epo.org/3.2/rest-services/family/"

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
  if(is.null(id) == FALSE){
    url <- paste0(url, id)
  }
  response<- get_ops(url, access_token, raw = FALSE)
  documents<- response$`ops:world-patent-data`$`ops:patent-family`$`ops:family-member`
  document_id_list <- lapply(documents, function(x) x[["publication-reference"]][["document-id"]])
  epodoc_id <- lapply(document_id_list, function(x) unlist(x[[2]]$`doc-number`))
  docdb_id <- lapply(document_id_list, function(x) unlist(paste0(x[[1]]$`country`, x[[1]]$`doc-number`, x[[1]]$`kind`)))

  return(response)
}



