#' @title Get equivalents
#'
#' @description Get patent equivalents
#'
#' @param id identification code of the patent.
#' @param type type of document to be retrieved. It can either be a publication or an application (pub or app)
#' @param format format type of the patent id, it must be either epodoc or docdb
#' @param access_token access token generated using the create_access_token function
#' @return Family members of the selected patent
#' @examples \dontrun{get_equivalents(id, type = "pub", format = "epodoc ", access_token)}
#' @export get_equivalents
#'
#'

get_equivalents<- function(id, type, format, access_token){

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
  if(is.null(id) == FALSE){
    url <- paste0(url, id, "/equivalents")
  }
  response<- get_ops(url, access_token, raw = FALSE)

  equivalents_list<- lapply(response$`ops:world-patent-data`$`ops:equivalents-inquiry`$`ops:inquiry-result`, as.data.frame)
  equivalents<- do.call(rbind, equivalents_list)
  names(equivalents) <- c("doc_type", "doc_id")

  return(equivalents)
}



