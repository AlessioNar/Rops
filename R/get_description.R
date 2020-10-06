#' @title Get patent description
#'
#' @description Retrieve patent description based on epodoc identification code
#'
#' @param epodoc_id epodoc identification code
#' @param access_token token for authentication
#' @param type the type of publication to be searched. 'pub' stands for patent publications and 'app' stands for patent applications.
#' @return Character vector containing patent description
#'
#' @examples \dontrun{get_description(epodoc_id, type = 'pub', access_token)}
#'
#'

#' @export get_description

get_description<-function(epodoc_id, type, access_token){
  header <- c(paste('Bearer', access_token), "application/json")
  names(header) <- c('Authorization', "Accept")
  baseURL <- "https://ops.epo.org/3.2/rest-services/published-data/"

  if(type == "pub"){
    url <- paste0(baseURL, "publication/epodoc/")
  }

  if(type == "app"){
    url <- paste0(baseURL, "application/epodoc/")
  }

  request<-paste0(url, epodoc_id, "/description")

  response<-GET(request,
                add_headers(header))

  parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

  description_df<-as.data.frame(parsed_response$`ops:world-patent-data`$`ftxt:fulltext-documents`$`ftxt:fulltext-document`$description)
  description<-description_df$X.[which(description_df$X.lang == "EN")]

  return(description)
}
