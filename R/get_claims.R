#' @title Get claims from OPS API
#'
#' @description Retrieve claims of a patent. It works only with the epodoc format
#'
#' @param epodoc_id epodoc code for a patent to have returned the claims
#' @param access_token token for authentications
#' @param type the type of publication to be searched. 'pub' stands for patent publications and 'app' stands for patent applications.
#' @return A character vector
#'
#' @examples \dontrun{get_claims(epodoc_id, type = 'pub', access_token))}
#'
#' @import httr
#'
#' @export get_claims

get_claims<-function(epodoc_id, type = NA, access_token){

  header <- c(paste('Bearer', access_token), "application/json")
  names(header) <- c('Authorization', "Accept")
  baseURL <- "https://ops.epo.org/3.2/rest-services/published-data/"
  if(type == "pub"){
    url <- paste0(baseURL, "publication/epodoc/")
  }

  if(type == "app"){
    url <- paste0(baseURL, "application/epodoc/")
  }

  request<-paste0(url, epodoc_id, "/claims")

  response<-GET(request,
                add_headers(header))

  parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

  claims_df<-as.data.frame(parsed_response$`ops:world-patent-data`$`ftxt:fulltext-documents`$`ftxt:fulltext-document`$claims)
  claims<-claims_df$X.[which(claims_df$X.lang == "EN")]

  claims<-gsub("\\n", " ", claims)

  return(claims)

}
