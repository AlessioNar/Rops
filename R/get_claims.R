#' @title Get claims
#'
#' @description Retrieve claims of a patent. It works only with the epodoc format
#'
#' @param epodoc epodoc code for a patent to have returned the claims
#' @param access_token token for authentications
#' @return claims contained in the patent as a list
#'
#' @examples \dontrun{get_claims(epodoc, access_token))}
#'
#'

#' @export claims

get_claims<-function(epodoc, access_token){

  header <- c(paste('Bearer', access_token), "application/json")
  names(header) <- c('Authorization', "Accept")
  request<-paste("http://ops.epo.org/3.2/rest-services/published-data/publication/epodoc/", epodoc, "/claims",
                 collapse = "", sep = "")

  response<-GET(request,
                add_headers(header))

  parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

  claims_df<-as.data.frame(parsed_response$`ops:world-patent-data`$`ftxt:fulltext-documents`$`ftxt:fulltext-document`$claims)
  claims<-claims_df$X.[which(claims_df$X.lang == "EN")]

  claims<-gsub("\\n", " ", claims)

  return(claims)

}
