#' @title Get description
#'
#' @description Retrieve patent description based on epodoc identification code
#'
#' @param epodoc epodoc identification code
#' @param access_token token for authentication
#' @return despcription contained in the patent
#'
#' @examples \dontrun{get_description(epodoc, access_token))}
#'
#'

#' @export description

get_description<-function(epodoc, access_token){
  header <- c(paste('Bearer', access_token), "application/json")
  names(header) <- c('Authorization', "Accept")
  request<-paste("http://ops.epo.org/3.2/rest-services/published-data/publication/epodoc/", epodoc, "/description",
                 collapse = "", sep = "")

  response<-GET(request,
                add_headers(header))

  parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

  description_df<-as.data.frame(parsed_response$`ops:world-patent-data`$`ftxt:fulltext-documents`$`ftxt:fulltext-document`$description)
  description<-description_df$X.[which(description_df$X.lang == "EN")]

  return(description)
}
