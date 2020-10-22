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

  # Create header and name it
  header <- c(paste('Bearer', access_token), "application/json")
  names(header) <- c('Authorization', "Accept")

  baseURL <- "https://ops.epo.org/3.2/rest-services/published-data/"

  if(type == "pub"){
    # Search among publications
    url <- paste0(baseURL, "publication/epodoc/")
  }

  if(type == "app"){
    # Search among applications
    url <- paste0(baseURL, "application/epodoc/")
  }

  # Access through the description endpoint
  request<-paste0(url, epodoc_id, "/description")

  # Make request
  response<-GET(request,
                add_headers(header))

  # Parse json file into list
  parsed_response<-fromJSON(content(response, "text"), flatten = TRUE)

  # Create df containing description and language
  description_df<-as.data.frame(parsed_response$`ops:world-patent-data`$`ftxt:fulltext-documents`$`ftxt:fulltext-document`$description)

  # Select only description in English language
  description<-description_df$X.[which(description_df$X.lang == "EN")]

  return(description)
}
