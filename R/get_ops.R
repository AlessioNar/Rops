#' @title Obtain a set of urls from ops_publications
#' @description Used in conjunction with ops_iterate. Given a set of urls convert to a
#' get request to retrieve the data from OPS. When the response is received extract the content to create a list.
#' @param url string, A single url
#' @param access_token string, bearer token for authentication
#' @param raw bool, r
#' @param from_range int Initial limit of results to be returned. Default to 1
#' @param to_range Final limit of results to be returned. Default to 100
#' @return A list
#' @importFrom httr GET
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @importFrom httr header
#' @export
#' @examples \dontrun{
#' search_respose <- get_ops("https://ops.epo.org/3.2/rest-services/published-data/search/?q=ct%3DCN104980769%20and%20pd%3D%2220180201%2020180203%22", access_token, raw = FALSE)
#' }
#' 

get_ops <- function(url, access_token, raw = NULL) {
  
  # Create header
  header <- c(paste('Bearer', access_token), "application/json")
  # Rename header
  names(header) <- c('Authorization', "Accept")
  # Make request
  response <- httr::GET(url = url, httr::add_headers(header))
  
  # Check if the response is 200
  if (httr::status_code(response) != 200) {
    # Stop the function if the response is not 200 and return the error code and headers
    stop(paste0(httr::status_code(response), "/n", as.character(httr::headers(response))))
  } else {
    # Convert the json file into a list
    if (raw == TRUE) {
      return(httr::content(response, as = "text", encoding = "UTF-8"))
    } else {
      return(fromJSON(httr::content(response, "text"), flatten = TRUE))
    }
  }
}