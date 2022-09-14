#' @title Makes a get request to the Open Patent Services API
#' @description Makes a request to the Open Patent Services API
#' and it returns the raw or parsed response
#' @param url string, A single url
#' @param access_token string, bearer token for authentication
#' @param raw bool, r
#' @return A list
#' @importFrom httr GET
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom httr status_code
#' @importFrom httr headers
#' @export get_request
#' @examples \dontrun{
#' response <- get_request(url, access_token, raw = FALSE)
#' }
#'

get_request <- function(url, access_token, raw = FALSE) {

  # Create header
  headers <- c(paste("Bearer", access_token), "application/json")
  # Rename header
  names(headers) <- c("Authorization", "Accept")

  # Make a GET request
  response <- httr::GET(url = url, httr::add_headers(headers))

  # Check if the response is 200
  if (httr::status_code(response) != 200) {
    # Stop the function if the response is not 200 and
    # return the error code and headers
    stop(
      paste0(httr::status_code(response),
      "/n",
      as.character(httr::headers(response))))
  } else {
    # Check if user wants raw response
    # if not, parse the response into a list and return it
    if (raw == TRUE) {
      return(httr::content(response, as = "text", encoding = "UTF-8"))
    } else {
      return(fromJSON(httr::content(response, "text"), flatten = TRUE))
    }
  }
}