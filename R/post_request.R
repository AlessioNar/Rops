#' @title Makes a POST request to the Open Patent Services API
#' @description Makes a POST request to the Open Patent Services API
#' and it returns the raw or parsed response
#' @param url string, A single url
#' @param access_token string, bearer token for authentication
#' @param raw bool, r
#' @return A list
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom httr status_code
#' @importFrom httr headers
#' @export post_request
#' @examples \dontrun{
#' search_respose <- get_ops(url, access_token, raw = FALSE)
#' }
#'

post_request <- function(url, access_token, raw = raw, id = NULL) {
  
  request_body <- stringr::str_c(paste(id, collapse = ","))

  headers <- c(paste("Bearer", access_token), "application/json")
  # Rename header
  names(headers) <- c("Authorization", "Accept")

  # Make the POST request
  response <- httr::POST(url,
                      body = request_body,
                      encode = "raw",
                      content_type("text/plain"),
                      add_headers(headers)
                      )

  # Check if the response is 200
  if (httr::status_code(response) != 200) {
    # Stop the function if the response is not 200 and
    # return the error code and headers
    stop(
      paste0(httr::status_code(response),
      "/n",
      httr::headers(response)))
  } else {
    # Convert the json file into a list
    if (raw == TRUE) {
      return(httr::content(response, as = "text", encoding = "UTF-8"))
    } else {
      return(fromJSON(httr::content(response, "text"), flatten = TRUE))
    }
  }
}