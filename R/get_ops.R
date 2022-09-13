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
#' @export
#' @examples \dontrun{
#' search_respose <- get_ops("https://ops.epo.org/3.2/rest-services/published-data/search/?q=ct%3DCN104980769%20and%20pd%3D%2220180201%2020180203%22", access_token, raw = FALSE)
#' }


get_ops <- function(url, access_token, raw = NULL, from_range = 1, to_range = 100) {
  # Create header for request
  header <- httr::add_headers(
    Authorization = paste("Bearer", access_token),
    Accept = "application/json",
    "X-OPS-Range" = paste("items=", from_range, "-", to_range)
  )
  # make request
  response <- httr::GET(url = url, add_headers(header))
  # check response
  if (httr::status_code(response) == 200) {
    # extract content
    content <- httr::content(response, as = "text")
    # check if raw is true
    if (raw) {
      return(content)
    } else {
      # parse content
      parsed_content <- jsonlite::fromJSON(content)
      return(parsed_content)
    }
  } else {
    # return error
    return(httr::status_code(response))
  }
}