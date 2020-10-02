#' @title Obtain a set of urls from ops_publications
#' @description Used in conjunction with ops_iterate. Given a set of urls convert to a
#' get request to retrieve the data from OPS. When the response is received extract the content to create a list.
#' @param url . A single url
#' @param access_token bearer token for authentication
#' @param raw boolean value to
#' @return A list containing a parse
#' @importFrom httr GET
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @export
#' @examples \dontrun{search_respose <- get_ops("https://ops.epo.org/3.2/rest-services/published-data/search/?q=ct%3D CN104980769 %20 and %20 pd %3D %22 20180201%2020180203%22", access_token, raw = FALSE)}
get_ops <- function(url, access_token, raw = NULL, from_range = NULL, to_range = NULL){
  # Paste token to bearer. In this case it is possible to improve this function to decide the response,
  # either in xml or in json format.
  if(is.null(from_range) == TRUE | is.null(to_range) == TRUE ){
    header <- c(paste('Bearer', access_token), "application/json")
    # Rename header
    names(header) <- c('Authorization', "Accept")
  } else {
  header <- c(paste('Bearer', access_token), "application/json", paste0(from_range, "-", to_range))
  # Rename header
  names(header) <- c('Authorization', "Accept", "X-OPS-Range")
  }
  #make request
  response <- httr::GET(url = url, add_headers(header))

  if(raw == TRUE){
    return(response)
  }
  if(raw == FALSE){
    #store value as json
    content <- httr::content(response) #required or raw return

  return(content)
  }
  }

