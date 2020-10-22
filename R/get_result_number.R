#' @title Retrieve number of result matching a query created using get_query
#' @description This is a memory efficient way to count the number of results of a single query. Whenever it returns 10000, it means that the number of results are 10000 or higher
#' @param query url built using the function get_query
#' @param access_token access token received after authentication
#' @return Number of results
#' @export get_result_number
#' @examples \dontrun{get_result_number(query, access_token)}
#'

get_result_number<- function(query, access_token){

  # Create header creating a range of 1
  header <- c(paste('Bearer', access_token), "application/json", paste0(1, "-", 1))

  # Rename header
  names(header) <- c('Authorization', "Accept", "X-OPS-Range")

  # Make request
  response <- httr::GET(url = query, add_headers(header))

  # Extract content
  content <- content(response)

  # Retrieve result number
  result_number <- content[["ops:world-patent-data"]][["ops:biblio-search"]][["@total-result-count"]]

  return(result_number)
}

