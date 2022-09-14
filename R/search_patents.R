#' @title Search patents
#' @description Search for patents contained in the EPO dataset
#' @param query url built using the function get_query
#' @param access_token access token received after authentication
#' @return Dataframe containing patents
#' @export search_patents
#' @examples \dontrun{search_patents(query, access_token)}

search_patents <- function(title = NA, abstract = NA, titab = NA, applicant = NA,
                           inventor = NA, citation = NA, pub_num = NA, cpc = NA, ipc = NA,
                           from = NA, to = NA, with_biblio = FALSE, access_token,
                           fromRange = 1, toRange = 100){

  # Create query
  url <- get_query(title = title, abstract = abstract,
                 titab = titab, applicant = applicant, inventor = inventor,
                 pub_num = pub_num, cpc = cpc, ipc = ipc, from = from,
                 to = to, with_biblio = with_biblio)

  # Make request
  search_results<- get_ops(url, access_token, raw = FALSE, fromRange = fromRange, toRange = toRange)

  if (with_biblio == FALSE){
  # Parse results
  dataframe<- parse_search(search_results)
  } else{
    # the function parse_search_biblio is unstable, I need to check it
    dataframe <- parse_search_biblio(search_results)
  }

  return(dataframe)
}
