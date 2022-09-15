#' @title Queries the Publication Services Open Patent Services API
#'
#' @description Retrieve claims of a patent.
#' It works only with the epodoc format
#'
#' @param id string or string vector, epodoc or docdb format
#' @param type the type of publication to be searched.
#' 'publication' or 'application'.
#' @param format the format of the document id to be searched.
#' 'epodoc' or 'docdb'.
#' @param access_token token for authentications
#' @param raw bool, if TRUE returns the raw response
#' @param what string, the type of information to be retrieved.
#' 'claims', 'abstract', 'description', 'title', 'fulltext', 'full-cycle'
#' @return A list or a raw string
#' @examples \dontrun{publication_services(id = "EP1000000.A1", type = "publication", format = "epodoc", access_token = "your_access_token", raw = FALSE, what = "claims")}
#' @examples \dontrun{publication_services(id = c("EP1000000.A1", "US2022179620.A1"), type = "publication", format = "epodoc", access_token = "your_access_token", raw = FALSE, what = "claims")}
#' @importFrom stringr str_c
#' @export publication_services

publication_services <- function(id, type, format, access_token, what = "fulltext", raw = FALSE) {

    base_url <- "https://ops.epo.org/3.2/rest-services/published-data"

    # If clause to check if the user supplied a string or a string array
    # if it is, use post_request if it is not, use get_request
    if (length(id) == 1) {
        url <- stringr::str_c(base_url, "/", type, "/", format, "/", id, "/", what)
        response <- get_request(url, access_token, raw = raw)
    } else {
        url <- stringr::str_c(base_url, "/", type, "/", format, "/", what)
        response <- post_request(url, access_token, raw = raw, id = id)
    }

    # return the response
    return(response)

}
