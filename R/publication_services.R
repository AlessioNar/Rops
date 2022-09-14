#' @title Queries the Publication Services Open Patent Services API
#'
#' @description Retrieve claims of a patent. It works only with the epodoc format
#'
#' @param epodoc_id character, epodoc code for a patent to have returned the claims
#' @param access_token token for authentications
#' @param type the type of publication to be searched. 'pub' stands for patent publications and 'app' stands for patent applications.
#' @return A character vector containing patent claims
#'
#' @examples \dontrun{get_full_text(epodoc_id, type = 'pub', access_token))}
#' @import httr
#' @export publication_services

publication_services<-function(id, type, format, access_token, what = "fulltext", raw = FALSE){

    baseURL <- "http://ops.epo.org/3.2/rest-services/published-data"

    if(is.vector(id) == TRUE){
            # Paste ids together
            id<-paste0(id, collapse = ", ")
    }

    
    url <- stringr::str_c(baseURL, "/", type, "/", format, "/", id, "/", what)
    
    
    print(url)
    # If clause to check if id is a vector or not, if it is, use post_request if it is not, use get_request
    if(is.vector(id) == TRUE){
        response <- get_ops(url, access_token, raw = raw, multiple = TRUE, id = id)
    } else {
        response <- get_ops(url, access_token, raw = raw)
    }




    # Make request
    response<-get_ops(url, access_token, raw = raw)
    # return the response
    return(response)

}
