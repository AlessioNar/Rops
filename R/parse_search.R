#' @title Parse json response into docdb codes of patents
#'
#' @description Parse json response into docdb codes of patents
#'
#' @param response access token generated using the create_access_token function
#' @return character vector containing docdb codes of query response
#' @examples \dontrun{parse_search(response)}
#' @export parse_search
#'


parse_search <- function(response){

        results<- response$`ops:world-patent-data`$`ops:biblio-search`$`ops:search-result`$`ops:publication-reference`
        if(response$`ops:world-patent-data`$`ops:biblio-search`$`@total-result-count` == 1){

                country <- results$`document-id`$country$`$`

                doc_number <- results$`document-id`$`doc-number`$`$`

                kind <- results$`document-id`$`kind`$`$`

                docdb_id<- paste0(country, doc_number, kind)
        } else {
        country <- lapply(results, function(x) x$`document-id`$country$`$`)

        doc_number <- lapply(results, function(x) x$`document-id`$`doc-number`$`$`)

        kind <- lapply(results, function(x) x$`document-id`$`kind`$`$`)

        docdb_id<- paste0(country, doc_number, kind)
}
        return(docdb_id)
}
