#' @title Extract abstracts during bibliographic patent data parsing
#'
#' @description
#'
#' @param x abstract list
#' @return list containing abstracts
#'
#' @examples \dontrun{extract_abstracts(x)
#'
#'
#' @export extract_abstracts
#'
extract_abstracts<- function(x){
  if(is.data.frame(x) == FALSE){
    abstract <- as.data.frame(t(as.data.frame(unlist(x))))$`p.$`

  } else {
    abstract <- x$`p.$`[which(x$`@lang`== "en")]
  }
  return(abstract)
}
