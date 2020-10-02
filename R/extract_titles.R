#' @title Extract titles during bibliographic patent data parsing
#'
#' @description
#'
#' @param x title list
#' @return list containing titles
#'
#' @examples \dontrun{extract_titles(x)
#'
#'
#' @export extract_titles

extract_titles<- function(x){
  if(is.data.frame(x) == FALSE){
  title <- as.data.frame(t(as.data.frame(unlist(x))))$`$`

} else {
  title <- x$`$`[which(x$`@lang`== "en")]
}
return(title)
}
