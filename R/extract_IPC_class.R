#' @title Extract IPC classification during patent data parsing
#'
#' @description
#'
#' @param x IPC location
#' @return vector containing IPC classification of various patents
#'
#' @examples \dontrun{extract_IPC_class(x)
#'
#'
#' @export extract_IPC_class
#'
#'
extract_IPC_class<- function(x){
  if(is.data.frame(x) == FALSE){
    IPC_class <- gsub("\\s+", "", as.data.frame(t(as.data.frame(unlist(x))))$`text.$`)

  } else {
    IPC_class <- gsub("\\s+", "", x$`text.$`)
  }
  return(IPC_class)
}



