#' @title Parse json data returned from the biblio endpoint into dataframe
#' @description Given a json list after fromJSON() function of the jsonlite package, it returns a dataframe conteining some bibliographic patent data in an orderly manner
#' it works only with results in multiple entries. Not stable, as the OPS is still developing, this method needs to be improved.
#' @param response . A single url or vector of urls
#' @param access_token bearer token for authentication
#' @return A list.
#' @importFrom httr GET
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @export
#' @examples \dontrun{lapply(three_urls, ops_get)}

parse_patents<- function(response, docdb_id){

  country<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["@country"]]

  inventors_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.parties.inventors.inventor"]]
  inventors  <- lapply(inventors_info, function (x) x$`inventor-name.name.$`[which(x$`@data-format` == "epodoc")])

  applicants_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.parties.applicants.applicant"]]
  applicants_epodoc <- lapply(applicants_info, function (x) x$`applicant-name.name.$`[which(x$`@data-format` == "epodoc")])
  applicants_original <- lapply(applicants_info, function (x) x$`applicant-name.name.$`[which(x$`@data-format` == "original")])


  publication_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.publication-reference.document-id"]]
  publication_date<- unlist(lapply(publication_info, function(x) unlist(min(x$`date.$`, na.rm = TRUE))))
  epodoc_id <- unlist(lapply(publication_info, function (x) x$`doc-number.$`[which(x$`@document-id-type` == "epodoc")]))

  application_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.application-reference.document-id"]]
  application_date<- unlist(lapply(application_info, function(x) min(x$`date.$`, na.rm = TRUE)))

  #IPC class, titles, and abstracts are in a strange format (some are stored as lists and some as dataframes)

  titles_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.invention-title"]]
  titles <- lapply(titles_info, function(x) extract_titles(x))


  abstracts_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["abstract"]]
  abstracts <- lapply(abstracts_info, function(x) extract_abstract(x))

  #priority_date_info<-some_patents[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.priority-claims.priority-claim"]]
  #lapply(priority_date_info, extract_priority_date(x))
  #idea: some of the contents are stored as dataframes and others as simple lists. I should
  #put an if condition into a lapply to evaluate when it is more appropriate to
  #is.data.frame(priority_date[[1]])

  IPC_class_info<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.classifications-ipcr.classification-ipcr"]]
  IPC_class <- lapply(IPC_class_info, function(x) extract_IPC_class(x))

  biblio_df <- as.data.frame(cbind(docdb_id = unlist(docdb_id), epodoc_id, titles, abstracts, country, inventors, applicants_epodoc, applicants_original, publication_date, application_date, IPC_class))

  return(biblio_df)
}
