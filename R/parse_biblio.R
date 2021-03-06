#' @title Parse json data returned from the biblio endpoint into dataframe
#' @description Given a json list after fromJSON() function of the jsonlite package, it returns a dataframe conteining some bibliographic patent data in an orderly manner. Tt works only with results in multiple entries. Not stable, as the OPS is still developing, this method needs to be improved.
#' @param response A single url or vector of urls
#' @return A list.
#' @importFrom httr GET
#' @importFrom httr content_type
#' @importFrom httr accept
#' @importFrom httr content
#' @export parse_biblio
#' @examples \dontrun{parse_biblio(response)}

parse_biblio<- function(response){

  #store family ids
  family_id <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["@family-id"]]

  #store country
  country<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["@country"]]
  #store document number
  doc_number <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["@doc-number"]]
  #store kind
  kind <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["@kind"]]
  #create docdb_id
  docdb_id <- as.list(paste0(country, doc_number, kind))


  #create list containing inventors
  inventor_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.parties.inventors.inventor"]]
  #store inventors in both epodoc and original format
  inventors_epodoc  <- lapply(inventor_list, function (x) x$`inventor-name.name.$`[which(x$`@data-format` == "epodoc")])
  inventors_original  <- lapply(inventor_list, function (x) x$`inventor-name.name.$`[which(x$`@data-format` == "original")])

  #create list containing applicants
  applicant_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.parties.applicants.applicant"]]
  #store applicants in both epodoc and original format
  applicants_epodoc <- lapply(applicant_list, function (x) x$`applicant-name.name.$`[which(x$`@data-format` == "epodoc")])
  applicants_original <- lapply(applicant_list, function (x) x$`applicant-name.name.$`[which(x$`@data-format` == "original")])

  #create list containing publication information
  publication_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.publication-reference.document-id"]]
  #store publication date
  publication_date<- unlist(lapply(publication_list, function(x) unlist(min(x$`date.$`, na.rm = TRUE))))

  pub_doc_epo <- lapply(publication_list, function(x) x$`doc-number.$`[x$`@document-id-type` == "epodoc"])

  #extract epodoc information dataframe
  epodoc_id <- lapply(publication_list, function(x) x$`doc-number.$`[[2]])

  #create list containing application information
  application_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.application-reference.document-id"]]
  #store application date
  application_date<- unlist(lapply(application_list, function(x) min(x$`date.$`, na.rm = TRUE)))
  app_doc_epo <- lapply(application_list, function(x) x$`doc-number.$`[x$`@document-id-type` == "epodoc"])
  app_doc_ori <- lapply(application_list, function(x) x$`doc-number.$`[x$`@document-id-type` == "original"])

  priority_list<- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.priority-claims.priority-claim.document-id"]]
  priority_date <- lapply(priority_list, function(x) x$`date.$`[x$`@document-id-type` == "epodoc"])
  priority_doc <- lapply(priority_list, function(x) x$`doc-number.$`[x$`@document-id-type` == "epodoc"])

  #create list containing titles
  title_list<-parsed_response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.invention-title"]]

  if(is.null(title_list) == FALSE){

    #create temporary function
    extract_titles<- function(x){
      if(is.data.frame(x) == FALSE){
        title <- as.data.frame(t(as.data.frame(unlist(x))))$`$`

      } else {
        title <- x$`$`[which(x$`@lang`== "en")]
      }
      return(title)
    }
    #store english titles
    title_en <- lapply(title_list, function(x) extract_titles(x))
  } else{
    title_en <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.invention-title.$"]]
  }

  #create list containing abstracts
  abstract_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["abstract"]]

  #create temporary function
  extract_abstracts<- function(x){
    if(is.data.frame(x) == FALSE){
      abstract <- as.data.frame(t(as.data.frame(unlist(x))))$`p.$`

    } else {
      abstract <- x$`p.$`[which(x$`@lang`== "en")]
    }
    return(abstract)
  }
  #store english abstracts
  abstract_en <- lapply(abstract_list, function(x) extract_abstracts(x))

  #create list containing IPC classification
  IPC_list<-response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.classifications-ipcr.classification-ipcr"]]
  #create temporary function
  extract_IPC<- function(x){
    if(is.data.frame(x) == FALSE){
      IPC_class <- gsub("\\s+", "", as.data.frame(t(as.data.frame(unlist(x))))$`text.$`)

    } else {
      IPC_class <- gsub("\\s+", "", x$`text.$`)
    }
    return(IPC_class)
  }

  #store IPC classification
  IPC_class <- lapply(IPC_list, function(x) extract_IPC(x))

  CPC_list <- response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["bibliographic-data.patent-classifications.patent-classification"]]
  extract_CPC<- function(dataframe){
    section<- dataframe$`section.$`
    class<- dataframe$`class.$`
    subclass<- dataframe$`subclass.$`
    main_group<- dataframe$`main-group.$`
    subgroup<- dataframe$`subgroup.$`
    CPC_class <- paste0(section, class, subclass, main_group, "/", subgroup)
    return(CPC_class)
  }
  CPC_class<- lapply(CPC_list, function(x) extract_CPC(x))


  biblio_df <- as.data.frame(cbind(docdb_id, epodoc_id, family_id, title_en, abs_en = abstract_en, app_epo = applicants_epodoc,
                                   app_ori = applicants_original, inv_epo = inventors_epodoc, inv_ori = inventors_original,
                                   pub_date = publication_date, pub_doc_epo = pub_doc_epo, app_date = application_date,
                                   app_doc_epo = app_doc_epo, app_doc_ori = app_doc_ori, pri_date = priority_date,
                                   pri_doc_epo = priority_doc, IPC = IPC_class, CPC = CPC_class,
                                   country))



  return(biblio_df)
}
