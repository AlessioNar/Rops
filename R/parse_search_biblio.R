#' @title Obtain a set of urls from ops_publications
#' @description Used in conjunction with ops_iterate. Given a set of urls convert to a
#' get request to retrieve the data from OPS. When the response is received extract the content to create a list.
#' @param url . A single url or vector of urls
#' @param response json list returned from get_ops function
#' @return A list.
#' @export parse_search
#' @examples \dontrun{lapply(three_urls, ops_get)}
#'
parse_search_biblio <- function(response){

  tot_results<- response[["ops:world-patent-data"]][["ops:biblio-search"]][["@total-result-count"]]


  #select different documents
  patent_list<- response[["ops:world-patent-data"]][["ops:biblio-search"]][["ops:search-result"]][["exchange-documents"]]

  empty_to_na<- function(value){

    if(is_empty(value) == TRUE){
      value <- NA
    }
    return(value)
  }

         documents <- lapply(patent_list, function(x) x[["exchange-document"]])

         #store family_id
         family_id <- lapply(documents, function(x) x$`@family-id`)
         #store country
         country <- lapply(documents, function(x) x$`@country`)
         #store document number
         doc_number <- lapply(documents, function(x) x$`@doc-number`)
         #store kind
         kind <- lapply(documents, function(x) x$`@kind`)
         #create docdb_id
         docdb_id <- as.list(paste0(country, doc_number, kind))

         #store list containing bibliographic data
         bibliographic_data <- lapply(documents, function(x) x[["bibliographic-data"]])

         #create dataframe containing title data
         title_df <- lapply(bibliographic_data, function(x) do.call(rbind, lapply(x$`invention-title`, function(x) as.data.frame(x))))

         #some publications contain the title in only one language and it is presented at a lower layer

         extract_titles<- function(title_df){

           if(ncol(title_df) == 1){
           title_df <- as.data.frame(t(title_df))
           names(title_df) <- c("X.", "X.lang")
         }
          return(title_df)
         }

         #apply extract_titles function
         title_df <- lapply(title_df, function(x) extract_titles(x))

         #store english title
         title_en <- lapply(title_df, function(x) x$X.[which(x$X.lang == "en")])
         title_en <- lapply(title_en, function(x) empty_to_na(x))

         #store original language title
         title_ol <- lapply(title_df, function(x) x$X.[which(x$X.lang == "ol")])
         title_ol <- lapply(title_ol, function(x) empty_to_na(x))

         #create dataframe containing applicants data
         applicant_list<- lapply(bibliographic_data, function(x) x$parties$applicants$applicant)

         extract_person <- function(person){
           if(ncol(as.data.frame(person)) == 3){
             person_df <- as.data.frame(person)
           } else {
             person_df<- do.call(rbind, lapply(person, function(x) as.data.frame(x)))
           }
           return(person_df)
         }

         applicant_df <- lapply(applicant_list, function(x) extract_person(x))

         #store applicant name in epodoc format
         applicant_epodoc <- lapply(applicant_df, function(x) c(x$X.[which(x$X.data.format == "epodoc")]))
         applicant_epodoc <- lapply(applicant_epodoc, function(x) empty_to_na(x))

         #store applicant name in original format
         applicant_original <- lapply(applicant_df, function(x) c(x$X.[which(x$X.data.format == "original")]))
         applicant_original <- lapply(applicant_original, function(x) empty_to_na(x))

         #create lists containing inventors data
         inventor_list<- lapply(bibliographic_data, function(x) x$parties$inventors$inventor)

         inventor_df <- lapply(inventor_list, function(x) extract_person(x))

         #store inventor name in epodoc format.
         inventor_epodoc <- lapply(inventor_df, function(x) c(x$X.[which(x$X.data.format == "epodoc")]))
         inventor_epodoc <- lapply(inventor_epodoc, function(x) empty_to_na(x))

         #store inventor name in original format
         inventor_original <- lapply(inventor_df, function(x) c(x$X.[which(x$X.data.format == "original")]))
         inventor_original <- lapply(inventor_original, function(x) empty_to_na(x))


         #store publication information in a list
         publication_list <- lapply(bibliographic_data, function(x) x$`publication-reference`$`document-id`)
         #retrieve publication date and store as a list
         publication_date <- lapply(publication_list, function(x) min(unlist(lapply(x, function(y) unlist(y$`date`, recursive = FALSE)))))

         #store application information in a list
         application_list <- lapply(bibliographic_data, function(x) x$`application-reference`$`document-id`)
         #retrieve application date and store as a list
         application_date <- lapply(application_list, function(x) min(unlist(lapply(x, function(y) unlist(y$`date`, recursive = FALSE)))))

         #extract epodoc information dataframe
         epodoc_df <- lapply(publication_list, function(x) as.data.frame(x[[2]]))
         #store epodoc number
         epodoc_id <- lapply(epodoc_df, function(x) x$X.)


         #retrieve lists containing abstract
         abstract_list <- lapply(documents, function(x) x$abstract)

         #create function to retrieve abstract in a stardardized df format
         extract_abstracts<- function(abstract_list){
           #if clause to distinguish between two typologies of abstracts
         if(ncol(as.data.frame(abstract_list)) == 2){
          abstract_df<- as.data.frame(abstract_list)
          names(abstract_df) <- c("X.lang", "X.")
         } else {
           abstract_df <- do.call(rbind, lapply(abstract_list, function(x) as.data.frame(x)))
         }
         return(abstract_df)
         }

         #apply function extract_abstracts
         abstract_df <- lapply(abstract_list, function(x) extract_abstracts(x))
         #retrieve english abstracts
         abstract_en <- lapply(abstract_df, function(x) x$X.[which(x$X.lang == "en")])
         abstract_en <- lapply(abstract_en, function(x) empty_to_na(x))

         #retrieve original language abstracts
         abstract_ol <- lapply(abstract_df, function(x) x$X.[which(x$X.lang == "ol")])
         abstract_ol <- lapply(abstract_ol, function(x) empty_to_na(x))

         IPC_list <- lapply(bibliographic_data, function(x) x$`classifications-ipcr`$`classification-ipcr`)

         extract_IPC<- function(IPC_list){
           if(ncol(as.data.frame(IPC_list)) == 2){
             IPC_df<- as.data.frame(IPC_list)
             names(IPC_df) <- c("X.sequence", "X.")
           } else{
             IPC_df <- do.call(rbind, lapply(IPC_list, function(x) as.data.frame(x)))
           }

           IPC_df$X. <- gsub("\\s+", "", IPC_df$X.)
           return(IPC_df)
         }


         IPC_class <- lapply(IPC_list, function(x) extract_IPC(x))

         IPC_class<- lapply(IPC_class, function(x) empty_to_na(x$X.))

  patent_info <- as.data.frame(cbind(docdb_id, epodoc_id, family_id, title_en, abs_en = abstract_en,
                            title_ol = title_ol, abs_ol = abstract_ol,
                            app_epo = applicant_epodoc, app_ori = applicant_original,
                            inv_epo = inventor_epodoc, inv_ori = inventor_original,
                            pub_date = publication_date, app_date = application_date, IPC = IPC_class, country))



  return(patent_info)
}
