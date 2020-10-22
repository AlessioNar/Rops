#' @title Get patent abstract
#'
#' @description Retrieves up to 100 patent abstracts through the epodoc or docdb id
#'
#' @param patent_id identification code of the patent. It can either be in the epodoc or docdb format
#' @param type publication or application (pub or app)
#' @param format id type of the patent, it must be either epodoc or docdb
#' @param access_token access token generated using the create_access_token function
#' @return patent bibliographic information as dataframe
#'
#' @examples \dontrun{search_patent(docdb_id, access_token))}
#'
#'
#' @import httr
#' @import jsonlite
#' @export get_abstract
#'
#'

get_abstract<-function(id, type, format, access_token, fromRange = 1, toRange = 100){

  baseURL <- "http://ops.epo.org/3.2/rest-services/published-data/"

  if(type == "pub"){
    # Search among publications
    url <- paste0(baseURL, "publication/")
  }
  if(type == "app"){
    # Search among applications
    url <- paste0(baseURL, "application/")
  }
  if(format == "docdb"){
    url <- paste0(url, "docdb/")
  }
  if(format == "epodoc"){
    url <- paste0(url, "epodoc/")
  }

  # Check the number of items to retrieve
  if(is.vector(id) == TRUE){

  # Paste ids together
  merged_id<-paste0(id, collapse = ", ")
  # Set multiple variable to be passed to the parsing function as true
  multiple<-TRUE
  # Search using the abstract endpoint
  url<-paste0(url, merged_id, "/abstract")
} else {
  # Set multiple variable to be passed to the parsing function as false
  multiple<-FALSE
  # Search using the abstract endpoint
  url<-paste0(url, id, "/abstract")
}

# Make request
response<-get_ops(url, access_token, raw = TRUE, from_range = fromRange, to_range = toRange)


if(response$status_code == 200){

  # Convert the json file into a list
  parsed_response<-fromJSON(httr::content(response, "text"), flatten = TRUE)

  # Store the number of documents
  documents <- parsed_response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]]

  #store country
  country <- documents$`@country`

  #store document number
  doc_number <- documents$`@doc-number`

  #store kind
  kind <- documents$`@kind`

  #create docdb_id
  docdb_id <- paste0(country, doc_number, kind)

  # Store abstract list
  abstract_list<-parsed_response[["ops:world-patent-data"]][["exchange-documents"]][["exchange-document"]][["abstract"]]

  #create temporary function to extract abstracts
  extract_abstracts<- function(x){

      # if the item is not a dataframe, then transform into one and extract the abstract
      if(is.data.frame(x) == FALSE){

        abstract <- as.data.frame(t(as.data.frame(unlist(x))))$`p.$`

      } else {

        # Store abstracts in english language
        abstract <- x$`p.$`[which(x$`@lang`== "en")]
      }

  return(abstract)
  }

  # Loop through the list and extract english abstracts
  abstract_en <- lapply(abstract_list, function(x) extract_abstracts(x))

  # Set the null items in the list as NA
  abstract_en[sapply(abstract_en, is.null)] <- NA

  # Create dataframe to be returned
  abstracts <- data.frame(docdb_id = docdb_id, abstract = unlist(abstract_en))

  } else {
    # Print error number
    print(paste("Failed request, error", response$status_code))
  }

if(is.null(abstracts) == FALSE){

  return(abstracts)

  } else {
    print("Request failed")
  }

}

