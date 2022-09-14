# Rops

R client to download and parse EPO data using the Open Patent Service

## Authentication process

In order to access the OPS API v3.2, you need to follow these steps:

* register to the OPS (web service)[https://www.epo.org/searching-for-patents/data/web-services/ops.html]
* create a task-specific application and gather the access information (the consumer key and the consumer secret key)
* create a temporary token by passing the consumer key and the consumer secret key to the function `create_access_token`

The token expires each 20 minutes. 

### Example usage

```{r}
library(Rops)

consumer_key <- 'YOURCONSUMERKEY'
consumer_secret_key <- 'YOURCONSUMERSECRETKEY'

access_token <- create_access_token(consumer_key, consumer_secret_key)

```

## get_abstract

Retrieves abstracts of patents from either the docdb or the epodoc document number. At present it works best when multiple patents are queried. The current limit for retrieval is 100 patents

## get_claims

Retrieves claims of a patent given the epodoc id in the form of a character vector. At present it is limited to request the claims for one patent at the time.

## get_description

Retrieves the description of a patent given the epodoc id in the form of a character vector. At present it is limited to request the description for one patent at the time.

## get_biblio

Retrieves bibliographic data of patents in the form of dataframes given the epodoc/docdb id of the patent

## get_query

Builds a query to search the EPO datasets based on various criteria. See documentation for more information

## search_patents

Searches for patents within the EPO dataset. At present it allows to retrieve only the docdb code of the patents matching those criterias. Specifically, I am encountering issues with the parse_search_biblio that should allow to parse the bibliographic information. 



