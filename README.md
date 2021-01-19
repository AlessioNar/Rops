# Rops

R client to download and parse EPO data using the Open Patent Service

## Authentication process

Accessing the OPS API requires applying for an account to obtain the consumer key and the consumer secret key. In order to register for the service, register at

https://www.epo.org/searching-for-patents/data/web-services/ops.html.

Then use the create_access_token function to retrieve an access token that is valid for 20 minutes and use it to make calls to the OPS API.

## get_abstract

Retrieves abstracts of patents from either the docdb or the epodoc document number. At present it works best when multiple patents are queried. The current limit for retrieval is 100 patents

## get_claims

Retrieves claims of a patent given the epodoc id in the form of 

- get_biblio: retrieves bibliographic data of patents in the form of dataframes given the epodoc/docdb id of the patent

- get_description: retrieves description of a patent given the epodoc id
- get_query: builds a query to search the EPO datasets based on various criteria. See documentation for more information
- search_patents: searches for patents within the EPO dataset. At present it allows to retrieve only the docdb code of the patents matching those criterias. Specifically, I am encountering issues with the parse_search_biblio that should allow to parse the bibliographic infromation. 



