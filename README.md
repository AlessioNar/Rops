# Rops
R client to download EPO OPS data. The package is still in development and I am open to collaborations to improve its functionalities. 

At present it presents four basic functions:

- get_biblio: retrieves bibliographic data of patents in the form of dataframes given the epodoc/docdb id of the patent
- get_query: builds a query to search the EPO datasets based on various criteria. See documentation for more information
- get_claims: retrieves claims of a patent given the epodoc id
- get_description: retrieves description of a patent given the epodoc id

## Authentication process
Accessing the OPS API requires applying for an account to obtain the consumer key and the consumer secret key. In order to register for the service, register at
https://www.epo.org/searching-for-patents/data/web-services/ops.html.

Then use the create_access_token function to retrieve an access token that is valid for 20 minutes. 

## Retrieve bibliographic information




