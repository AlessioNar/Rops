# Rops
R client to download EPO OPS data. The package is still in development and I am open to collaborations to improve its functionalities. 

At present it presents four basic functions:

- get_biblio: retrieves bibliographic data of patents in the form of dataframes given the epodoc/docdb id of the patent
- get_query: builds a query to search the EPO datasets based on various criteria. See documentation for more information
- get_claims: retrieves claims of a patent given the epodoc id
- get_description: retrieves description of a patent given the epodoc id
