# Rops

R client to download and parse EPO data using the Open Patent Service

## Authentication process

In order to access the OPS API v3.2, you need to follow these steps:

* register to the OPS [web service](https://www.epo.org/searching-for-patents/data/web-services/ops.html)
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


## Retrieving Patent information

At this stage, Rops can be used to call the OPS endpoints related to the Published-data services, through the `publication services` function:

* fulltext
* claims
* description
* abstract
* biblio
* full-cycle


### Example usage

```{r}

library(Rops)

# Retrieve the abstract of the patent publication with epodoc number US2022179620.A1 as a list
publication_services(type = "publication", format="epodoc", id='US2022179620.A1', access_token = access_token, what = 'abstract')

# Retrieve the full text of the patent publication with epodoc number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'fulltext')

# Retrieve the claims of the patent publication with epodoc number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'claims')

# Retrieve the description of the patent publication with epodoc number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'description')

# Retrieve the bibliographic information of the patent publication with epodoc number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'biblio')

# Retrieve the full-cycle information of the patent publication with epodoc number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'full-cycle')

# Retrieve the equivalent of the patent publication with epodoc application number EP1000000.A1 as a list
publication_services(type = "publication", format="epodoc", id='EP1000000.A1', access_token = access_token, what = 'equivalents')

# Retrieve the full-cycle information of the patent publication with docdb number EP1000000 as a raw text
publication_services(type = "publication", format="docdb", id='EP1000000', access_token = access_token, what = 'full-cycle', raw=TRUE)

# Retrieve the abstracts of the patent publications with epodoc numbers EP1000000.A1 and US2022179620.A1 as a list
publication_services(type = "publication", format="epodoc", id=c('EP1000000.A1', 'US2022179620.A1'), access_token = access_token, what = 'abstract')


```

## Additional information

* [OPS 3.2 API documentation](http://documents.epo.org/projects/babylon/eponet.nsf/0/F3ECDCC915C9BCD8C1258060003AA712/$File/ops_v3.2_documentation_-_version_1.3.18_en.pdf)