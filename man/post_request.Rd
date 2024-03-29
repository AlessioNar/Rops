% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_request.R
\name{post_request}
\alias{post_request}
\title{Makes a POST request to the Open Patent Services API}
\usage{
post_request(url, access_token, id = NULL, raw = raw)
}
\arguments{
\item{url}{string, A single url}

\item{access_token}{string, bearer token for authentication}

\item{id}{string vector, epodoc or docdb format}

\item{raw}{bool, r}
}
\value{
A list or a raw string
}
\description{
Makes a POST request to the Open Patent Services API
and it returns the raw or parsed response
}
\examples{
\dontrun{
response <- post_request(url, id, access_token, raw = FALSE)
}

}
