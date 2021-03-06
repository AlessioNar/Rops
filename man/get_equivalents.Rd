% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_equivalents.R
\name{get_equivalents}
\alias{get_equivalents}
\title{Get equivalents}
\usage{
get_equivalents(id, type, format, access_token)
}
\arguments{
\item{id}{identification code of the patent.}

\item{type}{type of document to be retrieved. It can either be a publication or an application (pub or app)}

\item{format}{format type of the patent id, it must be either epodoc or docdb}

\item{access_token}{access token generated using the create_access_token function}
}
\value{
Family members of the selected patent
}
\description{
Get patent equivalents
}
\examples{
\dontrun{get_equivalents(id, type = "pub", format = "epodoc ", access_token)}
}
