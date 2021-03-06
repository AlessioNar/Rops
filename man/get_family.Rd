% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_family.R
\name{get_family}
\alias{get_family}
\title{Get family members}
\usage{
get_family(id, type, format, access_token)
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
Get family members of the patent
}
\examples{
\dontrun{get_family(id, type = "pub", format = "epodoc ", access_token)}
}
