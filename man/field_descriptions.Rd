% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biosamples.R
\name{field_descriptions}
\alias{field_descriptions}
\title{Get BioSample field descriptions}
\usage{
field_descriptions(fields)
}
\arguments{
\item{fields}{vector of field names, like \code{c("env_biome", "env_feature")}.}
}
\value{
data frame of field names and descriptions.
}
\description{
Given a vector of BioSample field (column) names, return a data frame of
descriptions for each.
}
\examples{
fields <- c("env_biome", "env_feature")
descs <- field_descriptions(fields)
# descs is a data frame of field/descrption pairs
# Try View(descs)
}
