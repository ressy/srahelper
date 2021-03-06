% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{read_template}
\alias{read_template}
\title{Read table template}
\usage{
read_template(template_name, template_type = "biosample_attributes")
}
\arguments{
\item{template_name}{name of submission type (e.g.,
"MIMS.me.human-associated.4.0")}

\item{template_type}{either "biosample_attributes" or "library_metadata".}
}
\value{
data frame with field names and types set by template.
}
\description{
load a template by name (e.g., "MIMS.me.human-associated.4.0" for BioSamples)
and return as a data frame.  Currently supports BioSample and SRA Metadata
template types.
}
