% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{templates}
\alias{templates}
\title{NCBI Submission Templates}
\description{
Some common concepts apply for multiple submission template types (e.g. SRA,
BioSample, GenBank) like required and optional fields and fixed-vocabulary or
otherwise restricted-value fields.  A given submission type (e.g. BioSample)
can have a wide range of possible templates (Human, Microbe, Virus, etc.).
Template-related functions here help manage listing and loading these
templates.  These are used by more specific helpers like
\code{\link{build_biosamples_from_template}} from the \link{biosamples}
functions.
}
\details{
\itemize{
\item \code{\link{list_template_types}}: List the types (submission categories)
that templates are defined for.
\item \code{\link{list_templates}}: List templates within a category.
\item \code{\link{read_template}}: Load an installed template as a data frame.
}
}
