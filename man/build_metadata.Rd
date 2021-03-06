% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sra_metadata.R
\name{build_metadata}
\alias{build_metadata}
\title{Create blank metadata}
\usage{
build_metadata(sample_attrs, submission = NULL, col_pairs = NULL,
  constants = NULL)
}
\arguments{
\item{sample_attrs}{data frame of existing sample metadata to draw from.  Any
names given in the \code{col_pairs} argument will be used to explicitly map
column names from the existing data frame to the new data frame.  Remaining
columns with matching names will also be used.  Other columns not in
\code{col_pairs} or the template's fields are ignored.}

\item{submission}{the accession assigned by the submission portal for the
submission, like "SUB####".  Will be attached to the output data frame as
an attribute.}

\item{col_pairs}{named vector of column names in the existing data frame with
names set to column names in the new data frame.  Vector names that don't
match known column names signify custom columns to add.}

\item{constants}{vector of field names to match to constant values for all
samples.}
}
\value{
data frame with SRA metadata columns
}
\description{
Create a new empty SRA metadata data frame with columns named and typed
accordingly.
}
