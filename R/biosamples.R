# Functions to handle BioSample Attributes tables.

#' Create BioSamples Table
#'
#' Create a new SRA BioSamples table using a named template and existing sample
#' attributes.
#'
#' @param template_name name of submission type (e.g.,
#'   "MIMS.me.human-associated.4.0")
#' @param sample_attrs data frame of existing sample metadata to draw from.  Any
#'   names given in the \code{col_pairs} argument will be used to explicitly map
#'   column names from the existing data frame to the new data frame.  Remaining
#'   columns with matching names will also be used.  Other columns not in
#'   \code{col_pairs} or the template's fields are ignored.
#' @param submission the accession assigned by the SRA for the submission, like
#'   "SUB####".  Will be attached to the output data frame as an attribute.
#' @param col_pairs named vector of column names in the existing data frame with
#'   names set to column names in the new data frame.  Vector names that don't
#'   match known column names signify custom columns to add.
#' @param constants vector of field names to match to constant values for all
#'   samples.
#'
#' @return data frame with SRA BioSample attributes defined.
#' @export
build_biosamples_from_template <- function(template_name,
                                           sample_attrs,
                                           submission=NULL,
                                           col_pairs=NULL,
                                           constants=NULL) {
  template <- read_template(template_name, "biosample_attributes")
  # Build biosamples data frame, using rows from sample attributes, columns and
  # column classes from template.
  len <- nrow(sample_attrs)*ncol(template)
  biosamples <- as.data.frame(matrix((1:len)*NA, nrow = nrow(sample_attrs)))
  colnames(biosamples) <- colnames(template)
  for (j in 1:ncol(template)) {
    biosamples[[j]] <- as(biosamples[[j]], class(template[[j]]))
  }
  # Add object attributes from template
  for (a in names(attributes(template))) {
    if (! a %in% names(attributes(biosamples)))
      attr(biosamples, a) <- attr(template, a)
  }
  if (! is.null(submission)) {
    attr(biosamples, "submission") <- submission
  }
  attr(biosamples, "template") <- template_name
  # Match up columns for sample attributes table
  biosamples <- fill_from_columns(biosamples, sample_attrs, col_pairs)
  if (! is.null(constants)) {
    for (column in names(constants))
      biosamples[[column]] <- constants[[column]]
  }
  biosamples
}


#' Write SRA BioSample attributes or metadata to disk
#'
#' These functions write SRA spreadsheets to the working directory by type
#' (BioSample attributes or library metadata) with the naming scheme
#' \code{<submission>/<submission>_<type>.tsv} if submission is defined or just
#' \code{<type>.tsv} otherwise.
#'
#' @param data data frame of SRA metadata
#' @param ... additional arguments for \code{\link{write_sra_table}}
#'
#' @export
#' @describeIn write_biosamples Write SRA BioSample attributes to disk
write_biosamples <- function(data, ...) {
  s <- attributes(data)$submission
  if (! is.null(s)) {
    dp <- s
    if (! dir.exists(dp)) {
      dir.create(dp)
    }
    fn <- paste0(s, "_biosamples.tsv")
  } else {
    dp <- "."
    fn <- "biosamples.tsv"
  }
  fp <- file.path(dp, fn)
  write_sra_table(data, fp, ...)
}
