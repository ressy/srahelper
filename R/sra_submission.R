# High-level functions for handling SRA submission data.

#' Create BioSamples Attributes and SRA Metadata Tables
#'
#' Create all required SRA metadata in one go from the given data frames and
#' returns list of resulting data frames.  This encapsulates both
#' \code{\link{build_biosamples_from_template}} and
#' \code{\link{build_metadata}}.
#'
#' If there is a one-to-one match between biological samples and sequencer
#' samples, \code{metadata_attrs} can be left out and \code{biosamples_attrs}
#' will be used for both spreadsheets.
#'
#' @param package_name name of BioSample Package for the template to use, as
#'   either the display name (e.g., "MIGS: cultured bacteria/archaea,
#'   human-associated; version 4.0") or the short name (e.g.,
#'   "MIGS.ba.human-associated.4.0")
#' @param biosample_attrs data frame of existing sample metadata to draw from
#'   for biological samples.  Any names given in the \code{biosample_col_pairs}
#'   argument will be used to explicitly map column names from the existing data
#'   frame to the new data frame.  Remaining columns with matching names will
#'   also be used.  Other columns not in \code{biosample_col_pairs} or the
#'   template's fields are ignored.
#' @param biosample_col_pairs named vector of column names in the existing data
#'   frame with names set to column names in the new data frame.  Vector names
#'   that don't match known column names signify custom columns to add.
#' @param biosample_constants vector of field names to match to constant values
#'   for all samples.
#' @param metadata_attrs data frame of existing library metadata to draw from,
#'   analagous to \code{biosample_attrs}.  If not specified,
#'   \code{biosample_attrs} wil be used.
#' @param metadata_col_pairs named vector for metadata, as for
#'   \code{biosample_col_pairs}.
#' @param metadata_constants named vector for metadata, as for
#'   \code{biosample_constants}.
#' @param submission_accession the accession assigned by the submission portal
#'   for the submission, like "SUB####".  Will be attached to the output data
#'   frames as an attribute if given.
#'
#' @return list of data frames for BioSample Attributes and SRA Metadata.
#' @export
build_sra_submission <- function(package_name, biosample_attrs,
                                 biosample_col_pairs=NULL,
                                 biosample_constants=NULL,
                                 metadata_attrs=NULL,
                                 metadata_col_pairs=NULL,
                                 metadata_constants=NULL,
                                 submission_accession=NULL) {
  # This is currently just the same as calling the BioSample and Metadata
  # functions separately and list'ing the result.
  if (is.null(metadata_attrs)) {
    metadata_attrs <- biosample_attrs
  }
  biosamples <- build_biosamples_from_template(
    package_name,
    biosample_attrs,
    submission = submission_accession,
    col_pairs = biosample_col_pairs,
    constants = biosample_constants)
  metadata <- build_metadata(
    metadata_attrs,
    submission = submission_accession,
    col_pairs = metadata_col_pairs,
    constants = metadata_constants)
  submission <- list(biosamples = biosamples,
                     metadata = metadata)
  submission
}

#' Check both SRA submission spreadsheets for problems
#'
#' Check the biosample attributes and library metadata data frames for problems
#' using \code{\link{validate_fields}}.
#'
#' @param submission list of data frames of SRA submission information
#' @param ... any additional arguments to validate_fields
#'
#' @return list of character vector of warnings for each data frame
#' @export
validate_sra_submission <- function(submission, ...) {
  lapply(submission, validate_fields, ... = ...)
}

#' Write SRA submission spreadsheets to disk
#'
#' These functions write SRA spreadsheets to the working directory by type
#' (BioSample attributes and library metadata) with the naming scheme
#' \code{<submission>/<submission>_<type>_<part>.tsv}.  Each \code{part} splits
#' the submission spreadsheets so as to have fewer than 1000 rows in both, while
#' keeping entries for a given sample_name together between spreadsheet pairs.
#' \code{submission} will use the submission accession attribute on the data
#' frames, if present.  If either of these fields are not appliable they will be
#' excluded from the filename.  At its most basic this will simply save
#' \code{biosamples.tsv} and \code{metadata.tsv}.
#'
#' @param submission list of data frames of SRA submission information, such as
#'   created by \code{\link{build_sra_submission}}.
#' @param ... additional arguments for \code{\link{write_table}}
#'
#' @return list of vectors of file paths saved for each spreadsheet category
#'
#' @export
write_sra_submission <- function(submission, ...) {
  submission <- chunk_submission(submission)
  # Handle biosamples and metadata separately
  result <- list()
  for (thing in names(submission)) {
    for (i in seq_along(submission[[thing]])) {
      data <- submission[[thing]][[i]]
      sub_acc <- attributes(submission[[thing]][[i]])$submission
      # If we have a submission accession, include that in the
      # directory/filename.
      if (! is.null(sub_acc)) {
        dp <- sub_acc
        if (! dir.exists(dp)) {
          dir.create(dp)
        }
        fn_prefix <- paste(sub_acc, thing, sep = "_")
      } else {
        dp <- "."
        fn_prefix <- thing
      }
      # If the chunking above actually required a split, include that in the
      # filename.
      if (length(submission[[thing]]) > 1) {
        fn <- paste0(fn_prefix, "_", i, ".tsv")
      } else {
        fn <- paste0(fn_prefix, ".tsv")
      }
      fp <- file.path(dp, fn)
      write_table(data, fp, ...)
      result[[thing]] <- c(result[[thing]], fp)
    }
  }
  result
}


# Util --------------------------------------------------------------------


#' Chunk up submission data frames
#'
#' The SRA web interface will not allow a spreadsheet to be uploaded that
#' contains more than 1000 rows, for both BioSample Attributes and Metadata
#' (even though the documentation just says "1000 samples.")
#' https://www.ncbi.nlm.nih.gov/sra/docs/submitportal/
#' This will take a submission list pair of biosample/metadata data frames and
#' split each into its own list, keeping the number of rows below 1000 for all
#' output data frames.  The resulting spreadsheets will need to be uploaded in
#' separate submissions under the same BioProject.  The data frames here will be
#' organized so a given sample has all of its data in the same "chunk."
#'
#' @param submission list of submission data, as created by
#'   \code{\link{build_sra_submission}}.
#' @param rowsmax maximum number of rows to be permitted in each chunk
#'   (spreadsheet).
chunk_submission <- function(submission, rowsmax=1000) {
  # Get number of md rows for each unique sample name
  # (We'll always have at least one row in the metadata for a given row in the
  # biosamples, so we may as well just go by the metadata.)
  md <- submission$metadata
  entries_per_name <- sapply(unique(md$sample_name),
                             function(this) sum(md$sample_name == this))
  # assign each sample name to a given chunk to keep max rows per chunk under
  # rowsmax
  chunk_ids <- sort(ceiling(cumsum(entries_per_name) / rowsmax))
  lapply(submission, function(obj) {
    lapply(split(names(chunk_ids), chunk_ids), function(chunk) {
      obj[obj$sample_name %in% chunk, ]
    })
  })
}
