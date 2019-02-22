#' NCBI SRA Metadata
#'
#' These functions help manage SRA Metadata spreadsheets and individual
#' fields.
#'
#' Notable functions:
#'
#' * \code{\link{build_metadata}}: make a new spreadsheet from existing
#'   metadata and the standard template.
#' * \code{\link{write_metadata}}: save a prepared metadata spreadsheet to a
#'   file.
#'
#' @md
#'
#' @name sra_metadata
NULL

#' @describeIn write_biosamples Write SRA library metadata to disk
write_metadata <- function(data, ...) {
  write_table(data, fp_suffix = "metadata", ...)
}

#' Create blank metadata
#'
#' Create a new empty SRA metadata data frame with columns named and typed
#' accordingly.
#'
#' @param sample_attrs data frame of existing sample metadata to draw from.  Any
#'   names given in the \code{col_pairs} argument will be used to explicitly map
#'   column names from the existing data frame to the new data frame.  Remaining
#'   columns with matching names will also be used.  Other columns not in
#'   \code{col_pairs} or the template's fields are ignored.
#' @param submission the accession assigned by the submission portal for the
#'   submission, like "SUB####".  Will be attached to the output data frame as
#'   an attribute.
#' @param col_pairs named vector of column names in the existing data frame with
#'   names set to column names in the new data frame.  Vector names that don't
#'   match known column names signify custom columns to add.
#' @param constants vector of field names to match to constant values for all
#'   samples.
#'
#' @return data frame with SRA metadata columns
#' @export
build_metadata <- function(sample_attrs,
                              submission=NULL,
                              col_pairs=NULL,
                              constants=NULL) {
  N <- nrow(sample_attrs)
  metadata <- within(list(), {
    # Last text fields
    filename             <- character(N)
    filetype             <- character(N)
    # These fields will be factors when processed by process_fixed_vocab.
    instrument_model     <- character(N)
    platform             <- character(N)
    library_layout       <- character(N)
    library_selection    <- character(N)
    library_source       <- character(N)
    library_strategy     <- character(N)
    # Text fields
    design_description   <- character(N)
    library_ID           <- character(N)
    title                <- character(N)
    bioproject_accession <- character(N)
    biosample_accession  <- character(N)
    sample_name          <- character(N)
  })
  metadata <- do.call(data.frame, c(metadata, list(stringsAsFactors = FALSE)))
  metadata <- fill_from_columns(metadata, sample_attrs, col_pairs)
  metadata <- process_fixed_vocab(metadata)
  # All fields added so far are mandatory
  attr(metadata, "mandatory_fields") <- colnames(metadata)
  # Fill in any additional filenameN columns.
  columns <- grep("^filename[0-9]+$", colnames(sample_attrs), value = TRUE)
  metadata[columns] <- sample_attrs[columns]
  # Constant fields
  if (! is.null(constants)) {
    for (column in names(constants)) {
      metadata[[column]] <- if (is.factor(metadata[[column]])) {
        factor(constants[[column]], levels = levels(metadata[[column]]))
      } else {
        constants[[column]]
      }
    }
  }
  if (! is.null(submission)) {
    attr(metadata, "submission") <- submission
  }
  metadata
}

#' Download Library Metadata Template
#'
#' Download the library metadata template as a TSV file.
#'
#' @param fp file path to write to, or NULL to use a default within the package
#'   directory.
#'
#' @return file path written to
#'
#' @export
download_metadata <- function(fp=NULL) {
  url <- paste0("ftp://",
                FTP_SRV["TRACE"],
                "/sra/metadata_table/SRA_metadata.tsv")
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    dp_templates <- file.path(dp, "templates", "library_metadata")
    if (! dir.exists(dp_templates)) {
      dir.create(dp_templates, recursive = TRUE)
    }
    fp <- file.path(dp_templates, "SRA_metadata.tsv")
  }
  utils::download.file(url = url, destfile = fp)
  names(fp) <- "metadata"
  return(fp)
}


# Util --------------------------------------------------------------------


#' Fill in columns from reference
#'
#' Fill columns in one data frame from another, using data classes from the
#' destination data frame.  Optionally give a vector of mappings from one set of
#' column names to the other.
#'
#' @param data_new data frame to modify.  All column names present here will be
#'   checked for in \code{data_old}, enforcing data classes in \code{data_new}.
#' @param data_old data frame to pull from.  Column names here matching columns
#'   in \code{data_new} will be used.
#' @param col_pairs character vector of column names in new data frame (vector
#'   names) matched to column names in old data frame (vector values) to use.
#'
#' @return data frame of \code{data_new} values updated with matching columns
#'   from \code{data_old}.
fill_from_columns <- function(data_new, data_old, col_pairs=NULL) {
  for (colname in colnames(data_new)) {
    # Use mapping to get old name, if available and if present in old data
    # frame.  Otherwise just use new name.
    if (! is.null(col_pairs) && col_pairs[colname] %in% colnames(data_old)) {
      colname_old <- col_pairs[colname]
    } else {
      colname_old <- colname
    }
    if (colname_old %in% colnames(data_old))
      data_new[[colname]] <- methods::as(data_old[[colname_old]],
                                         class(data_new[[colname]]))
  }
  # Add any additional entries as extra columns
  if (! is.null(col_pairs)) {
    colnames_extra <- col_pairs[! names(col_pairs) %in% colnames(data_new)]
    data_new[names(colnames_extra)] <- data_old[colnames_extra]
  }
  data_new
}
