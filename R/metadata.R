# Functions to handle Library Metadata tables (the info specific to sample
# preparation and sequencing; for general metadata, see tables.R.)

#' @describeIn write_biosamples Write SRA library metadata to disk
write_metadata <- function(data, ...) {
  s <- attributes(data)$submission
  if (! is.null(s)) {
    dp <- s
    if (! dir.exists(dp)) {
      dir.create(dp)
    }
    fn <- paste0(s, "_metadata.tsv")
  } else {
    dp <- "."
    fn <- "metadata.tsv"
  }
  fp <- file.path(dp, fn)
  write_sra_table(data, fp, ...)
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
#' @param submission the accession assigned by the SRA for the submission, like
#'   "SUB####".  Will be attached to the output data frame as an attribute.
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
  })
  metadata <- do.call(data.frame, c(metadata, list(stringsAsFactors=FALSE)))
  metadata <- fill_from_columns(metadata, sample_attrs, col_pairs)
  metadata <- process_fixed_vocab(metadata)
  # All fields added so far are mandatory
  attr(metadata, "mandatory_fields") <- colnames(metadata)
  # Fill in any additional filenameN columns.
  columns <- grep("^filename[0-9]+$", colnames(sample_attrs), value=TRUE)
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

# remove any fields that are optional and entirely blank.
tidy_optional_fields <- function(data) {
  fields <- attributes(data)$optional_fields
  if (! is.null(fields)) {
    for (f in fields) {
    if (all(blank(data[[f]]))) {
        data[[f]] <- NULL
      }
    }
  }
  data
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
    data_new[colnames_extra] <- data_old[colnames_extra]
  }
  data_new
}
