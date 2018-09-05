# Functions to handle Library Metadata tables.

#' @describeIn write_biosamples Write SRA library metadata to disk
write_metadata <- function(data) {
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
  write_sra_table(data, fp)
}

#' Create blank metadata
#'
#' Create a new empty SRA metadata data frame with columns named and typed
#' accordingly.
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


#' Check for blank cells
#'
#' @param data vector of data to check.
#'
#' @return True if NA or empty, FALSE otherwise.
#'
#' @export
blank <- function(data) {
  is.na(data) | data == ""
}

#' Fill blanks with SRA keyword
#'
#' @param vec either vector of column names (if data not NULL) or explicit data.
#' @param data data frame to use, if filling multiple columns.
#' @param value text to fill with; one of the allowed values in
#'   \code{BLANK_TYPES}.
#'
#' @return data modified vector or data frame with blanks filled in.
#'
#' @export
fill_blanks <- function(vec, data=NULL, value="missing") {
  if (! value %in% BLANK_TYPES) {
    warning(paste("Value should be one of:"),
            paste(BLANK_TYPES, collapse = ", "))
  }
  if (is.null(data)) {
    vec[blank(vec)] <- value
    vec
  } else {
    for (v in vec) {
      data[[v]][blank(data[[v]])] <- value
    }
    data
  }
}

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
      data_new[[colname]] <- as(data_old[[colname_old]],
                                class(data_new[[colname]]))
  }
  # Add any additional entries as extra columns
  if (! is.null(col_pairs)) {
    colnames_extra <- col_pairs[! names(col_pairs) %in% colnames(data_new)]
    data_new[colnames_extra] <- data_old[colnames_extra]
  }
  data_new
}

#' Create factors for known fields with fixed vocabulary
#'
#' For SRA fields in the given data frame known to have a strictly fixed
#' vocabulary (according to \code{FIXED_VOCABULARY}), convert each to a factor
#' with levels corresponding to the vocabulary.
#'
#' @param data data frame to modify.
#'
#' @return data frame with recognized columns converted to factors.
process_fixed_vocab <- function(data) {
  nms <- names(FIXED_VOCABULARY)[match(colnames(data), names(FIXED_VOCABULARY))]
  nms <- nms[! is.na(nms)]
  for (nm in nms) {
    data[[nm]] <- factor(data[[nm]], levels = unlist(FIXED_VOCABULARY[[nm]]))
  }
  data
}
