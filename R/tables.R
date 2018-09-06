# Functions to handle SRA metadata tables generally, and individual fields.

# TODO xlsx support via openxlsx?

#' Read and write SRA metadata to disk
#'
#' These functions read and write to/from a data frame of SRA metadata (a
#' BioSamples spreadsheet, library prep metadata, or run info) to/from a file
#' in TSV format.
#'
#' @param data data frame of SRA metadata
#' @param fp file path to save text
#' @param overwrite should existing files be replaced?  If \code{FALSE}
#'   (default), an existing file throws an error.  If \code{TRUE}, an existing
#'   file is replaced without any prompting.
#' @param ... additional arguments for \code{utils::write.table}
#'
#' @export
#' @describeIn write_sra_table Write SRA metadata to disk
write_sra_table <- function(data, fp, overwrite=FALSE, ...) {
  # empty strings are allowed in the BioProject Accession column for the
  # Biosamples spreadsheet, but not "not applicable".
  if ("bioproject_accession" %in% colnames(data)) {
    idxl <- is.na(data$bioproject_accession)
    data$bioproject_accession[idxl] <- ""
  }
  # Likewise handle blank sample_title.  This is optional so blanks will be more
  # appropriate.
  if ("sample_title" %in% colnames(data)) {
    idxl <- is.na(data$sample_title)
    data$sample_title[idxl] <- ""
  }
  if (file.exists(fp) & ! overwrite) {
    stop(paste("Destination file already exists:", fp))
  }
  utils::write.table(x = data,
                     file = fp,
                     sep = "\t",
                     na = "not applicable",
                     row.names = FALSE,
                     ...)
}

#' @describeIn write_sra_table Read SRA metadata from disk
read_sra_table <- function(fp, ...) {
  # The SRA templates treat their comments like regular TSV fields so they might
  # be quoted.  This confuses read.table.
  rawdata <- readChar(fp, file.info(fp)$size)
  rawlines <- strsplit(rawdata, "[\\r\\n]+", perl = TRUE)[[1]]
  idxl <- grepl("^\"?#", rawlines)
  comments <- rawlines[idxl]
  rawlines <- rawlines[! idxl]
  data <- utils::read.table(text = rawlines,
                            sep = "\t",
                            na.strings = "not applicable",
                            stringsAsFactors = FALSE,
                            header = TRUE,
                            check.names = FALSE,
                            colClasses = "character",
                            ...)
  # Keep comments
  attr(data, "comments") <- comments
  # Mandatory fields are marked with an asterisk
  reqs <- grep("^\\*", colnames(data))
  colnames(data) <- make.names(sub("^\\*", "", colnames(data)))
  attr(data, "mandatory_fields") <- colnames(data)[reqs]
  data <- process_fixed_vocab(data)
  if ("sample_name" %in% colnames(data)) {
    data[["sample_name"]] <- as.character(data[["sample_name"]])
    rownames(data) <- make.names(data$sample_name)
  }
  data
}

#' Check SRA fields for problems
#'
#' Check a data frame of SRA fields (biosample attributes or library metadata)
#' for problems that may interfere with a submission.  Any problems found will
#' be raised as warnings (unless \code{quiet=TRUE}) and returned as a vector.
#'
#' @param data data frame of SRA fields
#' @param quiet skip calling \code{warning} for each problem?
#'
#' @return character vector of warnings
#' @export
validate_fields <- function(data, quiet=FALSE) {
  problems <- c()
  # Mandatory fields
  if ("mandatory_fields" %in% names(attributes(data))) {
    for (a in attr(data, "mandatory_fields")) {
      # These two are "mandatory" but can be left all-blank during all-in-one
      # submissions.
      if (a %in% c("biosample_accession", "bioproject_accession") &&
          all(blank(data[[a]]))) {
        next
      }
      if (any(blank(data[[a]]))) {
        problems <- c(problems,
                      paste("Mandatory field is missing values:",
                            a))
      }
    }
  }
  # Filename columns
  columns <- grep("^filename[0-9]*$", colnames(data), value=TRUE)
  for (column in columns) {
    if (any(basename(data[[column]]) != data[[column]]))
      problems <- c(problems,
                    paste("Filename column contains directory paths:", column))
  }
  # TODO validate other specific columns:
  # collection_date
  # ...?
  if (! quiet) {
    for (p in problems) {
      warning(p)
    }
  }

  return(invisible(problems))
}

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