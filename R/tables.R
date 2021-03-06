#' NCBI Metadata Tables
#'
#' These functions handle NCBI metadata tables generally, and individual fields.
#'
#' Notable functions:
#'
#' * \code{\link{read_table}}: read a TSV spreadsheet to a data frame
#' * \code{\link{write_table}}: save a prepared spreadsheet to a file
#' * \code{\link{validate_fields}}: check known NCBI metadata fields for
#'   problems.
#' * \code{\link{check_uniqueness}}: check for uniqueness of rows by
#'   NCBI's rules
#' * \code{\link{fill_blanks}}: fill in blank entries with one of the allowed
#'   values
#' * \code{\link{datemunge}}: Standardize date strings as YYY-MM-DD
#'
#' @md
#'
#' @name tables
NULL


# Input/Output ------------------------------------------------------------


# TODO xlsx support via openxlsx?

#' Read and write metadata to disk
#'
#' These functions read and write to/from a data frame of NCBI metadata (a
#' BioSamples spreadsheet, SRA metadata, or run info) to/from a file in TSV
#' format.
#'
#' @param data data frame of metadata
#' @param fp file path to save text.  If \code{NULL}, will be determined by the
#'   \code{submission} attribute on \code{data}, if present, and
#'   \code{fp_suffix}, if given, relative to the current working directory.
#' @param fp_suffix A filename suffix to use when constructing output file path
#'   if \code{fp} is not given.
#' @param overwrite should existing files be replaced?  If \code{FALSE}
#'   (default), an existing file throws an error.  If \code{TRUE}, an existing
#'   file is replaced without any prompting.
#' @param ... additional arguments for \code{utils::write.table}
#'
#' @return The file path written to, for \code{write_table}, or a data frame
#'   read, for \code{read_table}.
#'
#' @export
#' @describeIn write_table Write metadata to disk
write_table <- function(data, fp=NULL, fp_suffix="data",
                            overwrite=FALSE, ...) {
  if (is.null(fp)) {
    # Automatically determine output file path
    s <- attributes(data)$submission
    if (! is.null(s)) {
      dp <- s
      if (! dir.exists(dp)) {
        dir.create(dp)
      }
      fn <- paste0(s, "_", fp_suffix, ".tsv")
    } else {
      dp <- "."
      fn <- paste0(fp_suffix, ".tsv")
    }
    fp <- file.path(dp, fn)
  }
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
  return(fp)
}

#' @describeIn write_table Read metadata from disk
#' @export
read_table <- function(fp, ...) {
  # The NCBI submission templates treat their comments like regular TSV fields,
  # so they might be quoted.  This confuses read.table.
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
    rownames(data) <- make.names(data$sample_name, unique = TRUE)
  }
  data
}


# Validate Fields ---------------------------------------------------------


#' Check metadata fields for problems
#'
#' Check a data frame of metadata (biosample attributes or SRA metadata) for
#' problems that may interfere with a submission.  Any problems found will be
#' raised as warnings (unless \code{quiet=TRUE}) and returned as a vector.
#'
#' @param data data frame of metadata fields
#' @param quiet skip calling \code{warning} for each problem?
#'
#' @return character vector of warnings
#' @export
validate_fields <- function(data, quiet=FALSE) {
  # https://www.ncbi.nlm.nih.gov/biosample/docs/submission/validation/
  problems <- validate_mandatory_fields(data)
  problems <- validate_filenames(data, problems)
  problems <- validate_hardware(data, problems)
  problems <- validate_uniqueness(data, problems)

  attr(problems, "fields") <- unique(attr(problems, "fields"))

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

validate_mandatory_fields <- function(data, problems=character()) {
  fields <- c()
  all_blank_ok <- c("biosample_accession", "bioproject_accession")
  # Mandatory fields, as identified by an attribute attached to the data frame.
  if ("mandatory_fields" %in% names(attributes(data))) {
    for (a in attr(data, "mandatory_fields")) {
      # These two are "mandatory" but can be left all-blank during all-in-one
      # submissions.
      if (a %in% all_blank_ok && all(blank(data[[a]]))) {
        next
      }
      if (! a %in% colnames(data) || any(blank(data[[a]]))) {
        problems <- c(problems,
                      paste("Mandatory field is missing values:",
                            a))
        fields <- c(fields, a)
      }
    }
  }
  attr(problems, "fields") <- fields
  problems
}

validate_filenames <- function(data, problems=character()) {
  fields <- attr(problems, "fields")
  columns <- grep("^filename[0-9]*$", colnames(data), value = TRUE)
  for (column in columns) {
    if (any(basename(data[[column]]) != data[[column]]))
      problems <- c(problems,
                    paste("Filename column contains directory paths:", column))
      fields <- c(fields, column)
  }
  attr(problems, "fields") <- fields
  problems
}

validate_hardware <- function(data, problems=character()) {
  fields <- attr(problems, "fields")
  platform <- data[["platform"]]
  instrument_model <- data[["instrument_model"]]
  hardware_mismatch <- FALSE
  if (! is.null(platform) && ! all(blank(platform))) {
    if (any(! platform %in% FIXED_VOCABULARY$platform)) {
      problems <- c(problems,
                    "Unknown platform values")
      hardware_mismatch <- TRUE
      fields <- c(fields, "platform")
    }
  }
  if (! is.null(instrument_model) && ! all(blank(instrument_model))) {
    inst_all <- unlist(FIXED_VOCABULARY$instrument_model)
    if (any(! instrument_model %in% inst_all)) {
      problems <- c(problems,
                    "Unknown instrument_model values")
      hardware_mismatch <- TRUE
      fields <- c(fields, "instrument_model")
    }
  }
  if (! is.null(platform)         && ! all(blank(platform)) &&
      ! is.null(instrument_model) && ! all(blank(instrument_model)) &&
      ! hardware_mismatch) {
    # Last thing; are any platform/model pairs mismatched?
    ms <- mapply(function(p, m) m %in% FIXED_VOCABULARY$instrument_model[[p]],
                 platform,
                 instrument_model)
    if (! all(ms)) {
      problems <- c(problems,
                    "Mismatched instrument_model and platform values")
      fields <- c(fields, "platform", "instrument_model")
    }
  }
  attr(problems, "fields") <- fields
  problems
}

validate_uniqueness <- function(data, problems=character()) {
  fields <- attr(problems, "fields")
  if ("sample_name" %in% attributes(data)$mandatory_fields) {
    keys <- check_uniqueness(data)
    if (length(keys) != length(unique(keys))) {
      problems <- c(problems,
                    paste("Multiple entries cannot have identical",
                          "attributes (see ?check_uniqueness)"))
    }
  }
  attr(problems, "fields") <- fields
  problems
}


# Other Helpers -----------------------------------------------------------


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

# make a factor to label each row of input data

#' Check sample uniqueness from metadata
#'
#' Per NCBI's rules: "You should have one BioSample for each specimen, and each
#' of your BioSamples must have differentiating information (excluding sample
#' name, title, bioproject accession and description). This check was
#' implemented to encourage submitters to include distinguishing information in
#' their samples. If the distinguishing information is in the sample name, title
#' or description, please recode it into an appropriate attribute, either one of
#' the predefined attributes or a custom attribute you define. If it is
#' necessary to represent true biological replicates as separate BioSamples, you
#' might add an 'aliquot' or 'replicate' attribute, e.g., 'replicate =
#' biological replicate 1', as appropriate. Note that multiple assay types,
#' e.g., RNA-seq and ChIP-seq data may reference the same BioSample if
#' appropriate."
#'
#' @param data data frame of BioSample attributes
#'
#' @return factor with levels corresponding to unique rows after excluding
#'   certain columns.
#'
#' @export
check_uniqueness <- function(data) {
  cols_ignore <- c("sample_name",
                   "title",
                   "bioproject_accession",
                   "description")
  cols_idx <- match(cols_ignore, colnames(data), nomatch = 0)
  # This ensures that we'll always have as many rows out as went in
  data2 <- cbind("", data[, -cols_idx, drop = FALSE])
  key <- do.call(paste, data2)
  key <- factor(key)
  key
}

#' Fill blanks with NCBI-allowed keyword
#'
#' Factors will be ignored since they have their own controlled vocabulary, and
#' biosample_accession and bioproject_accession columns will be ignored for data
#' frames as these are allowed to be left blank for all-in-one SRA submissions.
#'
#' @param vec either vector of column names (if data not NULL) or explicit data.
#' @param data data frame to use, if filling multiple columns.
#' @param value text to fill with; one of the allowed values in
#'   \code{\link{BLANK_TYPES}}.
#'
#' @return data modified vector or data frame with blanks filled in.
#'
#' @export
fill_blanks <- function(vec, data=NULL, value="missing") {
  if (! value %in% BLANK_TYPES) {
    warning(paste("Value should be one of:"),
            paste(BLANK_TYPES, collapse = ", "))
  }
  if (is.null(data) & !is.factor(vec)) {
    vec[blank(vec)] <- value
    vec
  } else {
    ignores <- c("biosample_accession", "bioproject_accession")
    for (v in vec[! vec %in% ignores]) {
      if (! is.factor(data[[v]])) {
        data[[v]][blank(data[[v]])] <- value
      }
    }
    data
  }
}

#' Convert date text strings into YYY-MM-DD
#'
#' Convert month/day/year date stamps, like 5/31/2016, or other permutations of
#' those positions (e.g., 31/5/2016, 2016/5/31), to the  ISO 8601 standard of
#' YYY-MM-DD.  Any NA values in output that do not have associated blank or NA
#' values in input trigger an error.  This is just a wrapper around
#' \code{as.Date} to help with formatting and error checking.
#'
#' @param datetxt character vector of dates
#' @param ord_input character vector containing the values "M", "D", and "Y"
#'   ordered to specify the month, date, and year fields
#'
#' @return character vector of ISO 8601-formatted date stamps
#'
#' @export
#'
#' @examples
#' dates <- c("5/31/2016", "10/1/2018", "1/2/2019")
#' datemunge(dates)
#' dates_euro <- c("31/5/2016", "1/10/2018", "2/1/2019")
#' datemunge(dates_euro, ord_input = c("D", "M", "Y"))
datemunge <- function(datetxt, ord_input = c("M", "D", "Y")) {
  # build a format string from the order of the fields
  fmt <- ord_input
  fmt <- gsub("M", "m", fmt)
  fmt <- gsub("D", "d", fmt)
  fmt <- paste0("%", fmt, collapse = "/")
  dateout <- format(as.Date(datetxt, fmt), "%Y-%m-%d")
  # NA or blank input can give NA output, but otherwise we'll strictly disallow
  # the usual sloppy casting in R.
  baddates <- is.na(dateout) & ! blank(datetxt)
  if (any(baddates)) {
    stop(paste("Date formatting failed for:",
               paste(datetxt[baddates], collapse = ", ")))
  }
  return(dateout)
}

#' Create factors for known fields with fixed vocabulary
#'
#' For metadata fields in the given data frame known to have a strictly fixed
#' vocabulary (according to \code{FIXED_VOCABULARY}), convert each to a factor
#' with levels corresponding to the vocabulary.  For example, the library_layout
#' column in an SRA metadata spreadsheet can only contain the values "Single" or
#' "Paired".
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
