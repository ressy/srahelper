# Metadata Tables ---------------------------------------------------------


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

#' Write SRA BioSample attributes or metadata to disk
#'
#' These functions write SRA spreadsheets to the working directory by type
#' (BioSample attributes or library metadata) with the naming scheme
#' \code{<submission>/<submission>_<type>.tsv} if submission is defined or just
#' \code{<type>.tsv} otherwise.
#'
#' @param data data frame of SRA metadata
#'
#' @export
#' @describeIn write_biosamples Write SRA BioSample attributes to disk
write_biosamples <- function(data) {
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
  write_sra_table(data, fp)
}

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
#' @param col_pairs named vector of column names in the existing data frame with
#'   names set to column names in the new data frame.  Vector names that don't
#'   match known column names signify custom columns to add.
#' @param constants vector of field names to match to constant values for all
#'   samples.
#'
#' @return data frame with SRA BioSample attributes defined.
#' @export
build_biosamples_from_template <- function(template_name, sample_attrs,
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

#' Read SRA table template
#'
#' load an SRA template by name (e.g., "MIMS.me.human-associated.4.0") and
#' return as a data frame.
#'
#' @param template_name name of submission type (e.g.,
#'   "MIMS.me.human-associated.4.0")
#' @param template_type either "biosample_attributes" or "library_metadata".
#'
#' @return data frame with SRA field names and types set by template.
#' @export
read_template <- function(template_name,
                          template_type="biosample_attributes") {
  if (! template_type %in% list_template_types()) {
    warning(paste("Template type not recognized:", template_type))
  } else if (! template_name %in% list_templates(template_type)) {
    warning(paste("Template name not recognzied:", template_name))
  }
  fp <- system.file("exdata", "templates", template_type,
                    paste(template_name, "tsv", sep = "."),
                    package = methods::getPackageName())
  data <- read_sra_table(fp)
  mf <- attributes(data)$mandatory_fields
  of <- colnames(data)[! colnames(data) %in% mf]
  attr(data, "optional_fields") <- of
  data
}

#' List installed template files
#'
#' List names of templates installed in the package.  These are the spreadsheets
#' the SRA web interface gives you during a submission at the attributes and
#' metadata steps.
#'
#' @param template_type either "biosample_attributes" or "library_metadata".
#'
#' @return vector of template names, without file extension.
#' @export
list_templates <- function(template_type="biosample_attributes") {
  if (! template_type %in% list_template_types()) {
    warning(paste("Template type not recognized:", template_type))
  }
  dp <- system.file("exdata", "templates", template_type,
                    package = methods::getPackageName())
  fps <- list.files(dp)
  gsub("\\.tsv$", "", fps)
}

#' List template types
#'
#' List types of SRA templates recognized.
#'
#' @return vector of template type names.
#' @export
list_template_types <- function() {
  dp <- system.file("exdata", "templates",
                    package = methods::getPackageName())
  fps <- list.files(dp, full.names = TRUE)
  fps <- basename(fps[sapply(fps, dir.exists)])
  fps
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

# Constants ---------------------------------------------------------------


#' SRA empty field values
#'
#' These strings are listed in the SRA docs as options for data not present even
#' for required fields.  See \code{\link{fill_blanks}}.
#'
#' @export
BLANK_TYPES <- c('not collected',
                 'not applicable',
                 'missing')

#' Metadata fields with restricted values
#'
#' These are metadata field names that can only take on the values listed for
#' each name.
#' @export
FIXED_VOCABULARY <- list(
  platform = c(
    "LS454",
    "OXFORD_NANOPORE",
    "HELICOS",
    "ABI_SOLID",
    "COMPLETE_GENOMICS",
    "PACBIO_SMRT",
    "ION_TORRENT",
    "CAPILLARY",
    "ILLUMINA"
  ),
  instrument_model = list(
    "LS454" = c(
      "454 GS",
      "454 GS 20",
      "454 GS FLX",
      "454 GS FLX+",
      "454 GS FLX Titanium",
      "454 GS Junior"),
    "OXFORD_NANOPORE" = c(
      "GridION",
      "MinION"
    ),
    "HELICOS" = c(
      "Helicos HeliScope"
    ),
    "ABI_SOLID" = c(
      "AB SOLiD System",
      "AB SOLiD System 2.0",
      "AB SOLiD System 3.0",
      "AB SOLiD 4 System",
      "AB SOLiD 4hq System",
      "AB SOLiD PI System",
      "AB 5500 Genetic Analyzer",
      "AB 5500xl Genetic Analyzer",
      "AB 5500x-Wl Genetic Analyzer",
      "AB SOLiD 3 Plus System"
    ),
    "COMPLETE_GENOMICS" = c(
      "Complete Genomics"
    ),
    "PACBIO_SMRT" = c(
      "PacBio RS",
      "PacBio RS II"
    ),
    "ION_TORRENT" = c(
      "Ion Torrent PGM",
      "Ion Torrent Proton"
    ),
    "CAPILLARY" = c(
      "AB 3730xL Genetic Analyzer",
      "AB 3730 Genetic Analyzer",
      "AB 3500xL Genetic Analyzer",
      "AB 3500 Genetic Analyzer",
      "AB 3130xL Genetic Analyzer",
      "AB 3130 Genetic Analyzer",
      "AB 310 Genetic Analyzer"
    ),
    "ILLUMINA" = c(
      "Illumina Genome Analyzer",
      "Illumina Genome Analyzer II",
      "Illumina Genome Analyzer IIx",
      "Illumina HiSeq 2500",
      "Illumina HiSeq 2000",
      "Illumina HiSeq 1000",
      "Illumina MiSeq",
      "Illumina HiScanSQ",
      "NextSeq 500",
      "HiSeq X Ten",
      "HiSeq X Five",
      "Illumina HiSeq 1500",
      "Illumina HiSeq 3000",
      "Illumina HiSeq 4000",
      "NextSeq 550"
    )
  ),
  library_strategy = c(
    "WGA",
    "WGS",
    "WXS",
    "RNA-Seq",
    "miRNA-Seq",
    "WCS",
    "Synthetic-Long-Read",
    "CLONE",
    "POOLCLONE",
    "AMPLICON",
    "CLONEEND",
    "FINISHING",
    "ChIP-Seq",
    "MNase-Seq",
    "DNase-Hypersensitivity",
    "Bisulfite-Seq",
    "Tn-Seq",
    "EST",
    "FL-cDNA",
    "CTS",
    "MRE-Seq",
    "MeDIP-Seq",
    "MBD-Seq",
    "OTHER"
  ),
  library_source = c(
    "GENOMIC",
    "TRANSCRIPTOMIC",
    "METAGENOMIC",
    "METATRANSCRIPTOMIC",
    "SYNTHETIC",
    "VIRAL RNA",
    "OTHER"
  ),
  library_selection = c(
    "RANDOM",
    "PCR",
    "RANDOM PCR",
    "RT-PCR",
    "HMPR",
    "MF",
    "CF-S",
    "CF-M",
    "CF-H",
    "CF-T",
    "MDA",
    "MSLL",
    "cDNA",
    "ChIP",
    "MNase",
    "DNAse",
    "Hybrid Selection",
    "Reduced Representation",
    "Restriction Digest",
    "5-methylcytidine antibody",
    "MBD2 protein methyl-CpG binding domain",
    "CAGE",
    "RACE",
    "size fractionation",
    "Padlock probes capture method",
    "other",
    "unspecified"
  ),
  library_layout = c(
    "Single",
    "Paired"
  )
)
