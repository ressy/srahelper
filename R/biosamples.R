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
    biosamples[[j]] <- methods::as(biosamples[[j]],
                                   class(template[[j]]))
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


# BioSample Package / Template Info ---------------------------------------


#' Download SRA BioSample Metadata
#'
#' Download all available BioSample package and template information and save to
#' a given directory.
#'
#' @param dp directory path to save files to.  If not specified, the extdata
#'   subdirectory of the package location will be used.
#'
#' @return data frame of information for all available BioSample types.
#'
#' @export
dump_metadata <- function(dp=NULL) {
  if (is.null(dp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
  }
  if (! dir.exists(dp)) {
    dir.create(dp, recursive = TRUE)
  }
  # Download BioSample Package data
  biosample_packages <- download_biosample_packages()
  write_sra_table(biosample_packages,
                  fp = file.path(dp, "biosample_packages.tsv"))
  dp_templates <- file.path(dp, "templates", "biosample_attributes")
  if (! dir.exists(dp_templates)) {
    dir.create(dp_templates, recursive = TRUE)
  }
  # Download BioSample Template files
  templates <- lapply(biosample_packages$Name, download_template)
  names(templates) <- biosample_packages$Name
  for (tn in biosample_packages$Name) {
    fp <- file.path(dp_templates, paste0(tn, ".tsv"))
    writeChar(templates[[tn]], fp, eos=NULL)
  }
}


#' Download BioSample Package Information
#'
#' Download an XML file of BioSample Package information (including template
#' names) and return a data frame.
#'
#' @return data frame of information for all available BioSample types.
#'
#' @export
download_biosample_packages <- function() {
  url_xml <- paste0("https://",
                    HTTP_SRV["WWW"],
                    "/biosample/docs/packages/?format=xml")
  x <- xml2::xml_children(xml2::read_xml(url_xml))
  columns <- c("Name", "DisplayName", "ShortName", "EnvPackage",
               "EnvPackageDisplay", "Description", "Example")
  names(columns) <- columns
  data <- lapply(columns, function(column) xml2::xml_text(xml2::xml_child(x, column)))
  data <- do.call(cbind.data.frame, c(data,list(stringsAsFactors = FALSE)))
  data
}

#' Download BioSample Template
#'
#' Download a BioSample template as a TSV file and return the text.
#'
#' @return text of TSV template
#'
#' @export
download_template <- function(package_name) {
  args <- c(package=package_name, action="download_tsv") # or download_excel
  url_args <- paste(names(args), args,
                    sep = "=",
                    collapse = "&")
  url <- paste0("https://", HTTP_SRV["SUBMIT"], "/biosample/template/?", url_args)
  con <- file(url, "r")
  txt <- ""
  N <- 1024*1024
  chunk <- readChar(con, N)
  while (length(chunk) > 0) {
    txt <- paste(txt, chunk)
    chunk <- readChar(con, N)
  }
  close(con)
  # Remove leading space
  txt <- sub("^ +", "", txt)
  txt
}
