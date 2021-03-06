#' NCBI BioSamples
#'
#' These functions help manage BioSamples spreadsheets and individual attribute
#' fields.
#'
#' Notable functions:
#'
#' * \code{\link{build_biosamples_from_template}}: make a new spreadsheet from a
#'   template name and existing metadata
#' * \code{\link{write_biosamples}}: save a prepared biosamples spreadsheet to a
#  ' file
#'
#' @md
#'
#' @name biosamples
NULL

#' Create BioSamples Table
#'
#' Create a new BioSamples table using a named template and existing sample
#' attributes.
#'
#' @param package_name name of BioSample Package for the template to use, as
#'   either the display name (e.g., "MIGS: cultured bacteria/archaea,
#'   human-associated; version 4.0") or the short name (e.g.,
#'   "MIGS.ba.human-associated.4.0").  Call \code{\link{list_templates}} to see
#'   all the pre-installed templates by their short name.
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
#' @return data frame with BioSample attributes defined.
#' @export
build_biosamples_from_template <- function(package_name,
                                           sample_attrs,
                                           submission=NULL,
                                           col_pairs=NULL,
                                           constants=NULL) {
  # Figure out what template to use.  By default try to use the package name
  # given, and if that isn't found, try using that as a display name (the
  # long-format one shown on the web pages).  If neither works, warn.
  template_name <- package_name
  if (! package_name %in% list_templates()) {
    pkgs <- read_biosample_packages()
    template_name <- pkgs[pkgs[["DisplayName"]] == package_name, "Name"]
    if (is.null(template_name)) {
      warning(paste("Template name not recognzied:", package_name))
    }
  }
  # Read in the relevant template from disk.
  template <- read_template(template_name, "biosample_attributes")
  # Build biosamples data frame, using rows from sample attributes, columns and
  # column classes from template.
  len <- nrow(sample_attrs) * ncol(template)
  biosamples <- as.data.frame(matrix( (1:len) * NA, nrow = nrow(sample_attrs)))
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

#' Write BioSample attributes or metadata to disk
#'
#' These functions write spreadsheets to the working directory by type
#' (BioSample attributes or library metadata) with the naming scheme
#' \code{<submission>/<submission>_<type>.tsv} if submission is defined or just
#' \code{<type>.tsv} otherwise.
#'
#' @param data data frame of metadata
#' @param ... additional arguments for \code{\link{write_table}}
#'
#' @export
#' @describeIn write_biosamples Write BioSample attributes to disk
write_biosamples <- function(data, ...) {
  write_table(data, fp_suffix = "biosamples", ...)
}


# BioSample Package / Template Info ---------------------------------------

# These functions are structured in pairs of download/read steps, so we can
# keep a raw copy of the metadata inside the package and update from NCBI when
# needed.

#' Download BioSample Metadata
#'
#' Download all available BioSample package and template information and save to
#' a given directory.
#'
#' @param dp directory path to save files to.  If not specified, the extdata
#'   subdirectory of the package location will be used.
#'
#' @return named vector of downloaded file paths.
#'
#' @export
dump_metadata <- function(dp=NULL) {
  # TODO library metadata be added and this moved elsewhere?
  if (is.null(dp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
  }
  if (! dir.exists(dp)) {
    dir.create(dp, recursive = TRUE)
  }
  fps <- download_biosample_packages()
  data <- read_biosample_packages(fps)
  fps <- c(fps, sapply(data$Name, download_template))
  fps
}

#' Download BioSample Package info XML
#'
#' Download an XML file of BioSample package information and write to disk.
#'
#' @param fp file path to write to, or NULL to use a default within the package
#'   directory.
#'
#' @return file path written to
#'
#' @export
download_biosample_packages <- function(fp=NULL) {
  # (Or one at a time by giving a path after the last "/" of a package name.)
  url_xml <- paste0("https://",
                    HTTP_SRV["WWW"],
                    "/biosample/docs/packages/?format=xml")
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    fp <- file.path(dp, "biosample_packages.xml")
  }
  utils::download.file(url = url_xml, destfile = fp)
  c(biosample_packages = fp)
}

#' Read BioSample package information from file
#'
#' Read in XML file of BioSample package information (including template
#' names) and return a data frame.
#'
#' @param fp file path to write to, or NULL to use a default within the package
#'   directory.
#'
#' @return data frame of information for all available BioSample types.
#'
#' @export
read_biosample_packages <- function(fp=NULL) {
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    fp <- file.path(dp, "biosample_packages.xml")
  }
  data <- xml2::xml_children(xml2::read_xml(fp))
  columns <- c("Name", "DisplayName", "ShortName", "EnvPackage",
               "EnvPackageDisplay", "Description", "Example")
  names(columns) <- columns
  data <- lapply(columns, function(column) {
    xml2::xml_text(xml2::xml_child(data, column))
  })
  data <- do.call(cbind.data.frame, c(data, list(stringsAsFactors = FALSE)))
  data
}

#' Download BioSample Attributes XML
#'
#' Download an XML file of BioSample field attribute information and write to
#' disk.
#'
#' @param fp file path to write to, or NULL to use a default within the package
#'   directory.
#'
#' @return file path written to
#'
#' @export
download_biosample_attributes <- function(fp=NULL) {
  url_xml <- paste0("https://",
                    HTTP_SRV["WWW"],
                    "/biosample/docs/attributes/?format=xml")
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    fp <- file.path(dp, "biosample_package_attributes.xml")
  }
  utils::download.file(url = url_xml, destfile = fp)
  c(biosample_package_attributes = fp)
}

#' Read BioSample attributes list from file
#'
#' Read in an XML file of BioSample field attributes and return as nested list
#' structure.  (This doesn't fit cleanly into a table like the packages
#' information does.)
#'
#' @param fp file path to read from, or NULL to use a default within the package
#'   directory.
#'
#' @return nested list of BioSample field attributes.  Each named entry refers
#'   to a single field (column) used by one or more package templates.  Each
#'   entry contains "metadata" and "extra"; metadata stores basic information
#'   like a description of the field and what packages it's used in.  extra
#'   refers to additional per-metadata-item characteristics (currently only used
#'   for Packages).
#'
#' @export
read_biosample_attributes <- function(fp=NULL) {
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    fp <- file.path(dp, "biosample_package_attributes.xml")
  }
  data <- xml2::xml_children(xml2::read_xml(fp))
  output <- lapply(data, function(entry) {
    children <- xml2::xml_children(entry)
    entry_names <- xml2::xml_name(children)
    entry_contents <- split(xml2::xml_text(children), entry_names)
    entry_attrs <- split(xml2::xml_attrs(children), entry_names)
    list(metadata = entry_contents, extra = entry_attrs)
  })
  names(output) <- sapply(output,
                          function(x) x[["metadata"]][["HarmonizedName"]])
  output
}

#' Download BioSample Template
#'
#' Download a BioSample template as a TSV file.
#'
#' @param package_name name of BioSample Package for the template to download,
#'   (e.g., "MIMS.me.human-associated.4.0")
#' @param fp file path to write to, or NULL to use a default within the package
#'   directory.
#'
#' @return file path written to
#'
#' @export
download_template <- function(package_name, fp=NULL) {
  # read_template is currently in templates.R.  Should tidy that in one
  # direction or the other.
  args <- c(package = package_name, action = "download_tsv") # or download_excel
  url_args <- paste(names(args), args,
                    sep = "=",
                    collapse = "&")
  url <- paste0("https://",
                HTTP_SRV["SUBMIT"],
                "/biosample/template/?",
                url_args)
  if (is.null(fp)) {
    dp <- system.file("extdata", package = methods::getPackageName())
    dp_templates <- file.path(dp, "templates", "biosample_attributes")
    if (! dir.exists(dp_templates)) {
      dir.create(dp_templates, recursive = TRUE)
    }
    fp <- file.path(dp_templates, paste0(package_name, ".tsv"))
  }
  utils::download.file(url = url, destfile = fp)
  names(fp) <- package_name
  return(fp)
}

# Other -------------------------------------------------------------------


# Also useful: https://www.ncbi.nlm.nih.gov/biosample/docs/submission/validation/

#' Remove blank optional fields
#'
#' Given a data frame (with optional_fields attribute), remove any columns that
#' are listed as optional and are entirely blank.  Call this after filling in
#' any optional fields that you do have information for to clear out the
#' remaining empty ones.
#'
#' @param data data frame of metadata
#'
#' @return data frame with blank optional fields removed
#'
#' @export
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

#' Get BioSample field descriptions
#'
#' Given a vector of BioSample field (column) names, return a data frame of
#' descriptions for each.
#'
#' @param fields vector of field names, like \code{c("env_biome", "env_feature")}.
#'
#' @return data frame of field names and descriptions.
#'
#' @export
#'
#' @examples
#' fields <- c("env_biome", "env_feature")
#' descs <- field_descriptions(fields)
#' # descs is a data frame of field/descrption pairs
#' # Try View(descs)
field_descriptions <- function(fields) {
  bs_attrs <- read_biosample_attributes()
  has_name <- function(field, attrib) {
    (field == attrib[["metadata"]][["HarmonizedName"]]) ||
      (field %in% unlist(attrib[["metadata"]][["Synonym"]]))
  }

  desc <- sapply(fields, function(field) {
    for (attrib in bs_attrs) {
      if (has_name(field, attrib)) {
        d <- attrib[["metadata"]][["Description"]]
        if (is.null(d))
          d <- ""
        return(d)
      }
    }
    return("")
  })

  # Trim off whitespace including trailing line endings
  # https://stackoverflow.com/a/2261149
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  desc <- trim(desc)

  data.frame(
    Field = fields,
    Description = desc,
    stringsAsFactors = FALSE)
}
