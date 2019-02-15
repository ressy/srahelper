#' NCBI Submission Templates
#'
#' Some common concepts apply for multiple submission template types (e.g. SRA,
#' BioSample, GenBank) like required and optional fields and fixed-vocabulary or
#' otherwise restricted-value fields.  A given submission type (e.g. BioSample)
#' can have a wide range of possible templates (Human, Microbe, Virus, etc.).
#' Template-related functions here help manage listing and loading these
#' templates.  These are used by more specific helpers like
#' \code{\link{build_biosamples_from_template}} from the \link{biosamples}
#' functions.
#'
#' * \code{\link{list_template_types}}: List the types (submission categories)
#'   that templates are defined for.
#' * \code{\link{list_templates}}: List templates within a category.
#' * \code{\link{read_template}}: Load an installed template as a data frame.
#'
#' @md
#'
#' @name templates
NULL

#' Read table template
#'
#' load a template by name (e.g., "MIMS.me.human-associated.4.0" for BioSamples)
#' and return as a data frame.  Currently supports BioSample and SRA Metadata
#' template types.
#'
#' @param template_name name of submission type (e.g.,
#'   "MIMS.me.human-associated.4.0")
#' @param template_type either "biosample_attributes" or "library_metadata".
#'
#' @return data frame with field names and types set by template.
#' @export
read_template <- function(template_name,
                          template_type="biosample_attributes") {
  if (! template_type %in% list_template_types()) {
    warning(paste("Template type not recognized:", template_type))
  } else if (! template_name %in% list_templates(template_type)) {
    warning(paste("Template name not recognzied:", template_name))
  }
  fp <- system.file("extdata", "templates", template_type,
                    paste(template_name, "tsv", sep = "."),
                    package = methods::getPackageName())
  data <- read_table(fp)
  mf <- attributes(data)$mandatory_fields
  of <- colnames(data)[! colnames(data) %in% mf]
  attr(data, "optional_fields") <- of
  data
}

#' List installed template files
#'
#' List names of templates installed in the package.  These are the spreadsheets
#' the submission portal web interface gives you during a submission at the
#' attributes and metadata steps.
#'
#' @param template_type either "biosample_attributes" or "library_metadata".
#'
#' @return vector of template names, without file extension.
#' @export
list_templates <- function(template_type="biosample_attributes") {
  if (! template_type %in% list_template_types()) {
    warning(paste("Template type not recognized:", template_type))
  }
  dp <- system.file("extdata", "templates", template_type,
                    package = methods::getPackageName())
  fps <- list.files(dp)
  gsub("\\.tsv$", "", fps)
}

#' List template types
#'
#' List types of NCBI submission templates installed.
#'
#' @return vector of template type names.
#' @export
list_template_types <- function() {
  dp <- system.file("extdata", "templates",
                    package = methods::getPackageName())
  fps <- list.files(dp, full.names = TRUE)
  fps <- basename(fps[sapply(fps, dir.exists)])
  fps
}
