# SRA Templates provided as TSV or Excel files during a submission for BioSample
# Attributes or Library Metadata.

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
  fp <- system.file("extdata", "templates", template_type,
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
  dp <- system.file("extdata", "templates", template_type,
                    package = methods::getPackageName())
  fps <- list.files(dp)
  gsub("\\.tsv$", "", fps)
}

#' List template types
#'
#' List types of SRA templates installed.
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
