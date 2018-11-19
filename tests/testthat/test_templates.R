context("Test SRA template functions")


# list_templates ----------------------------------------------------------


test_that("list_templates lists templates installed", {
  templates <- list_templates()
  templates2 <- list_templates("biosample_attributes")
  expect_equal(length(templates), 153)
  expect_true("MIGS.ba.human-associated.4.0" %in% templates)
  expect_identical(templates, templates2)
})

test_that("list_templates lists metadata templates", {
  templates <- list_templates("library_metadata")
  expect_identical(templates, "SRA_metadata_acc")
})

test_that("list_templates warns for unknown template type", {
  templates <- expect_warning(list_templates("something_else"),
                              "Template type not recognized: something_else")
  expect_equal(templates, character())
})


# list_template_types -----------------------------------------------------


test_that("list_template_types lists template types installed", {
  types <- list_template_types()
  expect_identical(types, c("biosample_attributes", "library_metadata"))
})


# read_template -----------------------------------------------------------


test_that("read_template can read all template files", {
  # see read_sra_table above as well
  for (template_name in list_templates()) {
    # They all have mandatory fields, optional fields, and comments
    template <- read_template(template_name)
    attrs <- c("mandatory_fields", "optional_fields", "comments")
    expect_true(all(attrs %in% names(attributes(template))))
  }
})

test_that("read_template handles unknown template", {
  template_name <- "non.existent.template"
  expect_error(
    expect_warning(
      read_template(template_name),
      paste("Template name not recognized:", template_name))
  )
})
