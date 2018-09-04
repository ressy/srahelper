context("Test metadata utilities")

# Package MIMS: metagenome/environmental, human-associated; version 4.0
# https://submit.ncbi.nlm.nih.gov/biosample/template/?package=MIMS.me.human-associated.4.0&action=definition
# Example sample: SAMN02808661


# read_sra_table ----------------------------------------------------------


test_that("read_sra_table can read all template files", {
  dp_templates <- system.file("exdata", "templates", package = "srahelper")
  fps <- list.files(dp_templates, full.names = TRUE)
  mds <- lapply(fps, read_sra_table)
  testthat::fail("test not yet implemented")
})

test_that("read_sra_table reads a Run Info table", {
  fp <- system.file("exdata", "examples", "SraRunTable.txt", package = "srahelper")
  data <- read_sra_table(fp)
  testthat::fail("test not yet implemented")
})

test_that("make_sra_metadata creates new data frame", {
  testthat::fail("test not yet implemented")
})


# write_sra_table ---------------------------------------------------------


test_that("write_sra_table writes SRA fields", {
  testthat::fail("test not yet implemented")
})

test_that("write_sra_table warns about sample uniqueness", {
  # SRA will refuse biosample metadata that it doesn't think distinguishes
  # samples well enough.  What were those rules, again?
  testthat::fail("test not yet implemented")
})

test_that("write_sra_table handles overwrite options", {
  # check that overwrite is off by default and works when on
  testthat::fail("test not yet implemented")
})

# write_biosamples --------------------------------------------------------


test_that("write_biosamples writes SRA biosample attributes", {
  testthat::fail("test not yet implemented")
})

test_that("write_biosamples writes attributes, no submission set", {
  testthat::fail("test not yet implemented")
})


# write_metadata ----------------------------------------------------------


test_that("write_metadata writes SRA library metadata", {
  testthat::fail("test not yet implemented")
})

test_that("write_metadata writes metadata, no submission set", {
  testthat::fail("test not yet implemented")
})


# build_biosamples --------------------------------------------------------


test_that("build_biosamples_from_template makes attributes data frame", {
  testthat::fail("test not yet implemented")
  # test attributes: submission, template_name, mandatory_fields
  # test columns from implicit match, explicit with col_pairs, and constants
  # test rows
})

test_that("build_biosamples_from_template supports col_pairs", {
  # make sure it pulls explicitly-named columns as expected
  # make sure that extra column names are included (so custom columns can be
  # added)
  testthat::fail("test not yet implemented")
})

test_that("build_biosamples_from_template handles unknown template", {
  testthat::fail("test not yet implemented")
})


# read_template -----------------------------------------------------------


test_that("read_template can read all template files", {
  # see read_sra_table above as well
  testthat::fail("test not yet implemented")
})

test_that("read_template handles unknown template", {
  testthat::fail("test not yet implemented")
})


# validate_fields ---------------------------------------------------------


test_that("validate_fields handles biosample attributes", {
  # see read_sra_table above as well
  testthat::fail("test not yet implemented")
})

test_that("validate_fields handles library metadata", {
  testthat::fail("test not yet implemented")
})

test_that("validate_fields handles generic data frame", {
  # in this one, no special attributes, just a data frame.  columns checked by
  # name only.
  testthat::fail("test not yet implemented")
})

test_that("validate_fields checks mandatory fields", {
  # also note special handling for biosample_accession / bioproject_accession
  testthat::fail("test not yet implemented")
})

test_that("validate_fields checks filenames for directories", {
  # The processing gets confused server-side if the upload directory has any
  # sort of nested structure.
  testthat::fail("test not yet implemented")
})

test_that("validate_fields checks instrument model and platform", {
  # The instrument model needs to match the platform selected.
  testthat::fail("test not yet implemented")
})

test_that("validate_fields can skip warnings", {
  # quiet=TRUE
  testthat::fail("test not yet implemented")
})


# build_metadata ----------------------------------------------------------


test_that("build_metadata makes metadata data frame", {
  testthat::fail("test not yet implemented")
  # test attributes: submission, template_name, mandatory_fields
  # test columns from implicit match, explicit with col_pairs, and constants
  # test rows
})

test_that("build_metadata supports col_pairs", {
  # make sure it pulls explicitly-named columns as expected
  # make sure that extra column names are included (so custom columns can be
  # added)
  testthat::fail("test not yet implemented")
})


# fill_blanks -------------------------------------------------------------


test_that("fill_blanks fills blank values in data frame", {
  testthat::fail("test not yet implemented")
})

test_that("fill_blanks fills blank values in vector", {
  testthat::fail("test not yet implemented")
})

test_that("fill_blanks warns of unknown blank text", {
  testthat::fail("test not yet implemented")
})


# blank -------------------------------------------------------------------


test_that("blank reports NA or empty string as blank", {
  testthat::fail("test not yet implemented")
})
