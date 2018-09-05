context("Test Library Metadata functions")

# Package MIMS: metagenome/environmental, human-associated; version 4.0
# https://submit.ncbi.nlm.nih.gov/biosample/template/?package=MIMS.me.human-associated.4.0&action=definition
# Example sample: SAMN02808661


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


# write_metadata ----------------------------------------------------------


test_that("write_metadata writes SRA library metadata", {
  testthat::fail("test not yet implemented")
})

test_that("write_metadata writes metadata, no submission set", {
  testthat::fail("test not yet implemented")
})
