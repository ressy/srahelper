context("Test metadata utilities")

# Package MIMS: metagenome/environmental, human-associated; version 4.0
# https://submit.ncbi.nlm.nih.gov/biosample/template/?package=MIMS.me.human-associated.4.0&action=definition
# Example sample: SAMN02808661

test_that("read_sra_table reads all template files", {
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


test_that("write_sra_table warns about sample uniqueness", {
  # SRA will refuse biosample metadata that it doesn't think distinguishes
  # samples well enough.  What were those rules, again?
  testthat::fail("test not yet implemented")
})
