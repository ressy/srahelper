context("Test general SRA table and field functions")

# read_sra_table ----------------------------------------------------------


test_that("read_sra_table can read all template files", {
  dp_templates <- system.file("exdata", "templates", "biosample_attributes",
                              package = "srahelper")
  fps <- list.files(dp_templates, full.names = TRUE)
  mds <- lapply(fps, read_sra_table)
  # There should be N data frames
  mdclass <- sapply(mds, class)
  expect_equal(mdclass, rep("data.frame", length(fps)))
  # They all have mandatory fields defined
  mfclass <- sapply(lapply(mds, attr, "mandatory_fields"), class)
  expect_equal(mfclass, rep("character", length(fps)))
})

test_that("read_sra_table reads a Run Info table", {
  fp <- system.file("exdata", "examples", "SraRunTable.txt", package = "srahelper")
  data <- read_sra_table(fp)
  testthat::fail("test not yet implemented")
})


# write_sra_table ---------------------------------------------------------

test_that("write_sra_table writes SRA fields", {
  # Test write_sra_table alongside read_sra_table by ensuring they match.
  data <- setup_sra_table()
  fp <- tempfile()
  write_sra_table(data, fp)
  expect_true(file.exists(fp))
  # Rownames will be sample names
  rownames(data) <- data$sample_name
  # Columns are always character
  data$sample_thing1 <- as.character(data$sample_thing1)
  # The comments and mandatory fields attributes are always present from read_sra_table
  attr(data, "comments") <- character()
  attr(data, "mandatory_fields") <- character()
  data2 <- read_sra_table(fp)
  expect_identical(data2, data)
})

test_that("write_sra_table handles overwrite options", {
  # check that overwrite is off by default and works when on
  data <- setup_sra_table()
  fp <- tempfile()
  # Make sure overwriting is blocked by default
  write_sra_table(data, fp)
  expect_true(file.exists(fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  expect_error(write_sra_table(data, fp),
               paste("Destination file already exists:", fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  # Make sure overwriting works when enabled
  data$sample_name[1] <- "newname"
  write_sra_table(data, fp, overwrite = TRUE)
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "newname")
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

test_that("validate_fields warns about sample uniqueness", {
  # SRA will refuse biosample metadata that it doesn't think distinguishes
  # samples well enough.  What were those rules, again?
  testthat::fail("test not yet implemented")
})

test_that("validate_fields can skip warnings", {
  # quiet=TRUE
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
