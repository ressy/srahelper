context("Test metadata utilities")

# Package MIMS: metagenome/environmental, human-associated; version 4.0
# https://submit.ncbi.nlm.nih.gov/biosample/template/?package=MIMS.me.human-associated.4.0&action=definition
# Example sample: SAMN02808661


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


setup_sra_table <- function() {
  data.frame(sample_name = paste0("sample", 1:5),
             sample_thing1 = (1:5)*5,
             sample_thing2 = letters[1:5],
             stringsAsFactors = FALSE)
}

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


# write_biosamples --------------------------------------------------------

setup_biosamples <- function(submission = "SUB") {
  sample_attrs <- setup_sra_table()
  biosamples <- build_biosamples_from_template("MIGS.ba.human-associated.4.0",
                                               sample_attrs = sample_attrs,
                                               submission = submission)
  biosamples <- tidy_optional_fields(biosamples)
  biosamples
}

test_that("write_biosamples writes SRA biosample attributes", {
  biosamples <- setup_biosamples()
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  fp <- file.path(submission, paste0(submission, "_biosamples.tsv"))
  expect_true(file.exists(fp))
})

test_that("write_biosamples writes attributes, no submission set", {
  biosamples <- setup_biosamples(NULL)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  fp <- "biosamples.tsv"
  expect_true(file.exists(fp))
})

test_that("write_biosamples handles overwrite options", {
  # This functionality comes from write_sra_table and should be usable from here
  # too.
  biosamples <- setup_biosamples()
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  fp <- file.path(submission, paste0(submission, "_biosamples.tsv"))
  expect_true(file.exists(fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  expect_error(write_biosamples(biosamples),
               paste("Destination file already exists:", fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  biosamples$sample_name[1] <- "newname"
  write_biosamples(biosamples, overwrite = TRUE)
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "newname")
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


# list_templates ----------------------------------------------------------

test_that("list_templates lists templates installed", {
  templates <- list_templates()
  templates2 <- list_templates("biosample_attributes")
  expect_equal(length(templates), 4)
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
