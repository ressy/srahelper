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
  # This default has blanks for mandatory fields.  There should be a warning
  # raised and an entry in the returned vector for each one.
  biosamples <- setup_biosamples()
  expect_warning(problems <- validate_fields(biosamples))
  mf <- attributes(biosamples)$mandatory_fields
  blnks <- mf[sapply(mf, function(f) any(blank(biosamples[[f]])))]
  problems_expected <- paste("Mandatory field is missing values:", blnks)
  expect_equal(problems, problems_expected)
  # What if a mandatory column is completely missing?  Should be the same
  # result.
  biosamples$organism <- NULL
  expect_warning(problems <- validate_fields(biosamples))
  expect_equal(problems, problems_expected)
  # And if we have text in all the mandatory fields?  For now it should think
  # everything is fine, though really there should be more checks.
  biosamples <- setup_biosamples()
  biosamples <- fill_blanks(mf, biosamples)
  problems <- validate_fields(biosamples)
  expect_equal(problems, character())
})

test_that("validate_fields handles library metadata", {
  # This default has blanks for mandatory fields.  There should be a warning
  # raised and an entry in the returned vector for each one.
  sample_attrs <- setup_sra_table()
  col_pairs <- c(title = "sample_name")
  constants <- c(platform = "ILLUMINA")
  metadata <- build_metadata(sample_attrs = sample_attrs,
                             col_pairs = col_pairs,
                             constants = constants)
  expect_warning(problems <- validate_fields(metadata))
  mf <- attributes(metadata)$mandatory_fields
  blnks <- mf[sapply(mf, function(f) any(blank(metadata[[f]])))]
  blnks <- blnks[! blnks %in% c("biosample_accession", "bioproject_accession")]
  problems_expected <- paste("Mandatory field is missing values:", blnks)
  expect_equal(problems, problems_expected)
  # What if a mandatory column is completely missing?  Should be the same
  # result.
  metadata$instrument_model <- NULL
  expect_warning(problems <- validate_fields(metadata))
  expect_equal(problems, problems_expected)
  # And if we have text in all the mandatory fields?
  sample_attrs$filename <- paste0(sample_attrs$sample_name, ".fastq")
  col_pairs["library_ID"] <- "sample_name"
  constants["library_strategy"]  <- "OTHER"
  constants["library_source"]    <- "OTHER"
  constants["library_selection"] <- "RANDOM"
  constants["library_layout"]    <- "Paired"
  constants["instrument_model"]  <- "Illumina MiSeq"
  constants["filetype"]          <- "fastq"
  metadata <- build_metadata(sample_attrs = sample_attrs,
                             col_pairs = col_pairs,
                             constants = constants)
  metadata <- fill_blanks(mf, metadata)
  problems <- validate_fields(metadata)
  expect_equal(problems, character())
})

test_that("validate_fields checks filenames for directories", {
  # The processing gets confused server-side if the upload directory has any
  # sort of nested structure.
  sra_table <- setup_sra_table()
  sra_table$filename <- paste0(sra_table$sample_name, ".fastq")
  problems <- validate_fields(sra_table)
  expect_equal(problems, character())
  sra_table$filename <- file.path("folder", sra_table$filename)
  expect_warning(problems <- validate_fields(sra_table))
  problems_expected <- "Filename column contains directory paths: filename"
  expect_equal(problems, problems_expected)
})

test_that("validate_fields checks instrument model and platform", {
  # The instrument model needs to match the platform selected.
  sra_table <- setup_sra_table()
  # These match up, should be fine.
  sra_table$platform <- "ILLUMINA"
  sra_table$instrument_model <- "Illumina MiSeq"
  problems <- validate_fields(sra_table)
  expect_equal(problems, character())
  # PacBio RS is a valid instrument model but not for ILLUMINA.
  sra_table$instrument_model <- "PacBio RS"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equal(problems, "Mismatched instrument_model and platform values")
  # These are invalid values for either column.
  sra_table$instrument_model <- "does not exist"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equal(problems, "Unknown instrument_model values")
  sra_table$platform <- "does not exist"
  sra_table$instrument_model <- "PacBio RS"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equal(problems, "Unknown platform values")
})

test_that("validate_fields warns about sample uniqueness", {
  # SRA will refuse biosample metadata that it doesn't think distinguishes
  # samples well enough.  What were those rules, again?
  testthat::fail("test not yet implemented")
})

test_that("validate_fields can skip warnings", {
  sra_table <- setup_sra_table()
  sra_table$platform <- "does not exist"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equal(problems, "Unknown platform values")
  problems <- validate_fields(sra_table, quiet = TRUE)
  expect_equal(problems, "Unknown platform values")
})


# fill_blanks -------------------------------------------------------------


test_that("fill_blanks fills blank values in data frame", {
  data <- data.frame(A=c("a", "b", "", "c", NA),
                     B=c("a", "b", "", "c", NA),
                     stringsAsFactors = FALSE)
  result_expected <- data.frame(A=c("a", "b", "missing", "c", "missing"),
                                B=c("a", "b", "missing", "c", "missing"),
                                stringsAsFactors = FALSE)
  result_observed <- fill_blanks(colnames(data), data)
  expect_equal(result_observed, result_expected)
})

test_that("fill_blanks fills blank values in vector", {
  vec <- c("a", "b", "", "c", NA)
  result_expected <- c("a", "b", "missing", "c", "missing")
  result_observed <- fill_blanks(vec)
  expect_equal(result_observed, result_expected)
})

test_that("fill_blanks warns of unknown blank text", {
  vec <- c("a", "b", "", "c", NA)
  result_expected <- c("a", "b", "default", "c", "default")
  expect_warning(result_observed <- fill_blanks(vec, value = "default"))
  expect_equal(result_observed, result_expected)
})

test_that("fill_blanks ignores factors", {
  data <- data.frame(A=c("a", "b", "", "c", NA),
                     B=c("a", "b", "", "c", NA))
  result_expected <- data
  result_observed <- fill_blanks(colnames(data), data)
  expect_equal(result_observed, result_expected)
})


# blank -------------------------------------------------------------------


test_that("blank reports NA or empty string as blank", {
  result_expected <-       c(TRUE, TRUE, FALSE)
  result_observed <- blank(c("",   NA,   "text"))
  expect_equal(result_observed, result_expected)
})
