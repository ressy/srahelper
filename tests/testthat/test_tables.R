context("Test general table and field functions")


# read_table --------------------------------------------------------------


test_that("read_table can read all template files", {
  dp_templates <- system.file("extdata", "templates", "biosample_attributes",
                              package = "srahelper")
  fps <- list.files(dp_templates, full.names = TRUE)
  mds <- lapply(fps, read_table)
  # There should be N data frames
  mdclass <- sapply(mds, class)
  expect_equal(mdclass, rep("data.frame", length(fps)))
  # They all have mandatory fields defined
  mfclass <- sapply(lapply(mds, attr, "mandatory_fields"), class)
  expect_equal(mfclass, rep("character", length(fps)))
})

test_that("read_table reads a Run Info table", {
  testthat::skip("test not yet implemented")
  fp <- system.file("extdata", "examples", "SraRunTable.txt",
                    package = "srahelper")
  data <- read_table(fp)
})


# write_table -------------------------------------------------------------


check_table_written <- function(data, fp) {
  # Rownames will be sample names
  rownames(data) <- data$sample_name
  # Columns are always character
  data$sample_thing1 <- as.character(data$sample_thing1)
  # The comments and mandatory fields attributes are always present from
  # read_table
  attr(data, "comments") <- character()
  attr(data, "mandatory_fields") <- character()
  data2 <- read_table(fp)
  expect_identical(data2, data)
}

test_that("write_table writes metadata fields", {
  # Test write_table alongside read_table by ensuring they match.
  fp <- tempfile()
  data <- setup_sra_table()
  write_table(data, fp)
  expect_true(file.exists(fp))
  check_table_written(data, fp)
})

test_that("write_table handles overwrite options", {
  # check that overwrite is off by default and works when on
  fp <- tempfile()
  # Make sure overwriting is blocked by default
  data <- setup_sra_table()
  write_table(data, fp)
  expect_true(file.exists(fp))
  data2 <- read_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  expect_error(write_table(data, fp),
               paste("Destination file already exists:", fp))
  data2 <- read_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  # Make sure overwriting works when enabled
  data$sample_name[1] <- "newname"
  write_table(data, fp, overwrite = TRUE)
  data2 <- read_table(fp)
  expect_equal(data2$sample_name[1], "newname")
})

test_that("write_table autodetermines output file path", {
  fp <- tempfile()
  dir.create(fp)
  .cwd <- getwd()
  setwd(fp)
  data <- setup_sra_table()
  fp <- write_table(data)
  expect_true(file.exists(fp))
  expect_equal(fp, "./data.tsv")
  check_table_written(data, fp)
})

test_that("write_table uses file suffix given", {
  fp <- tempfile()
  dir.create(fp)
  .cwd <- getwd()
  setwd(fp)
  data <- setup_sra_table()
  fp <- write_table(data, fp_suffix = "stuff")
  expect_true(file.exists(fp))
  expect_equal(fp, "./stuff.tsv")
  check_table_written(data, fp)
})


# check_uniqueness --------------------------------------------------------

test_that("check_uniqueness marks equivalent rows", {
  data <- data.frame(sample_name = LETTERS[1:4],
                     description = letters[1:4])
  u_obs <- check_uniqueness(data)
  u_exp <- factor(rep("", 4))
  expect_equal(u_obs, u_exp)
})

test_that("check_uniqueness marks unique rows", {
  data <- setup_sra_table()
  u_obs_int <- as.integer(sort(check_uniqueness(data)))
  u_exp_int <- 1:5
  expect_equal(u_obs_int, u_exp_int)
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
  expect_equal(names(attributes(problems)), "fields")
  expect_equal(length(attributes(problems)[["fields"]]), 11)
  expect_equivalent(problems, problems_expected)
  # What if a mandatory column is completely missing?  Should be the same
  # result.
  biosamples$organism <- NULL
  expect_warning(problems <- validate_fields(biosamples))
  expect_equivalent(problems, problems_expected)
  # And if we have text in all the mandatory fields?  For now it should think
  # everything is fine, though really there should be more checks.
  biosamples <- setup_biosamples()
  biosamples <- fill_blanks(mf, biosamples)
  problems <- validate_fields(biosamples)
  expect_equivalent(problems, character())
})

test_that("validate_fields handles library metadata", {
  # This default has blanks for mandatory fields.  There should be a warning
  # raised and an entry in the returned vector for each one.
  sample_attrs <- setup_sra_table()
  col_pairs <- c(title = "sample_name", library_ID = "sample_thing1")
  constants <- c(platform = "ILLUMINA")
  metadata <- build_metadata(sample_attrs = sample_attrs,
                             col_pairs = col_pairs,
                             constants = constants)
  expect_warning(problems <- validate_fields(metadata))
  mf <- attributes(metadata)$mandatory_fields
  blnks <- mf[sapply(mf, function(f) any(blank(metadata[[f]])))]
  blnks <- blnks[! blnks %in% c("biosample_accession", "bioproject_accession")]
  problems_expected <- paste("Mandatory field is missing values:", blnks)
  expect_equivalent(problems, problems_expected)
  # What if a mandatory column is completely missing?  Should be the same
  # result.
  metadata$instrument_model <- NULL
  expect_warning(problems <- validate_fields(metadata))
  expect_equivalent(problems, problems_expected)
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
  expect_equivalent(problems, character())
})

test_that("validate_fields checks filenames for directories", {
  # The processing gets confused server-side if the upload directory has any
  # sort of nested structure.
  sra_table <- setup_sra_table()
  sra_table$filename <- paste0(sra_table$sample_name, ".fastq")
  problems <- validate_fields(sra_table)
  expect_equivalent(problems, character())
  sra_table$filename <- file.path("folder", sra_table$filename)
  expect_warning(problems <- validate_fields(sra_table))
  problems_expected <- "Filename column contains directory paths: filename"
  expect_equivalent(problems, problems_expected)
})

test_that("validate_fields checks instrument model and platform", {
  # The instrument model needs to match the platform selected.
  sra_table <- setup_sra_table()
  # These match up, should be fine.
  sra_table$platform <- "ILLUMINA"
  sra_table$instrument_model <- "Illumina MiSeq"
  problems <- validate_fields(sra_table)
  expect_equivalent(problems, character())
  # PacBio RS is a valid instrument model but not for ILLUMINA.
  sra_table$instrument_model <- "PacBio RS"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equivalent(problems, "Mismatched instrument_model and platform values")
  # These are invalid values for either column.
  sra_table$instrument_model <- "does not exist"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equivalent(problems, "Unknown instrument_model values")
  sra_table$platform <- "does not exist"
  sra_table$instrument_model <- "PacBio RS"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equivalent(problems, "Unknown platform values")
})

test_that("validate_fields warns about sample uniqueness", {
  # SRA will refuse biosample metadata that it doesn't think distinguishes
  # samples well enough.
  biosamples <- setup_biosamples()
  mf <- attributes(biosamples)$mandatory_fields
  biosamples <- setup_biosamples()
  biosamples <- fill_blanks(mf, biosamples)
  biosamples$host <- 1
  expect_warning(problems <- validate_fields(biosamples))
  expect_equivalent(problems,
                    paste("Multiple entries cannot have",
                          "identical attributes (see ?check_uniqueness)"))
})

test_that("validate_fields can skip warnings", {
  sra_table <- setup_sra_table()
  sra_table$platform <- "does not exist"
  expect_warning(problems <- validate_fields(sra_table))
  expect_equivalent(problems, "Unknown platform values")
  problems <- validate_fields(sra_table, quiet = TRUE)
  expect_equivalent(problems, "Unknown platform values")
})


# fill_blanks -------------------------------------------------------------


test_that("fill_blanks fills blank values in data frame", {
  data <- data.frame(A = c("a", "b", "", "c", NA),
                     B = c("a", "b", "", "c", NA),
                     stringsAsFactors = FALSE)
  result_expected <- data.frame(A = c("a", "b", "missing", "c", "missing"),
                                B = c("a", "b", "missing", "c", "missing"),
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
  data <- data.frame(A = c("a", "b", "", "c", NA),
                     B = c("a", "b", "", "c", NA))
  result_expected <- data
  result_observed <- fill_blanks(colnames(data), data)
  expect_equal(result_observed, result_expected)
})


# datemunge ---------------------------------------------------------------


test_that("datemunge formats dates", {
  dates     <- c("1/1/2006",   "10/5/2009",  "12/31/2012")
  dates_exp <- c("2006-01-01", "2009-10-05", "2012-12-31")
  dates_obs <- datemunge(dates)
  expect_equal(dates_obs, dates_exp)
})

test_that("datemunge accepts arbitrary input field ordering", {
  # As above, but, D/M/Y
  dates     <- c("1/1/2006",   "5/10/2009",  "31/12/2012")
  dates_exp <- c("2006-01-01", "2009-10-05", "2012-12-31")
  dates_obs <- datemunge(dates, ord_input = c("D", "M", "Y"))
  expect_equal(dates_obs, dates_exp)
})

test_that("datemunge rejects invalid input", {
  dates     <- c("1/1/2006",   "10/5/2009", "not a date")
  expect_error(datemunge(dates))
  dates[3] <- "also/not/date"
  expect_error(datemunge(dates))
  # Oops, we forgot to specify the field order
  dates     <- c("1/1/2006",   "5/10/2009",  "31/12/2012")
  expect_error(datemunge(dates))
})


# blank -------------------------------------------------------------------


test_that("blank reports NA or empty string as blank", {
  result_expected <-       c(TRUE, TRUE, FALSE)
  result_observed <- blank(c("",   NA,   "text"))
  expect_equal(result_observed, result_expected)
})
